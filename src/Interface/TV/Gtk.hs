{-# LANGUAGE RecursiveDo, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Gtk
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gtk-based GUIs in the TV (tangible value) framework
----------------------------------------------------------------------

module Interface.TV.Gtk where

import Control.Applicative (liftA2,(<$>))
import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime,utctDayTime)

import Data.Title
import Data.Pair
import Data.Lambda

import Interface.TV.Input
import Interface.TV.Output

import Graphics.UI.Gtk -- as Gtk

type In  = Input  MkI
type Out = Output MkI MkO

  -- IPrim :: src a -> Input src a
  -- OPrim :: snk a -> Output src snk a


-- Make a input UI.
newtype MkI  a = MkI { unMkI :: MkI' a }

-- Representation type for 'MkI'.  Takes a change call-back and produces a widget and a
-- polling operation and a clean-up action.
type MkI' a = IO () -> IO (Widget, IO a, IO ())

-- Make an output UI.
newtype MkO a = MkO { unMkO :: MkO' a }

-- Representation type for 'MkO'.  Give a widget and a way to send it new
-- info to disply and a clean-up action.
type MkO' a = IO (Widget, OI a, IO ())

-- | Sink of information
type OI a = a -> IO ()

-- | Add post-processing
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)


runOut :: Out a -> String -> a -> IO ()
runOut out name a = do
  initGUI
  (wid,sink,cleanup) <- unMkO (output out)
  sink a
  window <- windowNew
  set window [ windowDefaultWidth   := 200 -- , windowDefaultHeight := 200
             -- , containerBorderWidth := 10
             , containerChild       := wid
             , windowFocusOnMap     := True       -- helpful?
             , windowTitle          := name
             ]
  onDestroy window (cleanup >> mainQuit)
  widgetShowAll window
  mainGUI
  return ()


data Orient = Horizontal | Vertical deriving (Read,Show)

boxNew :: Orient -> Bool -> Int -> IO Box
boxNew Vertical   = boxer vBoxNew
boxNew Horizontal = boxer hBoxNew

boxer :: BoxClass box => (a -> b -> IO box) -> (a -> b -> IO Box)
boxer = (result.result.fmap) toBox

instance Pair MkI where
  pair (MkI ia) (MkI ob) = MkI $ \ refresh ->
    do box <- boxNew Horizontal True 10
       (wa,geta,cleana) <- ia refresh
       (wb,getb,cleanb) <- ob refresh
       set box [ containerChild := wa , containerChild := wb ]
       return (toWidget box, liftA2 (,) geta getb, cleana >> cleanb)

instance Pair MkO where
  pair (MkO oa) (MkO ob) = MkO $
    do box <- boxNew Horizontal True 10
       (wa,snka,cleana) <- oa
       (wb,snkb,cleanb) <- ob
       set box [ containerChild := wa , containerChild := wb ]
       return (toWidget box, \ (a,b) -> snka a >> snkb b, cleana >> cleanb)

instance Title_f MkI where
  title_f str (MkI ia) = MkI $ \ refresh ->
    do (widget,geta,cleana) <- ia refresh
       frame  <- frameNew
       set frame [ frameLabel      := str
                 -- , frameShadowType := ShadowEtchedOut
                 , containerChild  := widget ]
       return (toWidget frame, geta, cleana)

instance Title_f MkO where
  title_f str (MkO oa) = MkO $
   do (widget,sink,clean) <- oa
      frame  <- frameNew
      set frame [ frameLabel      := str
                -- , frameShadowType := ShadowEtchedOut
                , containerChild  := widget ]
      return (toWidget frame, sink, clean)

instance Lambda MkI MkO where
  lambda (MkI ia) (MkO ob) = MkO $
    mdo box  <- boxNew Vertical True 10
        reff <- newIORef (error "mkLambda: no function yet")
        let update = do f <- readIORef reff
                        a <- geta   -- note loop
                        snkb (f a)
        (wa,geta,cleana) <- ia update
        (wb,snkb,cleanb) <- ob
        set box [ containerChild := wa , containerChild := wb ]
        return ( toWidget box
               , \ f -> writeIORef reff f >> update
               , cleana >> cleanb)


primMkI :: MkI' a -> In a
primMkI = iPrim . MkI

primMkO :: MkO' a -> Out a
primMkO = oPrim . MkO

type R = Float

-- TODO: Consider using R == Double (for constant folding), while really
-- being float on the GLSL side.

sliderRI :: (R,R) -> R -> In R
sliderRI = sliderGIn realToFrac realToFrac 0.01 5

sliderII :: (Int,Int) -> Int -> In Int
sliderII = sliderGIn fromIntegral round 1 0

-- Generalized slider.  Gtk's scaling widgets work with Double, so this
-- adapter takes some initial params for conversion.  Only fires when a
-- value really changes.
sliderGIn :: Eq a => (a -> Double) -> (Double -> a) -> a -> Int
            -> (a,a) -> a -> In a
sliderGIn toD fromD step digits
             (lo,hi) a0 = primMkI $ \ refresh ->
  let changeTo getter new =
        do old <- getter
           -- putStrLn $ "(old,new) ==" ++ show (old,new)
           when (old /= new) refresh
  in
      do w <- hScaleNewWithRange (toD lo) (toD hi) (toD step)
         set w [ rangeValue := toD a0, scaleDigits := digits ]
         -- Unlike wxHaskell, I guess call-backs aren't attributes in gtk2hs.
         let getter = fromD <$> get w rangeValue
         onRangeChangeValue w (\ _ x -> changeTo getter (fromD x) >> return False)
         -- TODO: experiment with return False vs True
         return (toWidget w, getter, return ())


toggleI :: Bool -> In Bool
toggleI start = primMkI $ \ refresh ->
  do w <- checkButtonNew
     toggleButtonSetActive w start
     onToggled w refresh
     return (toWidget w, toggleButtonGetActive w, return ())

toggleO :: Out Bool
toggleO = primMkO $
  do w <- checkButtonNew
     return (toWidget w, toggleButtonSetActive w, return ())

mkFileName :: FilePath -> In FilePath
mkFileName start = primMkI $ \ refresh ->
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     fileChooserSetFilename w start
     onCurrentFolderChanged w refresh
     -- fileChooserGetFilename w >>= print    -- testing
     return ( toWidget w
            , fromMaybe start <$> fileChooserGetFilename w
            , return () )

-- mkTextureI :: GlTexture -> In GlTexture
-- mkTextureI = error "mkTexture: not yet implemented"

-- mkTexture start refresh (BaseG oi) = do ...
-- mkTexture _ _ _ = error "mkTexture: not BaseG"

-- onEntryActivate :: EntryClass ec => ec -> IO () -> IO (ConnectId ec)

textI :: String -> In String
textI start = primMkI $ \ refresh ->
  do entry <- entryNew
     entrySetText entry start
     onEntryActivate entry refresh
     return (toWidget entry, entryGetText entry, return ())


textO :: Out String
textO = primMkO $
  do entry <- entryNew
     return (toWidget entry, entrySetText entry, return ())

-- textO = primMkO $
--         do lab <- labelNew Nothing
--            return (toWidget lab, labelSetText lab)

-- | A clock that reports time in seconds and updates at the given period
-- (in seconds).
clockDtI :: R -> In R
clockDtI period = primMkI $ \ refresh ->
  do start   <- time
     timeout <- timeoutAddFull (refresh >> return True)
                  priorityDefaultIdle (round (period * 1000))
     w <- vBoxNew True 0    -- size 0 box
     return (toWidget w, subtract start <$> time, timeoutRemove timeout)


-- Deactivating the clock's timeout during clean-up prevents it from
-- running when gtk starts up again.  Particularly useful in ghci, where
-- restarting gtk is commonplace.


-- | A clock that updates every 1/60 second
clockI :: In R
clockI = clockDtI (1/60)

-- Get the time since midnight, in seconds
time :: IO R
time = (fromRational . toRational . utctDayTime) <$> getCurrentTime


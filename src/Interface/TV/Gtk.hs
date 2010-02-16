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

module Interface.TV.Gtk
  ( -- * TV type specializations
    In, Out, GTV, gtv, runGTV
    -- * UI primitives
  , R, sliderRIn, sliderIIn, clockIn, fileNameIn
  , fooGL
  , module Interface.TV
  ) where

import Control.Applicative (liftA2,(<$>))
import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime,utctDayTime)

import Data.Title
import Data.Pair
import Data.Lambda

import qualified Control.Compose as C
import Control.Compose (ToOI(..),Cofunctor(..))

import Interface.TV

-- import Interface.TV.Input
-- import Interface.TV.Output
-- import Interface.TV.Tangible
-- import Interface.TV.Common

import Graphics.UI.Gtk hiding (Action)

import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import qualified Graphics.UI.Gtk as Gtk
-- TODO: Try dropping the 'qualified'
import Graphics.Rendering.OpenGL as GL hiding (Sink,get)

{--------------------------------------------------------------------
    TV type specializations
--------------------------------------------------------------------}

type In  = Input  MkI
type Out = Output MkI MkO
type GTV = TV MkI MkO

-- Type specialization of 'tv'
gtv :: Out a -> a -> GTV a
gtv = tv

-- Type specialization of 'runTV'
runGTV :: GTV a -> IO ()
runGTV = runTV


{--------------------------------------------------------------------
    Actions & info sinks
--------------------------------------------------------------------}

-- | Convenient shorthand
type Action = IO ()

-- | Sink of information
type Sink a = a -> Action

infixl 1 >+>  -- first guess
-- | Combine sinks
(>+>) :: Sink a -> Sink b -> Sink (a,b)
(snka >+> snkb) (a,b) = snka a >> snkb b


{--------------------------------------------------------------------
    Representations
--------------------------------------------------------------------}

-- Make a input UI.
newtype MkI  a = MkI (MkI' a)

-- Representation type for 'MkI'.  Takes a change call-back and produces a widget and a
-- polling operation and a clean-up action.
type MkI' a = Action -> IO (Widget, IO a, Action)

-- Make an output UI.
newtype MkO a = MkO (MkO' a)

-- Representation type for 'MkO'.  Give a widget and a way to send it new
-- info to display and a clean-up action.
type MkO' a = IO (Widget, Sink a, Action)

-- Currently, the clean-up actions are created only by clockDtI, and just
-- propagated by the other combinators.

instance Functor MkI where
  fmap f (MkI h) = MkI h'
    where
      h' refresh = do (wid,poll,clean) <- h refresh
                      return (wid, fmap f poll, clean)

instance Cofunctor MkO where
  cofmap f (MkO io) = MkO io'
   where
     io' = do (wid,sink,cleanup) <- io
              return (wid,sink . f,cleanup)

-- Note that Functor & Cofunctor are isomorphic to a standard form.
-- Consider redefining MkI' and MkO' accordingly.  See how other instances
-- work out.

instance CommonIns MkI where
  getString start = MkI $ \ refresh ->
    do entry <- entryNew
       entrySetText entry start
       onEntryActivate entry refresh
       return (toWidget entry, entryGetText entry, return ())
  getRead = getReadF  -- thanks to MkI Functor
  getBool start = MkI $ \ refresh ->
    do w <- checkButtonNew
       toggleButtonSetActive w start
       onToggled w refresh
       return (toWidget w, toggleButtonGetActive w, return ())

-- TODO: refactor textI, toggleI.  Or eliminate them, and just use
-- stringIn, boolIn in their place.

instance CommonOuts MkO where
  putString = MkO $
    do entry <- entryNew
       return (toWidget entry, entrySetText entry, return ())
  putShow = putShowC  -- thanks to MkO Cofunctor
  putBool = MkO $
    do w <- checkButtonNew
       return (toWidget w, toggleButtonSetActive w, return ())


-- | Add post-processing.  (Could use 'fmap' instead, but 'result' is more
-- specifically typed.)
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)

-- runOut :: String -> Out a -> a -> Action
-- runOut name out a = runMkO name (output out) a

runMkO :: String -> MkO a -> Sink a
runMkO name (MkO mko') a = do
  initGUI
  GtkGL.initGL -- okay to multi-init?
  -- putStrLn "past initGL"
  (wid,sink,cleanup) <- mko'
  sink a
  window <- windowNew
  set window [ windowDefaultWidth   := 200
          -- , windowDefaultHeight  := 200
          -- , containerBorderWidth := 10
             , containerChild       := wid
          -- , windowFocusOnMap     := True       -- helpful?
             , windowTitle          := name
             ]
  onDestroy window (cleanup >> mainQuit)
  widgetShowAll window
  -- Glew.glewInit                         --TODO: try moving up.
  mainGUI
  return ()

instance ToOI MkO where
  toOI mkO = C.Flip (runMkO "GtkTV" mkO)


{--------------------------------------------------------------------
    UI primitives
--------------------------------------------------------------------}


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
       return (toWidget box, snka >+> snkb, cleana >> cleanb)

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

-- Currently unused

-- primMkO :: MkO' a -> Out a
-- primMkO = oPrim . MkO

type R = Float

-- TODO: Consider using R == Double (for constant folding), while really
-- being float on the GLSL side.

sliderRIn :: (R,R) -> R -> In R
sliderRIn = sliderGIn realToFrac realToFrac 0.01 5

sliderIIn :: (Int,Int) -> Int -> In Int
sliderIIn = sliderGIn fromIntegral round 1 0

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

fileNameIn :: FilePath -> In FilePath
fileNameIn start = primMkI $ \ refresh ->
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

-- onEntryActivate :: EntryClass ec => ec -> Action -> IO (ConnectId ec)

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
clockIn :: In R
clockIn = clockDtI (1/60)

-- Get the time since midnight, in seconds
time :: IO R
time = (fromRational . toRational . utctDayTime) <$> getCurrentTime


{--------------------------------------------------------------------
    GtkGL stuff
--------------------------------------------------------------------}

fooGL :: IO (Action -> Action)
fooGL =
  do glconfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA,
                                    GtkGL.GLModeDepth,
                                    GtkGL.GLModeDouble,
                                    GtkGL.GLModeAlpha ]
     canvas <- GtkGL.glDrawingAreaNew glconfig
     -- putStrLn "made canvas"
     Gtk.widgetSetSizeRequest canvas 600 600
     -- Initialise some GL setting just before the canvas first gets shown
     -- (We can't initialise these things earlier since the GL resources that
     -- we are using wouldn't heve been setup yet)
     -- TODO experiment with moving some of these steps.
     Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ ->
       do -- setupMatrices  -- do elsewhere, e.g., runSurface
          depthFunc  $= Just Less
          drawBuffer $= BackBuffers
          clearColor $= Color4 0 0 0.2 1
          -- experimental {
          -- blendFunc $= (SrcAlpha, One)
          -- blend     $= Enabled
          -- GL.cullFace $= Just Back
          -- } experimental
          -- putStrLn "glEnableVSync"
          -- glEnableVSync True
     -- Sync canvas size with GL viewport
     Gtk.onExpose canvas $ \_ -> 
       do (w',h') <- Gtk.widgetGetSize canvas
          let w = fromIntegral w' ; h = fromIntegral h'
          -- viewport $= ((Position 0 0), (Size (fromIntegral w) (fromIntegral h)))
          let dim :: GLsizei; start :: GLsizei -> GLint
              dim = w `min` h ; start s = fromIntegral ((s - dim) `div` 2)
          viewport $= ((Position (start w) (start h)), (Size dim dim))
          return True
     return $ display canvas

display :: GtkGL.GLDrawingArea -> Sink Action
display canvas render =
  do GtkGL.withGLDrawingArea canvas $ \glwindow ->
       do GL.clear [GL.DepthBuffer, GL.ColorBuffer]
          render
          -- glWaitVSync
          finish
          GtkGL.glDrawableSwapBuffers glwindow
     return ()

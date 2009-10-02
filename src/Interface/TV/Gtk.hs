{-# LANGUAGE RecursiveDo, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Gtk
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
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


-- Make a input UI.  Takes a change call-back and produces a widget and a
-- polling operation.
newtype MkI  a = MkI { unMkI :: MkI' a }

-- Representation type for 'MkI'
type MkI' a = IO () -> IO (Widget, IO a)

-- Make an output UI.  Give a widget and a way to send it new info to disply.
newtype MkO a = MkO { unMkO :: MkO' a }

-- Representation type for 'MkO'
type MkO' a = IO (Widget, OI a)

-- | Sink of information
type OI a = a -> IO ()


-- | Add post-processing
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)


makeUI :: Out a -> String -> OI a
makeUI out name a = do
  initGUI
  (wid, sink) <- unMkO (output out)
  sink a
  window <- windowNew
  set window [ windowDefaultWidth   := 200 -- , windowDefaultHeight := 200
             -- , containerBorderWidth := 10
             , containerChild       := wid
             , windowFocusOnMap     := True       -- helpful?
             , windowTitle          := name
             ]
  onDestroy window mainQuit
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
       (wa,geta) <- ia refresh
       (wb,getb) <- ob refresh
       set box [ containerChild := wa , containerChild := wb ]
       return (toWidget box, liftA2 (,) geta getb)

instance Pair MkO where
  pair (MkO oa) (MkO ob) = MkO $
    do box <- boxNew Horizontal True 10
       (wa,snka) <- oa
       (wb,snkb) <- ob
       set box [ containerChild := wa , containerChild := wb ]
       return (toWidget box, \ (a,b) -> snka a >> snkb b)

instance Title_f MkI where
  title_f str (MkI ia) = MkI $ \ refresh ->
    do (widget,geta) <- ia refresh
       frame  <- frameNew
       set frame [ frameLabel      := str
                 -- , frameShadowType := ShadowEtchedOut
                 , containerChild  := widget ]
       return (toWidget frame, geta)

instance Title_f MkO where
  title_f str (MkO oa) = MkO $
   do (widget,sink) <- oa
      frame  <- frameNew
      set frame [ frameLabel      := str
                -- , frameShadowType := ShadowEtchedOut
                , containerChild  := widget ]
      return (toWidget frame, sink)

instance Lambda MkI MkO where
  lambda (MkI ia) (MkO ob) = MkO $
    mdo box  <- boxNew Vertical True 10
        reff <- newIORef (error "mkLambda: no function yet")
        let update = do f <- readIORef reff
                        a <- geta   -- note loop
                        snkb (f a)
        (wa,geta) <- ia update
        (wb,snkb) <- ob
        set box [ containerChild := wa , containerChild := wb ]
        return ( toWidget box
               , \ f -> writeIORef reff f >> update )


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
         return (toWidget w, getter)


toggleI :: Bool -> In Bool
toggleI start = primMkI $ \ refresh ->
  do w <- checkButtonNew
     toggleButtonSetActive w start
     onToggled w refresh
     return (toWidget w, toggleButtonGetActive w)

toggleO :: Out Bool
toggleO = primMkO $
  do w <- checkButtonNew
     return (toWidget w, toggleButtonSetActive w)

mkFileName :: FilePath -> In FilePath
mkFileName start = primMkI $ \ refresh ->
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     fileChooserSetFilename w start
     onCurrentFolderChanged w refresh
     -- fileChooserGetFilename w >>= print    -- testing
     return ( toWidget w
            , fromMaybe start <$> fileChooserGetFilename w )

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
     return (toWidget entry, entryGetText entry)


textO :: Out String
textO = primMkO $
        do entry <- entryNew
           return (toWidget entry, entrySetText entry)

-- textO = primMkO $
--         do lab <- labelNew Nothing
--            return (toWidget lab, labelSetText lab)


{-



mkUnitIn :: MkI ()
mkUnitIn () _ = do w <- vBoxNew True 0
                   return (toWidget w, return ())


-- type MkO a = IO (Widget, Sink a)

mkUnitOut :: MkO ()
mkUnitOut = do w <- vBoxNew True 0
               return (toWidget w, return)

-- type MkI  a = a -> IO () -> IO (Widget, IO a)


{--------------------------------------------------------------------
    Render UI into MkI
--------------------------------------------------------------------}

-- TODO: Move runUI to another module, say "GtkUI"

runUI :: Output a -> String -> Sink a
runUI = makeUI . renderO
 where
   renderO :: Output c -> MkO c
   renderO  BoolOut               = mkToggleOut
   -- renderO (RealOut lo hi)        = mkSliderROut lo hi
   -- renderO (IntOut  lo hi)        = mkSliderIOut lo hi
   -- renderO  TextureOut            = mkTextureOut
   renderO  TextOut               = mkTextOut
   renderO (LabelOut l u)         = mkLabeledOut l (renderO u)
   renderO  UnitOut               = mkUnitOut
   renderO (PairOut u v)          = mkPairOut (renderO u) (renderO v)
   renderO (FunctionOut a0 ia ob) = mkLambda a0 (renderI ia) (renderO ob)
   renderO _                      = error "renderO: unhandled case"
   
   renderI :: Input c -> MkI c
   renderI  BoolIn                = mkToggleIn
   renderI (RealIn lo hi)         = mkSliderRIn lo hi
   renderI (IntIn  lo hi)         = mkSliderIIn lo hi
   renderI  TextureIn             = mkTextureIn
   renderI (LabelIn l u)          = mkLabeledIn l (renderI u)
   renderI  UnitIn                = mkUnitIn
   renderI (PairIn u v)           = mkPairIn (renderI u) (renderI v)
   

-- runUI :: String -> Input a -> a -> MkO a -> IO ()
-- runUI = (result.argument) render makeUI
--  where
--    render :: Input c -> MkI c
--    render  BoolIn          = mkToggle
--    render (RealIn lo hi)   = mkSliderR lo hi
--    render (IntIn  lo hi)   = mkSliderI lo hi
--    render  TextureIn       = mkTexture
--    render (LabelIn l u)    = mkLabeledIn l (render u)
--    render  UnitIn          = mkUnit
--    render (PairIn o u v)   = mkPairIn o (render u) (render v)
   


{--------------------------------------------------------------------
    Errors
--------------------------------------------------------------------}


checkErrors :: Show a => String -> IO [a] -> IO ()
checkErrors name getter =
  do errs <- getter
     unless (null errs) $
       putStrLn (name ++ " Error Log: " ++ show errs)

showGLErrors :: IO ()
showGLErrors = checkErrors "GL" (GL.get errors)


{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

t0 :: IO ()
t0 = makeUI (mkLambda a mkI mkO) "Test t0" show
 where
   mkI = mkLabeledIn "Slider 1" s `mkPairIn` mkLabeledIn "Slider 2" s
     where s = mkSliderRIn 0 10
   a   = (3,7)
   mkO = mkLabeledOut "Sum" mkTextOut

u1 :: Input R
u1 = LabelIn "size" $ RealIn 0 10
u2 :: Input Bool
u2 = LabelIn "happy" BoolIn
u3 :: Input (R, Bool)
u3 = PairIn u1 u2
u4 :: Input Int
u4 = LabelIn "cookies" $ IntIn 0 10

test input a0 = runUI (FunctionOut a0 input TextOut) "test" show

t1 = test u1 3
t2 = test u2 False
t3 = test u3 (3,False)

t4 = test u4 5

t5 = test (PairIn u1 u4) (3,7)

-- t5 = runUI TextureIn "Gtk.hs" print
-- t6 = runUI TextureIn "/home/conal/Pictures/phone pics/Image002.jpg" print

-- WORKING HERE: curried function.

u6 = FunctionOut 3 u1 $ FunctionOut False u2 $ TextOut

t6 = runUI u6 "currying"  (\ a b -> show (a,b))

t7 = test TextureIn 0

{-
tryTex str = do allInit 
                loadTexture' str >>= print
                IL.showErrors
                showGLErrors

t8 = tryTex "/home/conal/Haskell/gadget/src/marble-256.png"
t9 = tryTex "/home/conal/cabal/nehe-tuts-0.1.1/Data/NeHe.bmp"
-}

main = t6

-- With a bogus file, I get errors.  With a legit file I get no errors and
-- texture 0.  Hm!  Could 0 be correct?  Try applying the texture.

-}

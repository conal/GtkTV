{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, ScopedTypeVariables
           , TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}   -- TEMP
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
    In, Out, GTV, gtv, runGTV, runOut, runOutIO
    -- * UI primitives
  , R, sliderRIn, sliderIIn, clockIn
  , rateSliderIn, integralIn
  , fileNameIn, renderOut
  , emptyTexture, textureIsEmpty, textureIn
  , module Interface.TV
  ) where

import Control.Applicative (liftA2,(<$>))
import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)

import Data.Time (getCurrentTime,utctDayTime)

import Graphics.UI.Gtk hiding (Action)
import Graphics.UI.Gtk.OpenGL
import qualified Graphics.Rendering.OpenGL as G
import Graphics.Rendering.OpenGL hiding (Sink,get)
-- For textures
import Data.Bitmap.OpenGL
import Codec.Image.STB

-- From TypeCompose
import Data.Title
import Data.Pair
import Data.Lambda
import Control.Compose (ToOI(..),Cofunctor(..),Flip(..))

-- From vector-space
import Data.VectorSpace

import Interface.TV

{--------------------------------------------------------------------
    TV type specializations
--------------------------------------------------------------------}

type In  = Input  MkI
type Out = Output MkI MkO
type GTV = TV MkI MkO

-- | Type specialization of 'tv'
gtv :: Out a -> a -> GTV a
gtv = tv

-- | Type specialization of 'runTV'
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

-- Representation type for 'MkI'.  Takes a change call-back and produces a widget, a
-- polling operation and a termination clean-up action.
type MkI' a = Action -> IO (Widget, IO a, Action)

-- Make an output UI.
newtype MkO a = MkO (MkO' a)

-- Representation type for 'MkO'.  Produce a widget, a way to send it new
-- info to display, and a termination clean-up action.
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

-- | Add pre-processing.
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

runMkO :: String -> MkO a -> a -> Action
runMkO = (result.result.argument) return runMkOIO

-- runMkO name mko = runMkOIO name mko . return


runMkOIO :: String -> MkO a -> IO a -> Action
runMkOIO name (MkO mko') mkA = do
  initGUI
  (wid,sink,cleanup) <- mko'
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
  -- Initial sink.  Must come after show-all for the GLDrawingArea.
  mkA >>= sink
  mainGUI
  return ()

instance ToOI MkO where
  toOI mkO = Flip (runMkO "GtkTV" mkO)


-- | Run a visualization on a constructed ('IO'-extracted) value.  The
-- action is executed just once, after the visualization is all set up,
-- which allows for things like OpenGL shader compilation.
runOutIO :: String -> Out a -> IO a -> Action
runOutIO name out = runMkOIO name (output out)

runOut :: String -> Out a -> a -> Action
runOut = (result.result.argument) return runOutIO
-- runOut name out = runOutIO name out . return

-- I'd like to eliminate the glew dependency, and I don't know how.  The
-- ToOI method doesn't get a chance to pass in specialized info.  Hm.


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
    do box <- boxNew Horizontal False 10
       (wa,geta,cleana) <- ia refresh
       (wb,getb,cleanb) <- ob refresh
       set box [ containerChild := wa , containerChild := wb ]
       return (toWidget box, liftA2 (,) geta getb, cleana >> cleanb)

instance Pair MkO where
  pair (MkO oa) (MkO ob) = MkO $
    do box <- boxNew Horizontal False 10
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
    mdo box  <- boxNew Vertical False 0  -- 10?
        reff <- newIORef (error "mkLambda: no function yet")
        let update = do f <- readIORef reff
                        a <- geta   -- forward ref geta
                        snkb (f a)  -- forward ref snkb
        (wa,geta,cleana) <- ia update
        (wb,snkb,cleanb) <- ob
        -- set box [ containerChild := wa , containerChild := wb ]
        -- Hack: stretch output but not input.  Really I want to choose
        -- per widget and propagate upward.
        boxPackStart box wa PackNatural 0
        boxPackStart box wb PackGrow    0
        return ( toWidget box
               , \ f -> writeIORef reff f >> update
               , cleana >> cleanb)


primMkI :: MkI' a -> In a
primMkI = iPrim . MkI

-- Currently unused

primMkO :: MkO' a -> Out a
primMkO = oPrim . MkO

type R = Float

-- TODO: Consider using R == Double (for constant folding), while really
-- being float on the GLSL side.

sliderRIn :: (R,R) -> R -> In R
sliderRIn = sliderGIn realToFrac realToFrac 0.01 5

sliderIIn :: (Int,Int) -> Int -> In Int
sliderIIn = sliderGIn fromIntegral round 1 0

-- The step argument indicates how big a jump to make when clicking to one
-- side of the slider tab.  Seems to be a fraction of the whole range,
-- rather than a fixed amount.  I don't know what makes a good choice.

-- Generalized slider.  Gtk's scaling widgets work with Double, so this
-- adapter takes some initial params for conversion.  Only fires when a
-- value really changes.
sliderGIn :: (Show a, Eq a) => (a -> Double) -> (Double -> a) -> a -> Int
            -> (a,a) -> a -> In a
sliderGIn toD fromD step digits
             (lo,hi) a0 = primMkI $ \ refresh ->
  do oldRef <- newIORef a0
     w <- hScaleNewWithRange (toD lo) (toD hi) (toD step)
     set w [ rangeValue := toD a0, scaleDigits := digits ]
     let getter = fromD <$> get w rangeValue
         changeTo new =
           do old <- readIORef oldRef
              when (old /= new) $
                   do refresh
                      writeIORef oldRef new
     -- Unlike wxHaskell, I guess call-backs aren't attributes in gtk2hs.
     afterRangeChangeValue w (\ _ x -> changeTo (fromD x) >> return False)
     -- TODO: experiment with return False vs True
     return (toWidget w, getter, return ())

-- -- Prevent vertical stretching
-- noVert :: WidgetClass w => w -> IO Widget
-- noVert w = do b <- boxNew Vertical False 0
--               boxPackStart b w PackNatural 0
--               return (toWidget b)


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


-- | Rate slider.  Convenience function built from 'sliderRin' and 'integralDtIn'.
rateSliderDtIn :: R -> (R,R) -> R -> In R
rateSliderDtIn period = (result.result) (integralDtIn period) sliderRIn

-- | Rate slider.  Updates result (integral) 60 times per second.
-- Convenience function built from 'sliderRin' and 'integralIn'.
rateSliderIn :: (R,R) -> R -> In R
rateSliderIn = rateSliderDtIn (1/60)

-- | Integral of an input, with given update interval (in seconds)
integralDtIn :: (VectorSpace v, Eq v, Scalar v ~ Float) =>
                R -> In v -> In v
integralDtIn period inp = primMkI $ \ refresh ->
  do refT  <- time >>= newIORef
     refX  <- newIORef zeroV
     (w,getV,cleanV) <- mkI' (return ())
     timeout <- timeoutAddFull (refresh >> return True)
                  priorityDefaultIdle (round (period * 1000))
     let getX = do v <- getV
                   prevX <- readIORef refX
                   if (v /= zeroV) then
                     do t <- time
                        prevT <- readIORef refT
                        let x = prevX ^+^ (t - prevT) *^ v
                        writeIORef refT t
                        writeIORef refX x
                        return x
                    else
                       return prevX
     return (w, getX, timeoutRemove timeout >> cleanV)
 where
   MkI mkI' = input inp

-- Better: getX changes no state.  Instead, update refT & refX when slider changes.
-- In any case, only invoke refresh when the rate is nonzero

-- | Integral of an input.  Updates result (integral) 60 times per second.
integralIn :: (VectorSpace v, Eq v, Scalar v ~ Float) =>
              In v -> In v
integralIn = integralDtIn (1/60)


-- CONCERN: integration can apply to pair-valued inputs (e.g., constructed
-- by 'pair'), but the DeepArrow dissecting operations will not be able to
-- split apart the (pair-valued) integral input.


{--------------------------------------------------------------------
    GtkGL stuff
--------------------------------------------------------------------}

mkCanvas :: IO GLDrawingArea
mkCanvas =
 glConfigNew [ GLModeRGBA, GLModeDepth , GLModeDouble, GLModeAlpha ]
  >>= glDrawingAreaNew

-- | Render output, given a rendering action.  Handles all set-up.
-- Intended as an implementation substrate for functional graphics. 
renderOut :: Out Action
renderOut = primMkO $
  do initGL
     canvas <- mkCanvas
     widgetSetSizeRequest canvas 300 300
     -- Initialise some GL setting just before the canvas first gets shown
     -- (We can't initialise these things earlier since the GL resources that
     -- we are using wouldn't have been set up yet)
     -- TODO experiment with moving some of these steps.
     onRealize canvas $ withGLDrawingArea canvas $ const $
       do -- setupMatrices  -- do elsewhere, e.g., runSurface
          depthFunc  $= Just Less
          drawBuffer $= BackBuffers
          clearColor $= Color4 0 0 0.2 1
     -- Stash the latest draw action for use in onExpose
     drawRef <- newIORef (return ())
     let display draw =
           -- Draw in context
           withGLDrawingArea canvas $ \ glwindow ->
              do clear [DepthBuffer, ColorBuffer]
                 flipY
                 draw
                 flipY
                 -- glWaitVSync
                 finish
                 glDrawableSwapBuffers glwindow
                 writeIORef drawRef draw
     -- Sync canvas size with and use draw action
     onExpose canvas $ \_ -> 
       do (w',h') <- widgetGetSize canvas
          let w = fromIntegral w' :: GLsizei
              h = fromIntegral h'
              maxWH = w `max` h
              start s = fromIntegral ((s - maxWH) `div` 2)
          viewport $= (Position (start w) (start h), Size maxWH maxWH)  -- square
          readIORef drawRef >>= display
          return True
     return (toWidget canvas, display, return ())

flipY :: Action
flipY = scale 1 (-1 :: GLfloat) 1

-- Is there another way to flip Y?

-- | An empty texture.  Test with 'textureIsEmpty'
emptyTexture :: TextureObject
emptyTexture = TextureObject bogusTO

bogusTO :: G.GLuint
bogusTO = -1

-- | Is a texture empty?
textureIsEmpty :: TextureObject -> Bool
textureIsEmpty (TextureObject i) = i == bogusTO

loadTexture :: FilePath -> IO (Either String TextureObject)
loadTexture path =
  do e  <- loadImage path
     case e of
       Left err -> return (Left err)
       Right im -> Right <$> makeSimpleBitmapTexture im


-- Is there a more elegant formulation of loadTex?  It's close to
-- being fmap on Either.  I can almost get there as follows:
-- 
--   foo :: FilePath -> IO (Either String (IO TextureObject))
--   foo = (result.fmap.fmap) makeSimpleBitmapTexture loadImage

-- loadImage :: FilePath -> IO (Either String Image)
-- makeSimpleBitmapTexture :: Image -> IO TextureObject



fileNameIn :: FilePath -> In FilePath
fileNameIn start = primMkI $ \ refresh ->
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     fileChooserSetFilename w start
     onCurrentFolderChanged w refresh
     return ( toWidget w
            , fromMaybe start <$> fileChooserGetFilename w
            , return () )

textureIn :: In TextureObject
textureIn = fileMungeIn loadTexture deleteTexture emptyTexture

deleteTexture :: Sink TextureObject
deleteTexture tex | textureIsEmpty tex = return ()
                  | otherwise          =
                      do -- putStrLn $ "deleteTexture " ++ show tex
                         deleteObjectNames [tex]

fileMungeIn :: -- Show a =>   -- for debugging
               (FilePath -> IO (Either String a)) -> Sink a -> a -> In a
fileMungeIn munge free start = primMkI $ \ refresh ->
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     current <- newIORef start
     -- onCurrentFolderChanged w $ putStrLn "onCurrentFolderChanged"
     -- onFileActivated w $ putStrLn "onFileActivated"
     -- I'm changing the value on preview.  TODO: change back if the
     -- user cancels.
     onUpdatePreview w $
       do -- putStrLn "onUpdatePreview"
          mb <- fileChooserGetFilename w
          case mb of
            Nothing -> return ()
            Just path ->
              do e <- munge path
                 case e of
                   Left   _ -> return ()
                   -- Left err -> putStrLn $ "fileMungeIn error: " ++ err
                   Right a  -> do readIORef current >>= free
                                  writeIORef current a
                                  -- putStrLn $ "fileMungeIn: new value " ++ show a
                                  refresh
     return (toWidget w, readIORef current, return ())


-- TODO: Replace the error message with a GUI version.

-- We're freeing the old thingie before saving the new thingie.  In a
-- multi-threaded setting, there could be dire consequences.

-- I'd like to move to a consistently GC'd setting, in which textures,
-- shaders, etc are GC'd.  In that case, what keeps GPU resources alive?

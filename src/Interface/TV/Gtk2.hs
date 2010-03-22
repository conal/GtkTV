{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
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
-- 
-- This variation eliminates mdo by having MkI' produce a consumer of
-- refresh actions rather than taking a refresh action as argument.
----------------------------------------------------------------------

module Interface.TV.Gtk2
  ( -- * TV type specializations
    In, Out, GTV, gtv, runGTV, runOut, runOutIO
    -- * UI primitives
  , R, sliderRIn, sliderIIn, clockIn
  , rateSliderIn, integralIn
  , fileNameIn, renderOut
  , emptyTexture, textureIsEmpty, textureIn
  , module Interface.TV
  ) where

import Control.Applicative (liftA2,(<$>),(<*>))
import Control.Monad (when,join)
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

-- From vector-space
import Data.VectorSpace

-- From TypeCompose
import Data.Title
import Data.Pair
import Data.Lambda
import Control.Compose (ToOI(..),Cofunctor(..),Flip(..))

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

-- Equivalently:
-- 
--   runGTV :: RunTV MkI MkO


{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

-- | Add post-processing.  (Could use 'fmap' instead, but 'result' is more
-- specifically typed.)
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)

-- | Add pre-processing.
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

infixr 1 ~>
-- | Add pre- and post processing
(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
f ~> h = result h . argument f

-- (f ~> h) g = h . g . f

-- More generally,
-- 
-- (~>) :: Category (-->) => (a' --> a) -> (b --> b') -> ((a --> b) -> (a' --> b'))

-- If I add argument back to DeepArrow, we can get a different generalization:
-- 
-- (~>) :: DeepArrow (-->) => (a' --> a) -> (b --> b') -> ((a -> b) --> (a' -> b'))
-- f ~> h = result h . argument f  -- generalized (.)


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
newtype MkI  a = MkI { unMkI :: MkI' a }

inMkI :: (MkI' a -> MkI' b) -> (MkI a -> MkI b)
inMkI = unMkI ~> MkI

inMkI2 :: (MkI' a -> MkI' b -> MkI' c) -> (MkI a -> MkI b -> MkI c)
inMkI2 = unMkI ~> inMkI

-- Representation type for 'MkI'.  Produces a widget, a polling operation,
-- a termination clean-up action, and a sink for a refresh action to be
-- constructed later.
type MkI' a = IO (Widget, IO a, Action, Sink Action)

-- TODO: Look to reformulate to make the explicit class instances unnecessary.

-- Make an output UI.
newtype MkO a = MkO { unMkO :: MkO' a }

inMkO :: (MkO' a -> MkO' b) -> (MkO a -> MkO b)
inMkO = unMkO ~> MkO

inMkO2 :: (MkO' a -> MkO' b -> MkO' c) -> (MkO a -> MkO b -> MkO c)
inMkO2 = unMkO ~> inMkO


{--------------------------------------------------------------------
    Instances
--------------------------------------------------------------------}

-- Representation type for 'MkO'.  Produce a widget, a way to send it new
-- info to display, and a termination clean-up action.
type MkO' a = IO (Widget, Sink a, Action)

-- Currently, the clean-up actions are created only by clockDtI, and just
-- propagated by the other combinators.

instance Functor MkI where
  fmap f = inMkI (fmap f')
    where
      f' (wid,poll,clean,install) = (wid, fmap f poll, clean, install)

-- Or
-- 
--   fmap = inMkI . fmap . h
--     where
--       h f (wid,poll,clean,install) = (wid, fmap f poll, clean, install)

-- Or
-- 
--   fmap = inMkI . fmap . \ f (wid,poll,clean,install) -> (wid, fmap f poll, clean, install)

-- Better yet: tweak the representation so that Functor is derived.

instance Cofunctor MkO where
  cofmap f = inMkO (fmap f')
   where
     f' (wid,sink,cleanup) = (wid, sink . f, cleanup)

--   cofmap f (MkO mk) = MkO (fmap f' mk)
--    where
--      f' (wid,sink,cleanup) = (wid, sink . f, cleanup)

-- Note that Functor & Cofunctor are isomorphic to a standard form.
-- Consider redefining MkI' and MkO' accordingly.  See how other instances
-- work out.

forget :: Monad m => (w -> a -> m b) -> (w -> a -> m ())
forget = (result.result) ( >> return ())

-- forget h w a = h w a >> return ()

instance CommonIns MkI where
  getString start = MkI $
    do w <- entryNew
       entrySetText w start
       return (toWidget w, entryGetText w, return (), forget onEntryActivate w)
  getRead = getReadF  -- thanks to MkI Functor
  getBool start = MkI $
    do w <- checkButtonNew
       toggleButtonSetActive w start
       return (toWidget w, toggleButtonGetActive w, return (), forget onToggled w)

instance CommonOuts MkO where
  putString = MkO $
    do entry <- entryNew
       return (toWidget entry, entrySetText entry, return ())
  putShow = putShowC  -- thanks to MkO Cofunctor
  putBool = MkO $
    do w <- checkButtonNew
       return (toWidget w, toggleButtonSetActive w, return ())


boxed :: Orient -> Widget -> Widget -> IO Widget
boxed o wa wb =
  do box <- boxNew o False 10
     set box [ containerChild := wa , containerChild := wb ]
     return (toWidget box)

hboxed :: Widget -> Widget -> IO Widget
hboxed = boxed Horizontal

instance Pair MkI where
  pair = inMkI2 $ \ ia ib ->
    do (wa,geta,cleana,installa) <- ia
       (wb,getb,cleanb,installb) <- ib
       box <- wa `hboxed` wb
       return ( box
              , liftA2 (,) geta getb
              , cleana >> cleanb
              , liftA2 (>>) installa installb
              )

instance Pair MkO where
  pair = inMkO2 $ \ oa ob ->
    do (wa,snka,cleana) <- oa
       (wb,snkb,cleanb) <- ob
       box <- wa `hboxed` wb
       return (box, snka >+> snkb, cleana >> cleanb)

-- These Pair instances are getting closer to vanishing.  Regular
-- structure is emerging. Keep inching along.  One wart is that hboxed is
-- an IO operation, unlike the other combiners.

instance Title_f MkI where
  title_f str = inMkI $ \ ia ->
    do (widget,geta,cleana,installa) <- ia
       frame  <- frameNew
       set frame [ frameLabel      := str
                 -- , frameShadowType := ShadowEtchedOut
                 , containerChild  := widget ]
       return (toWidget frame, geta, cleana, installa)

instance Title_f MkO where
  title_f str = inMkO $ \ oa ->
   do (widget,sink,clean) <- oa
      frame  <- frameNew
      set frame [ frameLabel      := str
                -- , frameShadowType := ShadowEtchedOut
                , containerChild  := widget ]
      return (toWidget frame, sink, clean)

instance Lambda MkI MkO where
  lambda = (unMkI ~> unMkO ~> MkO) $ \ ia ob ->
    do box  <- boxNew Vertical False 0  -- 10?
       reff <- newIORef (error "mkLambda: no function yet")
       (wa,geta,cleana,installa) <- ia
       (wb,snkb,cleanb         ) <- ob
       let refresh = readIORef reff <*> geta >>= snkb
       installa refresh
       -- set box [ containerChild := wa , containerChild := wb ]
       -- Hack: stretch output but not input.  Really I want to choose
       -- per widget and propagate upward.
       boxPackStart box wa PackNatural 0
       boxPackStart box wb PackGrow    0
       return ( toWidget box
              , \ f -> writeIORef reff f >> refresh
              , cleana >> cleanb)


{--------------------------------------------------------------------
    Execution
--------------------------------------------------------------------}

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

primMkI :: MkI' a -> In a
primMkI = iPrim . MkI

-- Currently unused

primMkO :: MkO' a -> Out a
primMkO = oPrim . MkO

type R = Float

-- TODO: Consider using R == Double (for constant folding), while really
-- being float on the GLSL side.

sliderRIn :: (R,R) -> R -> In R
sliderRIn = sliderGIn realToFrac realToFrac 0.005 5

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
             (lo,hi) a0 = primMkI $
  do oldRef <- newIORef a0
     w <- hScaleNewWithRange (toD lo) (toD hi) (toD step)
     set w [ rangeValue := toD a0, scaleDigits := digits ]
     let getter = fromD <$> get w rangeValue
         install refresh = forget afterRangeChangeValue w
                             (\ _ x -> changeTo (fromD x) >> return False)
          where
            changeTo new =
              do old <- readIORef oldRef
                 when (old /= new) $
                      do refresh
                         writeIORef oldRef new
     -- TODO: experiment with return False vs True
     return (toWidget w, getter, return (), install)

-- -- Prevent vertical stretching
-- noVert :: WidgetClass w => w -> IO Widget
-- noVert w = do b <- boxNew Vertical False 0
--               boxPackStart b w PackNatural 0
--               return (toWidget b)


-- | A clock that reports time in seconds and updates at the given period
-- (in seconds).
clockDtI :: R -> In R
clockDtI period = primMkI $
  do start <- time
     -- Start with a do-nothing refresh action.
     refreshRef <- newIORef (return ())
     timeout <- timeoutAddFull (join (readIORef refreshRef) >> return True)
                  priorityDefaultIdle (round (period * 1000))
     w <- vBoxNew True 0    -- size 0 box
     return ( toWidget w, subtract start <$> time
            , timeoutRemove timeout, writeIORef refreshRef )


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
-- Specialization of 'rateSliderDtIn'.
rateSliderIn :: (R,R) -> R -> In R
rateSliderIn = rateSliderDtIn (1/60)

-- | Integral of an input, with given update interval (in seconds)
integralDtIn :: (VectorSpace v, Eq v, Scalar v ~ Float) =>
                R -> In v -> In v
integralDtIn period inp = primMkI $
  do refT  <- time >>= newIORef
     refX  <- newIORef zeroV
     refreshRef <- newIORef (return ())
     (w,getV,cleanV,_) <- mkI'
     timeout <- timeoutAddFull (join (readIORef refreshRef) >> return True)
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
     return (w, getX, timeoutRemove timeout >> cleanV, writeIORef refreshRef)
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
fileNameIn start = primMkI $
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     fileChooserSetFilename w start
     return ( toWidget w
            , fromMaybe start <$> fileChooserGetFilename w
            , return ()
            , forget onCurrentFolderChanged w
            )

textureIn :: In TextureObject
textureIn = fileMungeIn loadTexture deleteTexture emptyTexture

deleteTexture :: Sink TextureObject
deleteTexture tex | textureIsEmpty tex = return ()
                  | otherwise          =
                      do -- putStrLn $ "deleteTexture " ++ show tex
                         deleteObjectNames [tex]

fileMungeIn :: -- Show a =>   -- for debugging
               (FilePath -> IO (Either String a)) -> Sink a -> a -> In a
fileMungeIn munge free start = primMkI $
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     current <- newIORef start
     -- onCurrentFolderChanged w $ putStrLn "onCurrentFolderChanged"
     -- onFileActivated w $ putStrLn "onFileActivated"
     -- I'm changing the value on preview.  TODO: change back if the
     -- user cancels.
     let install refresh =
           forget onUpdatePreview w $
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
     return (toWidget w, readIORef current, return (), install)


-- TODO: Replace the error message with a GUI version.

-- We're freeing the old thingie before saving the new thingie.  In a
-- multi-threaded setting, there could be dire consequences.

-- I'd like to move to a consistently GC'd setting, in which textures,
-- shaders, etc are GC'd.  In that case, what keeps GPU resources alive?

-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  test
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test GtkTV
----------------------------------------------------------------------

import Data.Lambda (lambda) -- or use oLambda
import Data.Pair   (pair)   -- or use iPair, oPair

import Interface.TV
import Interface.TV.Gtk


{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

i1 :: In R
i1 = iTitle "size" $ sliderRI (0,10) 3

i2 :: In Bool
i2 = iTitle "happy" $ toggleI False

i3 :: In (R, Bool)
i3 = iPair i1 i2

i4 :: In Int
i4 = iTitle "cookies" $ sliderII (0,10) 5

testI :: Show a => In a -> IO ()
testI i = runOut (oLambda i textO) "test" show

t1,t2,t3,t4,t5 :: IO ()
t1 = testI i1
t2 = testI i2
t3 = testI i3
t4 = testI i4
t5 = testI (pair i1 i4)

-- t5 = runUI TextureIn "Gtk.hs" print
-- t6 = runUI TextureIn "/home/conal/Pictures/phone pics/Image002.jpg" print

-- WORKING HERE: curried function.

o6 :: Out (R -> Bool -> String)
o6 = lambda i1 $ lambda i2 $ textO

t6 :: IO ()
t6 = runOut o6 "currying"  (\ a b -> show (a,b))

{-

t7 = testI TextureIn 0

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

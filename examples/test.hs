-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  test
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test GtkTV
----------------------------------------------------------------------

import Data.Lambda (lambda) -- or use oLambda
import Data.Pair   (pair)   -- or use iPair, oPair
import Data.Title  (title)  -- or use iTitle, oTitle

-- import Interface.TV (tv,runTV,boolIn,stringOut,oLambda)
-- import Interface.TV.Gtk (In,Out,gtv,R,sliderRI,sliderII)

import Interface.TV.Gtk


{--------------------------------------------------------------------
    Tests
--------------------------------------------------------------------}

i1 :: In R
i1 = title "size" $ sliderRIn (0,10) 3

i2 :: In Bool
i2 = title "happy" $ boolIn False

i3 :: In (R, Bool)
i3 = pair i1 i2

i4 :: In Int
i4 = title "cookies" $ sliderIIn (0,10) 5

-- testI i = runOut "test" (lambda i stringOut) show

testI :: Show a => In a -> IO ()

-- testI i = runTV (tv (lambda i (stringOut :: Out String)) show)

-- The explicit typing is unfortunate here.  Alternatively, use gtv or runGTV:

testI i = runGTV (tv (oLambda i showOut) id)


t1,t2,t3,t4,t5 :: IO ()
t1 = testI i1
t2 = testI i2
t3 = testI i3
t4 = testI i4
t5 = testI (pair i1 i4)

o6 :: Out (R -> Bool -> String)
o6 = lambda i1 $ lambda i2 $ stringOut

t6 :: IO ()
t6 = runTV $ tv (title "currying" o6)  (curry show)

-- t6 = runOut "currying" o6  (\ a b -> show (a,b))

{-

t7 = testI TextureIn 0

tryTex str = do allInit 
                loadTexture' str >>= print
                IL.showErrors
                showGLErrors

t8 = tryTex "marble-256.png"

main = t6

-- With a bogus file, I get errors.  With a legit file I get no errors and
-- texture 0.  Hm!  Could 0 be correct?  Try applying the texture.

-}

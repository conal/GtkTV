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

import Control.Arrow ((&&&))

import Interface.TV.Gtk
import Control.Arrow.DeepArrow
import Data.FunArr

import Data.Lambda (lambda) -- or use oLambda
import Data.Pair   (pair)   -- or use iPair, oPair
import Data.Title  (title)  -- or use iTitle, oTitle

-- import Interface.TV (tv,runTV,boolIn,stringOut,oLambda)
-- import Interface.TV.Gtk (In,Out,gtv,R,sliderRI,sliderII)


{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

type Action = IO ()
type Sink a = a -> Action


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

testI :: Show a => Sink (In a)

-- testI i = runTV (tv (lambda i (stringOut :: Out String)) show)

-- The explicit typing is unfortunate here.  Alternatively, use gtv or runGTV:

testI i = runGTV (tv (oLambda i showOut) id)


t1,t2,t3,t4,t5 :: Action
t1 = testI i1
t2 = testI i2
t3 = testI i3
t4 = testI i4
t5 = testI (pair i1 i4)

o6 :: Out (R -> Bool -> String)
o6 = lambda i1 $ lambda i2 $ stringOut

tv6 :: GTV (R -> Bool -> String)
tv6 = tv (title "currying" o6)  (curry show)

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

{--------------------------------------------------------------------
    Taken from GuiTV (based on Reactive, Phooey, and wxhaskell)
--------------------------------------------------------------------}

apples, bananas :: CInput Int
apples  = title "apples"  defaultIn
bananas = title "bananas" defaultIn

total :: Show a => COutput a
total = title "total" showOut

shoppingO :: COutput (Int -> Int -> Int)
shoppingO = title "shopping list" $
            oLambda apples (oLambda bananas total)


shopping :: CTV (Int -> Int -> Int)
shopping = tv shoppingO (+)

-- Uncurried variant
shoppingPr :: CTV ((Int,Int) -> Int)
shoppingPr = tv ( title "shopping list -- curried" $ 
                  oLambda (iPair apples bananas) total )
                (uncurry (+))

-- Or simply use uncurryA
shoppingPr' :: CTV ((Int,Int) -> Int)
shoppingPr' = uncurryA $$ shopping


-- Sliders instead of default inputs
applesU, bananasU :: In Int
applesU  = title "apples"  (sliderIIn (0,10) 3)
bananasU = title "bananas" (sliderIIn (0,10) 7)

shoppingUO :: Out (Int -> Int -> Int)
shoppingUO = title "shopping list" $
             oLambda applesU (oLambda bananasU total)

shoppingU :: GTV (Int -> Int -> Int)
shoppingU = tv shoppingUO (+)

shoppingPrU :: GTV ((Int,Int) -> Int)
shoppingPrU = uncurryA $$ shoppingU


{--------------------------------------------------------------------
    Rendering examples
--------------------------------------------------------------------}

iGray :: In R
iGray = title "gray level" $ sliderRIn (0,1) 0.5

o7 :: Out (R -> Action)
o7 = lambda iGray renderO

tv7 :: GTV (R -> Action)
tv7 = tv o7 renderGray

-- Oscillate between 0 & 1
osc :: Floating n => n -> n
osc x = (sin x + 1) / 2

-- osc = (sin + 1) / 2
-- osc = (/ 2) . (+1) . sin

tv8 :: GTV (R -> Action)
tv8 = tv (lambda clockIn renderO) (renderGray . osc)

tv9 :: GTV (R -> (R,Action))
tv9 = tv (lambda clockIn (title "osc" showOut `pair` renderO)) ((id &&& renderGray) . osc)

tv10 :: GTV (R -> (Action,Action))
tv10 = result dupA $$ tv7

tv11 :: GTV (R -> Action, R -> Action)
tv11 = dupA $$ tv7

tv12 :: GTV (R -> (Action,Action))
tv12 = result dupA $$ tv8

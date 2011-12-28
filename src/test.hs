-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  test
-- Copyright   :  (c) Conal Elliott 2009-2011
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test GtkTV
----------------------------------------------------------------------

import Interface.TV.Gtk2     -- or Gtk
import Control.Arrow.DeepArrow (uncurryA)
import Data.FunArr (($$))

import Data.Lambda (lambda) -- or use oLambda
import Data.Pair   (pair)   -- or use iPair, oPair
import Data.Title  (title)  -- or use iTitle, oTitle


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

tv6' :: GTV ((R, Bool) -> String)
tv6' = uncurryA $$ tv6

t7 :: Action
t7 = testI $ sliderIIn (0,5) 3 

t8 :: Action
t8 = testI $ boolIn True

t9 :: Action
t9 = testI $ stringIn "bloop"

i5 :: In String
i5 = fileNameIn "bloop"

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

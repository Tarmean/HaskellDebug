{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -ddump-simpl -O2 -dsuppress-all -dsuppress-uniques#-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module OOP  where
-- import Data.Proxy

-- data ListPrinter a = LP {
--    plist :: ListPrinter a -> [a] -> String,
--    pelem :: ListPrinter a -> a -> String
-- }

-- pDec :: ListPrinter Int -> ListPrinter Int
-- pDec super = LP {
--     plist = \r xs -> plist super r xs,
--     pelem = \r d -> if d == 0 then "ZERO" else pelem super r d
-- }

-- pList :: (a -> String) -> ListPrinter a
-- pList f = LP {
--    plist = \r xs -> case  xs of
--       [] -> "[]"
--       (x:xs) -> pelem r r x <> " : " <> plist r r xs,
--    pelem = \_ -> f
-- }

-- class Printer r t a where
--      printImpl :: (?printRoot :: Proxy# r, ?printLocal :: Proxy# t) => a -> String

data Base
-- instance (Printer r r [x], Printer r r x) => Printer r Base [x] where
--    printImpl [] = "[]"
--    printImpl (x:xs) = self (printR x) <> " : " <> self (printR xs)
-- instance Printer r Base Int where
--    printImpl = show

-- data DecZero f
-- instance (Printer r f Int) => Printer r (DecZero f) Int where
--    {-# INLINE printImpl #-}
--    printImpl 0 = "ZERO"
--    printImpl x = super (printR x)
-- instance {-# OVERLAPS #-} (Printer r f x) => Printer r (DecZero f) x where
--    {-# INLINE printImpl #-}
--    printImpl x = super (printR x)


newtype Dispatch self root next  a = Dispatch { unDispatch :: a }
self :: (?printBase :: Proxy# root) => Dispatch x root root a -> a
self = unDispatch
super :: (?printLocal :: Proxy# (f parent)) => Dispatch (f parent) root parent a -> a
super = unDispatch
-- printR ::forall x l a r.  (?printRoot :: Proxy# r, ?printLocal :: Proxy# l, Printer r x a) => a -> Dispatch l r x String
-- printR x = Dispatch (let ?printLocal = proxy# :: Proxy# x in printImpl x)

-- callPrint :: forall r a. (Printer r r a) => a -> String
-- callPrint a = let ?printRoot = proxy# :: Proxy# r; ?printLocal = proxy# :: Proxy# r in self (printR a)

data Proxy# a
proxy# :: Proxy# a
proxy# = error "Forced proxy"

-- >>> callPrint @(DecZero Base) [0,1,2,3 ::Int]
-- "ZERO : 1 : 2 : 3 : []"

-- Rec {
-- -- RHS size: {terms: 34, types: 30, coercions: 0, joins: 0/0}
-- $w$dPrinter
--   = \ w w1 ->
--       case w1 of {
--         [] -> $fPrinterkTYPErBase[]2;
--         : x xs ->
--           case x of { I# ds ->
--           case ds of ds1 {
--             __DEFAULT ->
--               case $witos ds1 [] of { (# ww1, ww2 #) ->
--               ++_$s++
--                 (unpackAppendCString# $fPrinterkTYPErBase[]1 ($w$dPrinter w xs))
--                 ww1
--                 ww2
--               };
--             0# ->
--               ++
--                 lvl1
--                 (unpackAppendCString# $fPrinterkTYPErBase[]1 ($w$dPrinter w xs))
--           }
--           }
--       }
-- end Rec }


-- printInts :: [Int] -> String
-- printInts a = callPrint @(DecZero Base) a
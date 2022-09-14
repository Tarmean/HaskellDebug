{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -fforce-recomp #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc

    ) where
import GHC.IO (unsafePerformIO)
import GHC.HeapView (ppHeapGraph, buildHeapGraph, asBox)
import GHC.Stack.CCS (whereFrom)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V

import UI

someFunc :: IO ()
someFunc = do
   let 
     {-# NOINLINE as #-}
     as = [1,2,4]
     {-# NOINLINE xs #-}
     !xs = foo . filter even  $ as
   x <- printValue' xs
   -- print =<< whereFrom x
   -- hg <- buildHeapGraph 10 () $ asBox x 
   -- print $ ppHeapGraph hg
   print x



{-# NOINLINE foo #-}
foo :: [Int] -> [Int]
foo ls =   scanl (+) 0 (map (*2) ls)


printValue' :: b -> IO b
printValue' a = do
    printValue a
    pure a


-- debug :: a -> a
-- debug a = unsafePerformIO (putStrLn "dbg" >> printGraph >> printWhere) `seq` a
--   where
--     printGraph = print . ppHeapGraph =<< buildHeapGraph 20 () (asBox a)
--     printWhere = putStrLn (prettyAny a)
-- {-# NOINLINE debug #-}



-- prettyAny :: a -> String
-- prettyAny a = T.unpack source <> " :: " <> typ <> show x
--   where
--     x@[coreName, ident, typ, context, mod, loc] = unsafePerformIO (whereFrom a)
--     source 
--       | null loc = "<?>"
--       | otherwise = sourceString (T.pack loc)

-- sourceString :: Loc -> T.Text
-- sourceString loc = theSlice 
--   where
--     (path, line, cols) = case T.splitOn ":" loc of
--       [a,b,c] -> (a,b,c)
--       o -> error $ "sourceString: " <> show loc <> ", " <> show o
--     [start, end] = map readT $ T.splitOn "-" cols
--     theLine = unsafePerformIO (retrieveCache (T.unpack path)) V.! (readT line - 1)
--     theSlice = T.take (end - start+1) (T.drop (start-1) theLine)

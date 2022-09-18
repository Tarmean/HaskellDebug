{-# OPTIONS_GHC -ddump-simpl -dsuppress-uniques -fforce-recomp #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin GhcDump.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc

    ) where

import UI ( printValue )
import State (runRequest, Requests (LoadGhcDump))
import Data.Text.Prettyprint.Doc (pretty)

someFunc :: IO ()
someFunc = do
   r <- runRequest (LoadGhcDump "Lib" 25)
   printValue r
  --  let 
  --    {-# NOINLINE as #-}
  --    as = [1..10]
  --    {-# NOINLINE xs #-}
  --    xs = foo . filter even  $ as
  --  xs `seq` pure ()
  --  printValue xs
{-# NOINLINE foo #-}
foo :: [Int] -> [Int]
foo ls =  scanl (+) 0 (map (*2) ls)



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

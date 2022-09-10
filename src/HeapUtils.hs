module HeapUtils where

import GHC.HeapView
import qualified Data.IntMap as IM
import WhereFrom


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


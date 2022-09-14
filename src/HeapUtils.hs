module HeapUtils where

import GHC.HeapView
import qualified Data.IntMap as IM
import WhereFrom
import Data.Functor ((<&>))
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


traverseHeapEntry :: Applicative m => (a -> m b) -> HeapGraphEntry a -> m (HeapGraphEntry b)
traverseHeapEntry f hg = f (hgeData hg) <&> \x -> hg { hgeData = x }


childrenOf :: HeapGraphIndex -> HeapGraph t -> [HeapGraphIndex]
childrenOf i hg = case lookupHeapGraph i hg of
  Nothing -> []
  Just hge -> catMaybes $ F.toList (hgeClosure hge)
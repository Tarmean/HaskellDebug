module HeapUtils where

import GHC.HeapView
import qualified Data.IntMap as IM
import WhereFrom
import Data.Functor ((<&>))


traverseHeapGraph :: Applicative m => (HeapGraphEntry a -> m (HeapGraphEntry b)) -> HeapGraph a -> m (HeapGraph b)
traverseHeapGraph f (HeapGraph im) = HeapGraph <$> traverse f im


traverseHeapEntry :: Applicative m => (a -> m b) -> HeapGraphEntry a -> m (HeapGraphEntry b)
traverseHeapEntry f hg = f (hgeData hg) <&> \x -> hg { hgeData = x }
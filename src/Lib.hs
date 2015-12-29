module Lib where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Heap a = Nil
            | Node a (Heap a) (Heap a)
            deriving (Show)

-- I'm sure there's a better way to do this...
maxHeapify :: Ord a => Heap a -> Heap a
-- if the heap is empty or has no children, it's already a max-heap
maxHeapify Nil = Nil
maxHeapify (Node p Nil Nil) = Node p Nil Nil

maxHeapify (Node p (Node l ll lr) (Node r rl rr))
    | p >= l && p >= r  = Node p (Node l ll lr) (Node r rl rr)
    | l > p && l >= r   = Node l (maxHeapify (Node p ll lr)) (Node r rl rr)
    | otherwise         = Node r (Node l ll lr) (maxHeapify (Node p rl rr))

maxHeapify (Node p (Node l ll lr) Nil)
    | p >= l    = Node p (Node l ll lr) Nil
    | otherwise = Node l (maxHeapify (Node p ll lr)) Nil

{-
  Code for prettifying and rendering heaps
  Shamelessly cribbed from https://github.com/cschneid/cschneid-pretty/blob/master/src/Cschneid/Pretty.hs
-}
instance Pretty a => Pretty (Heap a) where
  pPrint (Nil)        = empty
  pPrint (Node v l r) = vcat [ text "Node: " <> pPrint v
                             , nest 2 (pPrint l)
                             , nest 2 (pPrint r)]

renderHeap :: Pretty a => Heap a -> String
renderHeap = prettyShow

{- Some example data -}
exampleMaxHeap :: Heap Int
exampleMaxHeap =
 Node 5
   (Node 4
     (Node 2 Nil Nil)
     (Node 2 Nil Nil))
   (Node 3 Nil Nil)

maxHeapWithSmallRoot :: Heap Int
maxHeapWithSmallRoot =
  Node 1
    (Node 4
      (Node 2 Nil Nil)
      (Node 2 Nil Nil))
    (Node 3 Nil Nil)

module BinaryTree where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

binaryTreeDepth :: Tree a -> Int
binaryTreeDepth Leaf = 0
binaryTreeDepth (Node _ left right) =
  1 + max (binaryTreeDepth left) (binaryTreeDepth right)

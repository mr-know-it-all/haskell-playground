module BinaryTree where

data Tree a = TreeEmpty | Node a (Tree a) (Tree a) deriving Show

binaryTreeDepth :: Tree a -> Int
binaryTreeDepth TreeEmpty = 0
binaryTreeDepth (Node _ left right) =
  1 + max (binaryTreeDepth left) (binaryTreeDepth right)

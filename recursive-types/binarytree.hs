module BinaryTree where

data Tree a = TreeEmpty | Node a (Tree a) (Tree a) deriving Show

binaryTreeDepth :: Tree a -> Int
binaryTreeDepth TreeEmpty = 0
binaryTreeDepth (Node _ left right) =
  1 + max (binaryTreeDepth left) (binaryTreeDepth right)

binaryTreePreorder :: Tree a -> [a]
binaryTreePreorder TreeEmpty = []
binaryTreePreorder (Node x left right) = [x] ++ (binaryTreePreorder left) ++ (binaryTreePreorder right)

binaryTreeInorder :: Tree a -> [a]
binaryTreeInorder TreeEmpty = []
binaryTreeInorder (Node x left right) = (binaryTreePreorder left) ++ [x] ++ (binaryTreePreorder right)

binaryTreePostorder :: Tree a -> [a]
binaryTreePostorder TreeEmpty = []
binaryTreePostorder (Node x left right) = (binaryTreePreorder left) ++ (binaryTreePreorder right) ++ [x]

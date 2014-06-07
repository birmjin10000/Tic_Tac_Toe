import Data.List
import qualified Data.Foldable as F
import Data.Monoid

whose_turn position
    | (sum position == 0) = 1
    | otherwise = (-1)

replaceAt turn position index =
    a ++ [turn] ++ (drop 1 b)
    where (a, b) = splitAt index position

--moves::[a]->[[a]]
moves position = 
    let turn = whose_turn position 
    in map (replaceAt turn position) [index | (index, e) <- zip [0..] position, (e == 0)]

data Tree a = EmptyTree | Node a [Tree a] deriving (Show, Eq)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node label []) = f label
    foldMap f (Node label (x:xs)) = F.foldMap f x `mappend` F.foldMap f (Node label xs)

reptree f a = Node a (map (reptree f) (f a))

gametree position = reptree moves position


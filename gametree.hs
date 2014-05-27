import Data.List

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


data Node a = Node a [Node a] deriving (Show)

reptree f a = Node a (map (reptree f) (f a))

gametree position = reptree moves position


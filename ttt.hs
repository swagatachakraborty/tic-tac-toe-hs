import Data.List.Split
import Data.List

data Symbol = X | O deriving (Show,Eq)
data Position = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving(Show,Eq,Enum)
data Player = Player String Symbol deriving (Show,Eq)
data GameState = Setup | InPlay Player Player ([Position],[Position]) | HasWon Player | Drawn deriving(Show,Eq)

updateBoard :: ([Position],[Position]) -> Position -> Symbol -> ([Position],[Position])
updateBoard (xs, os) p X = (p : xs, os)
updateBoard (xs, os) p O = (xs, p:os)

InPlay :: Player -> Player -> ([Position], [Position]) -> GameState
--move
InPlay pNow pNext (xs, os) 
  |(hasWon xs) || (hasWon os) == True = HasWon pNow
  |(length xs + length os) == 9 = Drawn
  |otherwise = InPlay pNext pNow (xs, os)


Setup = InPlay (Player "p1" X) (Player "p2" O) ([],[])

diagonalComb = [[Zero,Four,Eight], [Two,Four,Six]]
rowComb = chunksOf 3 [Zero .. Eight]
columnComb = transpose rowComb
winComb = rowComb ++ columnComb ++ diagonalComb

isPresent :: (Eq a) => [a] -> a -> Bool
isPresent a b = b `elem` a

hasWon :: [Position] -> Bool
hasWon moves = any (all (isPresent moves)) winComb

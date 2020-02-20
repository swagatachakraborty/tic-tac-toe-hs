import Data.List.Split
import Data.List

type Board = ([Position],[Position])

data Symbol = X | O deriving (Show,Eq)
data Position = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving(Show,Eq,Enum)
data Player = Player {name :: String, symbol :: Symbol} deriving (Show,Eq)
data GameState = Setup | InPlay Player Player Board | HasWon Player | Drawn deriving(Show,Eq)

updateBoard :: Board -> Position -> Symbol -> Board
updateBoard (xs, os) p X = (p : xs, os)
updateBoard (xs, os) p O = (xs, p:os)

playGame :: GameState -> String
playGame (HasWon p) = (name p) ++ " has won"
playGame Drawn = "game is drawn"
playGame (InPlay pNow pNext (xs, os))
-- move
  |(hasWon xs) == True = playGame (HasWon pNow)
  |(hasWon os) == True = playGame (HasWon pNext)
  |(length xs + length os) == 9 = playGame Drawn
  |otherwise = playGame (InPlay pNext pNow (xs, os))

startGame :: IO String
startGame = do 
  putStrLn "Player 1 enter your name :"
  p1Name <- getLine
  putStrLn "Player 2 enter your name :"
  p2Name <- getLine
  return (playGame $ InPlay (Player p1Name X) (Player p2Name O) ([],[]))

diagonalComb = [[Zero,Four,Eight], [Two,Four,Six]]
rowComb = chunksOf 3 [Zero .. Eight]
columnComb = transpose rowComb
winComb = rowComb ++ columnComb ++ diagonalComb

isPresent :: (Eq a) => [a] -> a -> Bool
isPresent a b = b `elem` a

hasWon :: [Position] -> Bool
hasWon moves = any (all (isPresent moves)) winComb

main :: IO ()
main = do 
  putStrLn "Started"
  game <- startGame
  putStrLn game

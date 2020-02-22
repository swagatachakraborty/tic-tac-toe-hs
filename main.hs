import Data.List.Split
import Data.List
import Data.Set as Set

type Board = (Set Position, Set Position)

data Symbol = X | O deriving (Show,Eq)
data Position = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Invalid deriving(Show,Eq,Enum, Ord )
data Player = Player {name :: String, symbol :: Symbol} deriving (Show,Eq)
data GameState = Setup | InPlay Player Player Board | HasWon Player | Drawn deriving(Show,Eq)

diagonalComb = [[Zero,Four,Eight], [Two,Four,Six]]
rowComb = chunksOf 3 [Zero .. Eight]
columnComb = transpose rowComb
winComb = Set.fromList (Prelude.map Set.fromList (rowComb ++ columnComb ++ diagonalComb))

hasWon :: Set Position -> Bool
hasWon moves = any ((flip Set.isSubsetOf) moves) winComb

mapPosition :: String -> Position
mapPosition charCh
  | choice < (length choices) = choices !! choice
  | otherwise = Invalid
  where choices = [Zero .. Eight]
        choice = read charCh

updateBoard :: Board -> Symbol -> Position -> Board
updateBoard (xs, os) sm p
  | sm == X = (Set.insert p xs, os)
  | sm == O = (xs, Set.insert p os)
  | otherwise = (xs, os)

playGame :: GameState -> IO String
playGame (HasWon p) = return ((name p) ++ " has won")
playGame Drawn = return ("game is drawn")
playGame (InPlay pNow pNext (xs, os))
  |((hasWon xs) || (hasWon os)) == True = playGame (HasWon pNext)
  |(length xs + length os) == 9 = playGame Drawn
  |otherwise = 
    ((InPlay pNext pNow) <$> ((updateBoard (xs, os) (symbol pNow)) <$> (makeMove pNow))) >>= playGame

makeMove :: Player -> IO Position
makeMove p = do 
  putStrLn ((name p) ++ " enter your move :")
  choice <- getLine
  return (mapPosition choice)

startGame :: IO String
startGame = do 
  putStrLn "Player 1 enter your name :"
  p1Name <- getLine
  putStrLn "Player 2 enter your name :"
  p2Name <- getLine
  playGame $ InPlay (Player p1Name X) (Player p2Name O) (Set.empty,Set.empty)

main :: IO ()
main = do 
  putStrLn "-------------------Started-------------------"
  game <- startGame
  putStrLn game

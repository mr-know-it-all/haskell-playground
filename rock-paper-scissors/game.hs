import System.Random

data Move = Rock | Paper | Scissors deriving (Show, Eq, Enum)
data Game = Singleplayer | Multiplayer deriving (Show, Eq, Enum)

instance Ord Move where
  (<=) a b
    | a == b = True
    | elem (a, b) [(Rock, Paper), (Paper, Scissors), (Scissors, Rock)] = True
    | otherwise = False

parseMove :: String -> Move
parseMove = toEnum . read

parseGameMode :: String -> Game
parseGameMode = toEnum . read

getComputerMove :: IO Move
getComputerMove = fmap toEnum (randomRIO (0, 2))

resultString :: Game -> Ordering -> String
resultString game ord
  | ord == LT = if game == Singleplayer then "Computer wins" else "Player 2 Wins"
  | ord == GT = "Player 1 Wins"
  | otherwise = "Draw"

gameInstructions :: [Char]
gameInstructions = "Choose a move (0 for Rock, 1 for Paper, 2 for Scissors) :"

printChoices :: (Show a1, Show a2) => Game -> a1 -> a2 -> [Char]
printChoices game a b
  | game == Singleplayer = "Player: " ++ show a ++ " AI :D : " ++ show b
  | game == Multiplayer = "Player 1: " ++ show a ++ " Player 2: " ++ show b
  | otherwise = "nothing to say..."

printResult :: Ord a => Game -> a -> a -> String
printResult game a b = resultString game $ a `compare` b

singleplayerGame :: IO ()
singleplayerGame = do
  putStrLn gameInstructions
  player <- getLine
  computer <- getComputerMove
  putStrLn $ printChoices Singleplayer (parseMove player) computer
  putStrLn $ printResult Singleplayer (parseMove player) computer
  putStrLn "Do you want to play again? y/n"
  continue <- getLine
  if continue == "y"
    then singleplayerGame
    else putStrLn "Game over!"

multiplayerGame :: IO ()
multiplayerGame = do
  putStrLn gameInstructions
  putStrLn "Player 1:"
  playerOne <- getLine
  putStrLn "player 2:"
  playerTwo <- getLine
  putStrLn $ printChoices Multiplayer (parseMove playerOne) (parseMove playerTwo)
  putStrLn $ printResult Multiplayer (parseMove playerOne) (parseMove playerTwo)
  putStrLn "Do you want to play again? y/n"
  continue <- getLine
  if continue == "y"
    then multiplayerGame
    else putStrLn "Game over!"

main :: IO ()
main = do
  putStrLn "Choose game mode (0 for single player, 1 for multiplayer)"
  gameMode <- getLine
  putStrLn $ "You chose " ++ show (parseGameMode gameMode)
  if parseGameMode gameMode == Singleplayer
    then singleplayerGame
    else multiplayerGame

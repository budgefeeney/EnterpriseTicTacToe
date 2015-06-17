module Main where

import           Control.Monad (forM_)
import           TicTacToe
import           FSharpisms ((<|), (|>))

type ErrorMsg = String

data Command
  = CmdQuit
  | CmdIndex Int
  | InvalidCmd ErrorMsg

parseCommand :: String -> Int -> Command
parseCommand "Q" _ = CmdQuit
parseCommand "q" _ = CmdQuit
parseCommand intStr cmdCount =
    case (reads intStr) :: [(Int, String)] of
    [(index, "")] -> if index >= 0 && index < cmdCount
                      then CmdIndex (index)
                      else InvalidCmd ("Index " ++ intStr ++ " is invalid, acceptable indices are in the range 0.." ++ (show (cmdCount - 1)))
    _ -> InvalidCmd ("The given value \"" ++ intStr ++ "\" is neither an integer nor 'Q'")
parseCommand cmd _ = InvalidCmd ("The given value \"" ++ cmd ++ "\" is neither an integer nor 'Q'")

displayBoard :: GameState -> IO ()
displayBoard = print

numberedMoves :: [a] -> [(Int, a)]
numberedMoves = zip [0..]

printAsNumberedLines :: (Show a) => [a] -> IO ()
printAsNumberedLines ls =
    forM_ (numberedMoves ls) $ \(n,l) ->
        putStrLn <| show n ++ " - " ++ show l

processMove :: (GameState, MoveResult) -> IO (MoveResult)
processMove (game, m@(GameWon p)) = do { displayBoard game; return m }
processMove (game, m@GameTied)    = do { displayBoard game; return m }
processMove (game, m@(PlayerXsTurn (ValidXMoves xms))) =
  displayBoard game
  >> putStrLn "\nPlayer Xs turn"
  >> printAsNumberedLines xms
  >> putStrLn "Q - Quit"
  >> getLine
  >>= \cmd -> case (parseCommand cmd (length xms)) of
                CmdQuit         -> return GameQuit
                CmdIndex(index) -> playerXMove game (xms !! index) |> processMove
                InvalidCmd(err) -> putStrLn err >> processMove game m
processMove (game, m@(PlayerOsTurn (ValidOMoves oms))) =
  displayBoard game
  >> putStrLn "\nPlayer Os turn"
  >> printAsNumberedLines oms
  >> putStrLn "Q - Quit"
  >> getLine
  >>= \cmd -> case (parseCommand cmd (length oms)) of
                CmdQuit         -> return GameQuit
                CmdIndex(index) -> playerOMove game (oms !! index) |> processMove
                InvalidCmd(err) -> putStrLn err >> processMove game m

main::IO()
main =
  let printResult (GameWon p) = putStrLn <| "Game has been won by player " ++ show p
      printResult (GameTied)  = putStrLn <| "Game has been tied"
      printResult (GameQuit)  = putStrLn <| "Game has been quit by user "
      printResult _           = putStrLn <| "Game Ended Weirdly"

      (state, move) = newGame
  in
  processMove (state, move) >>= printResult

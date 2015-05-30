module Main where

import           Control.Monad (forM_)
import           TicTacToe
import           FSharpisms ((<|))

displayBoard :: GameState -> IO ()
displayBoard = print

numberedMoves :: [a] -> [(Int, a)]
numberedMoves = zip [0..]

printAsNumberedLines :: (Show a) => [a] -> IO ()
printAsNumberedLines ls =
    forM_ (numberedMoves ls) $ \(n,l) ->
        putStrLn <| show n ++ " - " ++ show l

processMove :: (GameState, MoveResult) -> IO (MoveResult)
processMove (game, m@(GameWon p)) = return m
processMove (game, m@GameTied)    = return m
processMove (game, m@(PlayerXsTurn (ValidXMoves xms))) =
  do
      displayBoard game

      putStrLn "\nPlayer Os turn"
      printAsNumberedLines oms
      putStrLn "Q - Quit"

      cmd <- getLine
      case cmd of
        "Q" -> return GameQuit
        _   -> case (reads cmd) :: [(Int, String)] of
                  [(intCmd, "")] -> processMove <| playerOMove game (oms !! intCmd)
                  _              -> do
                                      putStrLn <| "Invalid input '" ++ cmd ++ "', try again"
                                      processMove (game, m)
processMove (game, m@(PlayerOsTurn (ValidOMoves oms))) =
  do
      displayBoard game

      putStrLn "\nPlayer Os turn"
      printAsNumberedLines oms
      putStrLn "Q - Quit"

      cmd <- getLine
      case cmd of
        "Q" -> return GameQuit
        _   -> case (reads cmd) :: [(Int, String)] of
                  [(intCmd, "")] -> processMove <| playerOMove game (oms !! intCmd)
                  _              -> do
                                      putStrLn <| "Invalid input '" ++ cmd ++ "', try again"
                                      processMove (game, m)
processMove _ = error "Unsupported"


main::IO()
main =
  let printResult (GameWon p) = putStrLn <| "Game has been won by player " ++ show p
      printResult (GameTied)  = putStrLn <| "Game has been tied"
      printResult (GameQuit)  = putStrLn <| "Game has been quit by user "
      printResult _           = putStrLn <| "Game Ended Weirdly"
  in
  do
    let (state, move) = newGame
    m <- processMove (state, move)
    printResult m

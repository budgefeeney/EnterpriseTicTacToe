module Main where

import           Control.Monad (forM_)
import           TicTacToe

displayBoard :: GameState -> IO ()
displayBoard = print

numberedMoves :: [a] -> [(Int, a)]
numberedMoves = zip [0..]

printAsNumberedLines :: (Show a) => [a] ->  IO ()
printAsNumberedLines ls =
    forM_ (numberedMoves ls) $ \(n,l) ->
        putStrLn <| show n ++ " - " ++ show l

processMove :: (GameState, MoveResult) -> IO (GameState, MoveResult)
processMove (game, m@(GameWon p)) = error "Game is already won, shouldn't be asking for moves"
processMove (game, m@GameTied)    = error "Game is tied, shouldn't be asking for moves"
processMove (game, m@(PlayerXsTurn (ValidXMoves xms))) =
    do
        displayBoard game

        putStrLn "\nPlayer Xs turn"
        printAsNumberedLines xms
        putStrLn "Q - Quit"

        cmd <- getLine
        let intCmd = read cmd :: Int

        putStrLn $ "You chose " ++ show cmd
        return (game, m)

processMove _ = error "Unsupported"

main::IO()
main = do
   let (state, move) = newGame
   (s', m') <- processMove (state, move)
   return ()

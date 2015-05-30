module TicTacToe  (
  MoveResult(..),
  GameState,
  ValidXMoves(..),
  ValidOMoves(..),
  newGame,
  playerXMove,
  playerOMove,
  getCells
  )
where


import           Data.Function (on)
import           Data.List
import           Data.Maybe    (fromMaybe)
import           FSharpisms    ((<.), (<|), (|>))


-- The data model

data HorizPosition = Left | HCenter | Right  deriving (Show, Bounded, Enum, Eq, Ord)
data VertPosition  = Top  | VCenter | Bottom deriving (Show, Bounded, Enum, Eq, Ord)
data CellPosition  = CellPosition {
    posHor  :: HorizPosition,
    posVert :: VertPosition
    } deriving (Show, Eq)

data Player = PlayerX | PlayerO deriving (Show, Eq, Bounded, Enum)

data CellState
    = Played Player
    | Empty
    deriving (Show, Eq)

data Cell = Cell {
     cellPos   :: CellPosition,
     cellState :: CellState
    }

-- The API, part one, the inputs

newtype PlayerXMove = PlayerXMove CellPosition
newtype PlayerOMove = PlayerOMove CellPosition

newtype ValidXMoves = ValidXMoves [PlayerXMove]
newtype ValidOMoves = ValidOMoves [PlayerOMove]

data MoveResult
    = PlayerXsTurn ValidXMoves
    | PlayerOsTurn ValidOMoves
    | GameWon Player
    | GameTied
    | GameQuit

-- The API, part two, the functions

type NewGameFn     g     = (g, MoveResult)
type PlayerXMoveFn g = g -> PlayerXMove -> (g, MoveResult)
type PlayerOMoveFn g = g -> PlayerOMove -> (g, MoveResult)
type GetCellsFn    g = g -> [Cell]

--data NoughtsAndCrossesApi g = NoughtsAndCrossesApi {
--        newGame     :: NewGameFn g,
--        playerXMove :: PlayerXMoveFn g,
--        playerOMove :: PlayerOMoveFn g,
--       getCells    :: GetCellsFn g
--    }

-- --------------------- Implementation ----------------------

-- Get all enum values
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

horizPositions :: [HorizPosition]
horizPositions = allValues

vertPositions :: [VertPosition]
vertPositions  = allValues

-- The actual game state, its accessors, and its Show implementation
data GameState = GameState { getCells :: [Cell] }

getCell :: GameState -> CellPosition -> Cell
getCell state searchPos =
   getCells state
   |> find (\c -> cellPos c == searchPos)
   |> fromMaybe (error ("Failed to find this cell in the board " ++ show searchPos))


updateCell :: Cell -> GameState -> GameState
updateCell newCell gameState =
    let assignNewForOldAtPos newCell' oldCell =
            if cellPos oldCell == cellPos newCell'
            then newCell'
            else oldCell

        newCells = map (assignNewForOldAtPos newCell) (getCells gameState)
    in
        gameState { getCells = newCells }


instance Show Cell where
    show c =
        let showState (Played PlayerX) = "X"
            showState (Played PlayerO) = "O"
            showState Empty            = " "
        in
        showState (cellState c)


instance Show GameState where
    show game =
        let
            onRow v cell   = posVert (cellPos cell) == v
            horizPosCmp    = compare  `on` (posHor <. cellPos)
            row g v        = getCells g
                             |> filter (onRow v)
                             |> sortBy horizPosCmp

            cols v = [show cell | cell <- row game v]
            rows   = ["I" ++ intercalate "I" (cols v) ++ "I" | v <- vertPositions]
            rowSep = "\n" ++ replicate 7 '-' ++ "\n"
        in
        rowSep ++ intercalate rowSep rows ++ rowSep

instance Show PlayerXMove where
    show (PlayerXMove c) = show (posHor c) ++ " " ++ show (posVert c)

instance Show ValidXMoves where
    show (ValidXMoves mvs) = show mvs

instance Show PlayerOMove where
    show (PlayerOMove c) = show (posHor c) ++ " " ++ show (posVert c)

instance Show ValidOMoves where
    show (ValidOMoves mvs) = show mvs

newtype Line = Line [CellPosition] deriving Show

-- This is the F-Sharp original. It's a good deal more readable than mine above
possibleLines :: [Line]
possibleLines =
    let makeHLine v = Line [ CellPosition h v | h <- horizPositions]
        horizLines  = [makeHLine v | v <- vertPositions]

        makeVLine h = Line [ CellPosition h v | v <- vertPositions]
        vertLines   = [makeVLine h | h <- horizPositions]

        forwardDiag = Line $ zipWith CellPosition horizPositions vertPositions
        backDiag    = Line $ zipWith CellPosition (reverse horizPositions) vertPositions
    in
        backDiag : forwardDiag : (horizLines ++ vertLines)

isGameWonBy :: Player -> GameState -> Bool
isGameWonBy player game =
    let
        cellPlayedByPlayer pos =
            cellState (getCell game pos) == Played player

        linePlayedByPlayer (Line cs) =
            all cellPlayedByPlayer cs
    in
        case find linePlayedByPlayer possibleLines of
            Just _ -> True
            _      -> False

isGameTied :: GameState -> Bool
isGameTied game =
    let
        alreadyPlayed cell = cellState cell /= Empty
    in
        all alreadyPlayed (getCells game)

newGame :: (GameState, MoveResult)
newGame =
    let
        cellPoses   = [CellPosition h v | h <- horizPositions, v <- vertPositions]

        cells       = [Cell pos Empty  | pos <- cellPoses]
        validXMoves = ValidXMoves [PlayerXMove pos | pos <- cellPoses]
    in
    (GameState cells, PlayerXsTurn validXMoves)


remainingMovesForPlayer :: (Cell -> p) -> GameState -> [p]
remainingMovesForPlayer playerMoveFn game =
      getCells game
      |> filter (\c -> cellState c == Empty)
      |> map playerMoveFn


playerXMove :: GameState -> PlayerXMove -> (GameState, MoveResult)
playerXMove game (PlayerXMove pos) =
    let
        newCell = Cell { cellPos=pos, cellState=Played PlayerX }
        newGame = updateCell newCell game
    in
    if isGameWonBy PlayerX newGame
    then
        (newGame, GameWon PlayerX)
    else if isGameTied newGame
    then
        (newGame, GameTied)
    else
        let
            playerOMoves = ValidOMoves <| remainingMovesForPlayer (PlayerOMove <. cellPos)  newGame
        in
        (newGame, PlayerOsTurn playerOMoves)


playerOMove :: GameState -> PlayerOMove -> (GameState, MoveResult)
playerOMove game (PlayerOMove pos) =
    let
        updCell = Cell { cellPos=pos, cellState=Played PlayerO }
        updGame = updateCell updCell game
    in
    if isGameWonBy PlayerO updGame
    then
        (updGame, GameWon PlayerO)
    else if isGameTied updGame
    then
        (updGame, GameTied)
    else
        let
            playerXMoves =
                updGame
                |> remainingMovesForPlayer (PlayerXMove <. cellPos)
                |> ValidXMoves
        in
        (updGame, PlayerXsTurn playerXMoves)

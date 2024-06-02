{-# LANGUAGE RecordWildCards #-}

module Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,displayBoard,pieceValue,nextTurn,nextGameState,History,HistoryModifier(..),Moves,emptyBoard,getPicture) where
  import Data.Char (toUpper, digitToInt,isAlphaNum)
  import Data.List (intercalate,find)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Data.Maybe (maybeToList)
  import Utils
  import Graphics.Gloss

  data Side = Black | White deriving(Eq)
  data PieceType = Pawn | Rook | Knight | Bishop | King | Queen deriving(Eq, Enum, Read)
  data Piece = NoPiece | Piece { piecetype :: PieceType, playSide :: Side, firstMove :: Bool} deriving(Eq)

  data ChessGameState = ChessGameState { turn :: Side, board :: Board } 

  instance Show ChessGameState where
    show (ChessGameState Black board) = displayBoard False board
    show (ChessGameState White board) = displayBoard True board

  type Board = [[Piece]]

  class Picturable s where
    getPicture :: s -> Picture

  instance Show PieceType where 
    show Pawn = "pawn"
    show Rook = "rook"
    show Knight = "knight"
    show Bishop = "bishop"
    show King = "king"
    show Queen = "queen"

  getSymbol :: PieceType -> String
  getSymbol Pawn = "P"
  getSymbol Rook = "R"
  getSymbol Knight = "H"
  getSymbol Bishop = "B"
  getSymbol King = "K"
  getSymbol Queen = "Q"

  instance Show Side where
    show Black = "B"
    show White = "W"

  -- Picture representation of every piecetype
  instance Picturable PieceType where 
    getPicture Pawn = circleSolid 25
    getPicture Rook = rectangleSolid 50 50
    getPicture Knight = pictures [
      polygon [(26,30),((-26),30),((-26),10)],
      polygon [(26,30),((-26),(-15)),(26,(-15))]]
    getPicture Bishop = polygon [(0,30),((-26),(-15)),(26,(-15))]
    getPicture King = rectangleSolid 50 20
    getPicture Queen = pictures [
      polygon [((-26),30),((-26),(-15)),(26,(-15))],
      polygon [(0,30),((-26),(-15)),(26,(-15))],
      polygon [(26,30),((-26),(-15)),(26,(-15))]]

  instance Show Piece where
    show NoPiece = "."
    show (Piece piecetype Black _) = "\x1b[1m"++(getSymbol piecetype)++"\x1b[0m"--map toUpper $ getSymbol piecetype
    show (Piece piecetype White _) = "\x1b[30;47m"++(getSymbol piecetype)++"\x1b[39;49m"--getSymbol piecetype

  nextGameState :: ChessGameState -> Board -> ChessGameState
  nextGameState ChessGameState{..} = ChessGameState $ nextTurn turn

  nextTurn :: Side -> Side
  nextTurn Black = White
  nextTurn White = Black

  pieceValue :: PieceType -> Int
  pieceValue Pawn = 1
  pieceValue Rook = 5
  pieceValue Knight = 3
  pieceValue Bishop = 3
  pieceValue Queen = 9
  pieceValue King = 0

  backrowBoard :: [PieceType]
  backrowBoard = [Rook,Knight,Bishop, Queen, King, Bishop, Knight, Rook]

  newBoard :: Board
  newBoard = [[Piece p Black True | p <- backrowBoard] , replicate 8 (Piece Pawn Black True)] ++
    (replicate 4 $ replicate 8 NoPiece) ++
    [replicate 8 (Piece Pawn White True) , [Piece p White True | p <- backrowBoard]]

  emptyBoard :: Board
  emptyBoard = replicate 8 $ replicate 8 NoPiece

  displayBoard :: Show a => Bool -> [[a]] -> String
  displayBoard upRight board = 
    let 
      flip = if upRight then (id) else (reverse) 
    in unlines $ 
    [show i ++ " | " ++ (intercalate " " $ map (show) $ flip row) | (row, i) <- zip (flip board) $ flip [8,7..1]] ++ 
    ["--+" ++ replicate 16 '-'] ++ 
    ["  | " ++ (intercalate " " $ map (:[]) $ flip ['A' .. 'H'])]

  type Moves = [((Piece, Coordinate_t), [Coordinate_t])]
  type History = [(PieceType, Coordinate_t, Coordinate_t, Maybe HistoryModifier)]
  data HistoryModifier = Capture { caputuredPiece :: PieceType} | CastlingL | CastlingR | Promotion { caputuredPiece' :: Maybe PieceType}
  
  data ChessGameWorld 
    = ChessGameMenu {chosenSide :: Maybe Side , chosenMode :: Maybe Bool} 
    | ChessGameOngoing { gameState :: ChessGameState, selectedSquare :: (Maybe Coordinate_t), activeAI :: Bool, history :: History, displayMsg :: String, endReached :: Bool, possibleMoves :: Moves}

  instance Show HistoryModifier where
    show (Capture p) = show p
    show CastlingL = "O-O"
    show CastlingR = "O-O-O"
    show (Promotion (Just p)) = "=Q " ++ show p
    show (Promotion Nothing) = "=Q"


  
  -- -- board example for testing 1
  -- pieceBoard :: Board
  -- pieceBoard = [[NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, (Piece King Black True), NoPiece, NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, (Piece Pawn Black True), (Piece Queen White True)],
  --   [NoPiece,NoPiece,NoPiece,(Piece King White True), NoPiece, NoPiece, NoPiece, NoPiece],
  --   [NoPiece,NoPiece,NoPiece,(Piece Pawn Black True), NoPiece, NoPiece, (Piece Knight White True), NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece]]

  -- -- board example for testing 2
  -- pieceBoard' :: Board
  -- pieceBoard' = [[(Piece Rook Black True),NoPiece, NoPiece, (Piece Queen Black True), NoPiece, (Piece Bishop Black True), (Piece Knight Black True), (Piece Rook Black True)],
  --   [(Piece Pawn Black True),(Piece Pawn Black True), NoPiece, NoPiece, (Piece King Black False), NoPiece, (Piece Pawn Black True), (Piece Pawn Black True)],
  --   [NoPiece,NoPiece, NoPiece, (Piece Pawn Black False), NoPiece, (Piece Pawn Black False), NoPiece, NoPiece],
  --   [NoPiece, (Piece Bishop White True), (Piece Pawn Black False), NoPiece, (Piece Knight White True), NoPiece, (Piece Bishop White True), NoPiece],
  --   [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
  --   [NoPiece,NoPiece, (Piece Pawn White False), (Piece Pawn Black False),NoPiece, NoPiece, NoPiece, NoPiece],
  --   [(Piece Pawn White True), (Piece Pawn White True), NoPiece, NoPiece, NoPiece, (Piece Pawn White True), (Piece Pawn White True), (Piece Pawn White True)],
  --   [(Piece Rook White True),(Piece Knight White True), NoPiece, (Piece Bishop Black True), (Piece Rook White True), NoPiece, (Piece King White True), NoPiece]]
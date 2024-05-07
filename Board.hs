{-# LANGUAGE RecordWildCards #-}

module Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),newBoard,displayBoard,pieceBoard,pieceValue,nextTurn,nextGameState) where
  import Data.Char (isAlphaNum)
  import Data.Char (toUpper, digitToInt)
  import Data.List (intercalate)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)

  data Side = Black | White deriving(Eq, Show)
  data PieceType = Pawn | Tower | Horse | Bishop | King | Queen deriving(Eq, Enum)
  data Piece = NoPiece | Piece { piecetype :: PieceType, playSide :: Side, firstMove :: Bool} deriving(Eq)

  data ChessGameState = ChessGameState { moveCount :: Int, turn :: Side, board :: Board } 

  instance Show ChessGameState where
    show (ChessGameState _ Black board) = displayBoard False board
    show (ChessGameState _ White board) = displayBoard True board

  type Board = [[Piece]]

  instance Show PieceType where 
    show Pawn = "p"
    show Tower = "t"
    show Horse = "h"
    show Bishop = "b"
    show King = "k"
    show Queen = "q"

  instance Show Piece where
    show NoPiece = "."
    show (Piece piecetype Black _) = map toUpper $ show piecetype
    show (Piece piecetype White _) = show piecetype

  nextGameState :: ChessGameState -> Board -> ChessGameState
  nextGameState ChessGameState{..} = ChessGameState (moveCount + 1) $ nextTurn turn

  nextTurn :: Side -> Side
  nextTurn Black = White
  nextTurn White = Black

  pieceValue :: PieceType -> Int
  pieceValue Pawn = 1
  pieceValue Tower = 5
  pieceValue Horse = 3
  pieceValue Bishop = 3
  pieceValue Queen = 9
  pieceValue King = 0

  backrowBoard :: [PieceType]
  backrowBoard = [Tower,Horse,Bishop, Queen, King, Bishop, Horse, Tower]

  newBoard :: Board
  newBoard = [[Piece p Black True | p <- backrowBoard] , replicate 8 (Piece Pawn Black True)] ++
    (replicate 4 $ replicate 8 NoPiece) ++
    [replicate 8 (Piece Pawn White True) , [Piece p White True | p <- backrowBoard]]

  pieceBoard :: Board
  pieceBoard = (replicate 6 $ replicate 8 NoPiece) ++
    [[(Piece Pawn Black True),NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, (Piece Tower White True)],
    [NoPiece, NoPiece, NoPiece, NoPiece, (Piece King Black True), NoPiece, NoPiece, NoPiece]]

  displayBoard :: Show a => Bool -> [[a]] -> String
  displayBoard upRight board = 
    let flip = if upRight then (id) else (reverse) in
    unlines $ 
    [show i ++ " | " ++ (intercalate " " $ map (show) $ flip row) | (row, i) <- zip (flip board) $ flip [1..8]] ++ 
    ["--+" ++ replicate 16 '-'] ++ 
    ["  | " ++ (intercalate " " $ take 8 $ map (:[]) $ flip ['A' .. 'H'])]
--    putStr $ 
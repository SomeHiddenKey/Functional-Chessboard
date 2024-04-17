module Board(Side,PieceType(..),Piece(..),Board,newBoard,displayBoard) where
  import Data.Char (isAlphaNum)
  import Data.Char (toUpper, digitToInt)
  import Data.List (intercalate)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)

  data Side = Black | White deriving(Eq)
  data PieceType = Pawn | Tower | Horse | Bishop | King | Queen deriving(Eq)
  data Piece = NoPiece | Piece { piecetype :: PieceType, side :: Side, firstMove :: Bool} 

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

  backrowBoard :: [PieceType]
  backrowBoard = [Tower,Horse,Bishop, King, Queen,Bishop, Horse, Tower]

  newBoard :: Board
  newBoard = [[Piece p Black True | p <- backrowBoard] , replicate 8 (Piece Pawn Black True)] ++
    (replicate 4 $ replicate 8 NoPiece) ++
    [replicate 8 (Piece Pawn White True) , [Piece p White True | p <- backrowBoard]]

  displayBoard upRight board = 
    let flip = if upRight then (id) else (reverse) in
    putStr $ unlines $ 
    [show i ++ " | " ++ (intercalate " " $ map (show) $ flip row) | (row, i) <- zip (flip board) $ flip [1..8]] ++ 
    ["--+" ++ replicate 16 '-'] ++ 
    ["  | " ++ (intercalate " " $ take 8 $ map (:[]) $ flip ['A' .. 'G'])]
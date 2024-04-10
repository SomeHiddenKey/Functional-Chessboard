import Data.Char (isAlphaNum)
import Data.Char (toUpper)
import Data.List (intercalate)
import Control.Monad (unless)
import Text.Read (readEither)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }
data ChessGameState = ChessGameState { target :: Int } 
data Side = Black | White deriving(Eq,Ord)
data PieceType = Pawn | Tower | Horse | Bishop | King | Queen deriving(Eq)
data Piece = NoPiece | Piece { piecetype :: PieceType, side :: Side, firstMove :: Bool} 

instance Show PieceType where 
  show Pawn = "p"
  show Tower = "t"
  show Horse = "h"
  show Bishop = "b"
  show King = "k"
  show Queen = "q"

instance Show Piece where
  show NoPiece = "."
  show (Piece piecetype side _) = if (side==Black) then map toUpper l else l where l = show piecetype

type Board = [[Piece]]

backrowBoard :: [PieceType]
backrowBoard = [Tower,Horse,Bishop, King, Queen,Bishop, Horse, Tower]

newBoard :: Board
newBoard = [[Piece p Black True | p <- backrowBoard] , replicate 8 (Piece Pawn Black True)] ++
  (replicate 4 $ replicate 8 NoPiece) ++
  [replicate 8 (Piece Pawn White True) , [Piece p White True | p <- backrowBoard]]

displayBoard board = putStr $ unlines $ 
  [show i ++ " | " ++ (intercalate " " $ map (show) row) | (i,row) <- zip [1..] board] ++ 
  ["--+" ++ replicate 16 '-'] ++ 
  ["  | " ++ (intercalate " " $ take 8 $ map (:[]) ['A' ..])]

-- instance GameState GuessingGameState where
--     nextState GuessingGameState{..} input = GuessingGameState target . Guess <$> readEither input -- readEither produces a Left with an error message if parsing failed 
--     isFinalState GuessingGameState{lastGuess = Guess last, ..} = last == target
--     isFinalState _ = False 

-- instance TerminalGame GuessingGameState GuessingGameConfig where
--     initialState GuessingGameConfig{..}
--         | low < high = let (num, _) = randomR (low, high) (mkStdGen seed) in Right (GuessingGameState num NoGuess)
--         | otherwise = Left "Invalid configuration: low should be smaller than high"

-- instance Show GuessingGameState where
--     show GuessingGameState{..} = report lastGuess
--         where report NoGuess = "Make a guess"
--               report (Guess last)
--                 | target < last = "Aim lower!"
--                 | target > last = "Aim higher!"
--                 | otherwise = "Congratulations!" -- target == last, you won!
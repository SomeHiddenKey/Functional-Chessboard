{-# LANGUAGE FunctionalDependencies #-}

import Data.Char (isAlphaNum)
import Data.Char (toUpper, digitToInt)
import Data.List (intercalate)
import Control.Monad (unless)
import Text.Read (readEither)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

import Board
import BoardMovement

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }
data ChessGameState = ChessGameState { board :: Board, moveCount :: Int, turn :: Side} 

inRange :: (Int,Int) -> Int -> Bool
inRange (min, max) n = n < min || n > max

getCor :: [Char] -> Either ErrorMsg (Coordinate_t, Coordinate_t)
getCor input@[a,b,c,d] = 
  if all (inRange (1,8)) [_a-9,_b,_c-9,_d] then Right ((Coordinate (_a-9) _b), (Coordinate (_c-9) _d)) else Left "invalid coordinate"
  where [_a,_b,_c,_d] = map digitToInt input

type Command = (Coordinate_t, Coordinate_t)
type ErrorMsg = String 

class GameState s where
  nextState :: s -> Command -> Either ErrorMsg s
  isFinalState :: s -> Bool

class GameState s => TerminalGame s c | c -> s where
  initialState :: c -> Either ErrorMsg s

promptForInput :: IO String
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

-- runGame :: (Show s, TerminalGame s c) => c -> IO ()
-- runGame = either error loop . initialState
--   where loop st = do 
--                     print st
--                     unless (isFinalState st) $ do
--                             cmd <- promptForInput
--                             let nxt = nextState st cmd
--                             either ((>> loop st) . putStrLn) loop nxt

-- checkMoveInDirection :: Board -> Coordinate -> Coordinate
-- checkMoveInDirection board (Coordinate start_x start_y) (Coordinate end_x end_y) = let dirsteps = 



-- instance GameState ChessGameState where
--     nextState ChessGameState{..} input = <$> getCor <$> readEither input
--     isFinalState GuessingGameState{lastGuess = Guess last, ..} = last == target
--     isFinalState _ = False  

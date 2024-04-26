{-# LANGUAGE FunctionalDependencies #-}

import Data.Char (isAlphaNum)
import Data.Char (toUpper, ord)
import Data.List (intercalate)
import Control.Monad (unless)
import Text.Read (readEither)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

import Board
import BoardMovement

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }

inRange :: (Int,Int) -> Int -> Bool
inRange (min, max) n = n >= min && n < max

getCor :: [Char] -> Either ErrorMsg (Coordinate_t, Coordinate_t)
getCor input@[a,b,c,d] = 
  if all (inRange (0,8)) [_a-65,_b-49,_c-65,_d-49] then Right ((Coordinate (_a-65) (_b-49)), (Coordinate (_c-65) (_d-49))) else Left "invalid coordinate"
  where [_a,_b,_c,_d] = map ord input
getCor _ = Left "invalid coordinate"

type Command = (Coordinate_t, Coordinate_t)
type ErrorMsg = String 

class GameState s where
  nextState :: s -> Command -> Either ErrorMsg s
  isFinalState :: s -> Bool

class GameState s => TerminalGame s c | c -> s where
  initialState :: c -> Either ErrorMsg s

promptForInput :: IO String
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

-- runGame (Right (ChessGameState 0 White newBoard))

-- runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame = either error loop--((ChessGameState 0 Black newBoard))
  where loop st = do 
                    print st
                    cmd <- promptForInput
                    let nxt = (getCor cmd) >>= (checkMove st)
                    either ((>> loop st) . putStrLn) loop nxt

-- instance GameState ChessGameState where
--     nextState ChessGameState{..} input = <$> getCor <$> readEither input
--     isFinalState GuessingGameState{lastGuess = Guess last, ..} = last == target
--     isFinalState _ = False  

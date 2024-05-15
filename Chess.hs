{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (isAlphaNum)
import Data.Char (toUpper, ord)
import Data.List (intercalate)
import Control.Monad (unless)
import Text.Read (readEither)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Either (fromRight)

import Board
import BoardMovement
import Persistent
import Utils (Coordinate(..),toFst, Coordinate_t,inRange)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }

getCor :: [Char] -> Either ErrorMsg (Coordinate Int, Coordinate Int)
getCor input@[a,b,c,d] = 
  if all (inRange (0,8)) [_a-65,_b-49,_c-65,_d-49] then Right ((Coordinate (_a-65) (_b-49)), (Coordinate (_c-65) (_d-49))) else Left "invalid coordinate"
  where [_a,_b,_c,_d] = map ord input
getCor _ = Left "invalid coordinate"

type Command = (Coordinate Int, Coordinate Int)
type ErrorMsg = String 

class GameState s where
  nextState :: s -> Command -> Either ErrorMsg s
  isFinalState :: s -> Bool

class GameState s => TerminalGame s c | c -> s where
  initialState :: c -> Either ErrorMsg s

promptForInput :: IO String
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

-- runGame (Right (ChessGameState 0 White newBoard))
-- runNewGame = runGame $ Right (Coordinate 4 7, ChessGameState 0 White newBoard)

-- runGame :: (Show s, TerminalGame s c) => c -> IO ()
-- runGame = either error loop--((ChessGameState 0 Black newBoard))
--   where loop (kc, st) = do 
--                     print st
--                     cmd <- promptForInput
--                     let nxt = (getCor cmd) >>= (checkMove st) >>= (checkMoveLegality kc)
--                     either ((>> loop (kc, st)) . putStrLn) loop nxt

-- runPresetGame =
--   foldl (\(kc, st) cmd -> 
--        fromRight (kc, st) ((getCor cmd) >>= (checkMove st) >>= (checkMoveLegality kc))
--     ) (Coordinate 4 7, ChessGameState 0 White newBoard) ["E7E5", "E2E4", "F8C5", "B1C3", "D8H4", "G1F3"] -- H4F2

-- loadPresetGame :: Foldable t => t [Char] -> (Coordinate_t, ChessGameState)
-- loadPresetGame cmds = 
--   getKingCoordinate `toFst` foldr (\cmd st -> fromRight st $ (getCor cmd) >>= (checkMove st)) (ChessGameState 0 White newBoard) cmds


-- loadPresetGame :: Foldable t => t [Char] -> IO b
-- loadPresetGame cmds = 
--   runGame $ Right $ getKingCoordinate `toFst` foldr (\cmd st -> fromRight st $ (getCor cmd) >>= (checkMove st)) (ChessGameState 0 White newBoard) cmds

startGameFromMenu :: IO ()
startGameFromMenu = playIO FullScreen (greyN 0.3) 20 ChessGameMenu (return . displayWorld) changeWorld (\tick world -> return world)

changeWorld :: Event -> ChessGameWorld -> IO ChessGameWorld
changeWorld (EventKey (MouseButton LeftButton) Down _ c) = changeWorldBoard $ toBoardCoordinate c
changeWorld _ = return

changeWorldBoard :: Coordinate_t -> ChessGameWorld -> IO ChessGameWorld

changeWorldBoard (Coordinate 2 4) ChessGameMenu = return $ ChessGameOngoing newCgs Nothing [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White pieceBoard
changeWorldBoard (Coordinate 5 4) ChessGameMenu = return $ ChessGameOngoing newCgs Nothing [] "Black to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState Black newBoard

-- changeWorldBoard (Coordinate 10 7) cgw@ChessGameOngoing{} = serializeGame cgw

changeWorldBoard (Coordinate 10 7) cgw@ChessGameOngoing{} = return $ undo cgw

changeWorldBoard (Coordinate x y) (ChessGameOngoing gs Nothing hs _ False pm)
  | x < 0 = return $ ChessGameOngoing gs Nothing hs "" False pm
  | y < 0 = return $ ChessGameOngoing gs Nothing hs "" False pm
  | x > 7 = return $ ChessGameOngoing gs Nothing hs "" False pm
  | y > 7 = return $ ChessGameOngoing gs Nothing hs "" False pm

changeWorldBoard c (ChessGameOngoing gs Nothing hs _ False pm) = return $ ChessGameOngoing gs (Just c) hs "" False pm

changeWorldBoard c cgo@(ChessGameOngoing gs (Just sq) hs _ False _) = return $ either (const $ resetSelection cgo) (testsdd c cgo) (checkMove gs (sq, c))

changeWorldBoard _ world = return world

resetSelection :: ChessGameWorld -> ChessGameWorld
resetSelection ChessGameOngoing{..} = ChessGameOngoing gameState Nothing history "" endReached possibleMoves

-- startGameFromLoad :: ChessGameState -> History -> IO ()
-- startGameFromLoad cgs@(ChessGameState side board) history = play FullScreen (greyN 0.3) 5 (ChessGameOngoing cgs Nothing history "" False $ getMovesForSide cgs) displayWorld changeWorld (\tick world -> world)
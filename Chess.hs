{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper, ord,isAlphaNum) 
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
import Text.Parsec.String
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.IO.Game

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }

getCor :: [Char] -> Either ErrorMsg (Coordinate Int, Coordinate Int)
getCor input@[a,b,c,d] = 
  if all (inRange (0,8)) [_a-65,_b-49,_c-65,_d-49] then Right ((Coordinate (_a-65) (56-_b)), (Coordinate (_c-65) (56-_d))) else Left "invalid coordinate"
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

runNewGamePvP = runGamePvP $ Right $ ChessGameOngoing newCgs Nothing False [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White newBoard

runNewGamePvE Black = runGamePvE $ Right $ (,) False $ ChessGameOngoing newCgs Nothing True [] "" False $ getMovesForSide newCgs where newCgs = ChessGameState Black newBoard

runNewGamePvE White = runGamePvE $ Right $ (,) True $ ChessGameOngoing newCgs Nothing True [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White newBoard

runGamePvP :: Either [Char] ChessGameWorld -> IO ()
runGamePvP = either error loop
  where loop cgo = do 
                    print $ gameState cgo
                    cmd <- promptForInput
                    let cor = (getCor cmd)
                    let nxt = (\c -> either (const $ cgo{displayMsg="", selectedSquare=Nothing}) (flip testsdd cgo{selectedSquare=Just $ fst c} $ snd c) (flip checkMove c $ gameState cgo)) <$> cor
                    either ((>> loop cgo) . putStrLn) continue nxt
        continue cgo@ChessGameOngoing{endReached=True} = do
            putStrLn $ displayMsg cgo
            print $ gameState cgo
        continue cgo@ChessGameOngoing{endReached=False} = (>> loop cgo) . putStrLn $ displayMsg cgo

runGamePvE :: Either [Char] (Bool,ChessGameWorld) -> IO ()
runGamePvE = either error loop
  where loop (True,cgo) = do 
                    print $ gameState cgo
                    cmd <- promptForInput
                    let cor = (getCor cmd)
                    let nxt = (\c -> either (const $ cgo{displayMsg="", selectedSquare=Nothing}) (flip testsdd cgo{selectedSquare=Just $ fst c} $ snd c) (flip checkMove c $ gameState cgo)) <$> cor
                    either ((>> loop (True,cgo)) . putStrLn) (continue True cgo) nxt
        loop (False,cgo) = do 
                    print $ gameState cgo
                    continue False cgo $ promptAImove cgo
        continue _ _ cgo@ChessGameOngoing{endReached=True} = do
            putStrLn $ displayMsg cgo
            print $ gameState cgo
        continue player old_cgo new_cgo@ChessGameOngoing{endReached=False}
          | (turn . gameState $ old_cgo) == (turn . gameState $ new_cgo) = (>> loop (True,new_cgo)) . putStrLn $ displayMsg new_cgo
          | otherwise = (>> loop (not player,new_cgo)) . putStrLn $ displayMsg new_cgo

startGameFromLoad :: ChessGameWorld -> IO ()
startGameFromLoad cgw = playIO FullScreen (greyN 0.5) 20 cgw (return . displayWorld) changeWorld (\tick world -> return world)

startGameFromMenu :: IO ()
startGameFromMenu = startGameFromLoad $ ChessGameMenu Nothing Nothing

changeWorld :: Event -> ChessGameWorld -> IO ChessGameWorld
changeWorld (EventKey (MouseButton LeftButton) Down _ c) = changeWorldBoard $ toBoardCoordinate c
changeWorld _ = return

changeWorldBoard :: Coordinate_t -> ChessGameWorld -> IO ChessGameWorld

changeWorldBoard (Coordinate 2 4) cgm@ChessGameMenu{} = return cgm{chosenSide=Just White}
changeWorldBoard (Coordinate 5 4) cgm@ChessGameMenu{} = return cgm{chosenSide=Just Black}
changeWorldBoard (Coordinate 2 2) cgm@ChessGameMenu{} = return cgm{chosenMode=Just True}
changeWorldBoard (Coordinate 5 2) cgm@ChessGameMenu{} = return cgm{chosenMode=Just False}

changeWorldBoard (Coordinate _ 7) cgm@ChessGameMenu{chosenSide=Just White,chosenMode=Just True} = return $ ChessGameOngoing newCgs Nothing True [] "White to begin" False $ getMovesForSide newCgs 
  where newCgs = ChessGameState White newBoard
changeWorldBoard (Coordinate _ 7) cgm@ChessGameMenu{chosenSide=Just Black,chosenMode=Just True} = return $ promptAImove $ ChessGameOngoing newCgs Nothing True [] "" False $ getMovesForSide newCgs 
  where newCgs = ChessGameState White newBoard
changeWorldBoard (Coordinate _ 7) cgm@ChessGameMenu{chosenMode=Just False} = return $ ChessGameOngoing newCgs Nothing False [] "White to begin" False $ getMovesForSide newCgs 
  where newCgs = ChessGameState White newBoard

changeWorldBoard (Coordinate 10 6) cgw@ChessGameOngoing{} = serializeGame cgw

changeWorldBoard (Coordinate 10 7) cgw@ChessGameOngoing{} = return $ undo cgw

changeWorldBoard (Coordinate x y) cgo@ChessGameOngoing{endReached=False}
  | x < 0 = return $ cgo{selectedSquare=Nothing, displayMsg=""}
  | y < 0 = return $ cgo{selectedSquare=Nothing, displayMsg=""}
  | x > 7 = return $ cgo{selectedSquare=Nothing, displayMsg=""}
  | y > 7 = return $ cgo{selectedSquare=Nothing, displayMsg=""}

changeWorldBoard c cgo@ChessGameOngoing{endReached=False,selectedSquare=Nothing} = return $ cgo{selectedSquare=Just c}

changeWorldBoard c cgo@ChessGameOngoing{endReached=False,selectedSquare=Just sq,activeAI=False,gameState} = return $ either (\msg -> cgo{displayMsg=msg, selectedSquare=Nothing}) (testsdd c cgo) (checkMove gameState (sq, c))
changeWorldBoard c cgo@ChessGameOngoing{endReached=False,selectedSquare=Just sq,gameState} = return $ either (\msg -> cgo{displayMsg=msg, selectedSquare=Nothing}) (promptAImove . testsdd c cgo) (checkMove gameState (sq, c))

changeWorldBoard _ world = return world

unser = do{ result <- parseFromFile unserializeGame "test.txt"
            ; case result of
                Left err  -> print err
                Right xs  -> startGameFromLoad xs
            }

            
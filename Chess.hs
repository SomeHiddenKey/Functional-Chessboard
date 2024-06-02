{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (toUpper, ord,isAlphaNum) 
import Data.List (intercalate,find)
import Control.Monad (unless)
import Text.Read (readEither)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Either (fromRight)

import Board
import BoardMovement
import BoardLogic
import Persistent
import Interface
import Graphics.Gloss.Interface.IO.Interact
import Utils (Coordinate(..),toFst, Coordinate_t,inRange)

data ChessConfig = ChessConfig { seed :: Int, low :: Int, high :: Int }
type ErrorMsg = String 

getCor :: [Char] -> Either ErrorMsg (Coordinate Int, Coordinate Int)
getCor input@[a,b,c,d] = 
  if all (inRange (0,8)) [_a-65,_b-49,_c-65,_d-49] then Right ((Coordinate (_a-65) (56-_b)), (Coordinate (_c-65) (56-_d))) else Left "invalid coordinate"
  where [_a,_b,_c,_d] = map ord input
getCor _ = Left "invalid coordinate"

promptForInput :: IO String
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

runNewGamePvP :: IO ()
runNewGamePvP = runGamePvP $ Right $ ChessGameOngoing newCgs Nothing False [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White newBoard

runNewGamePvE :: Side -> IO ()
runNewGamePvE Black = runGamePvE $ Right $ (,) False $ ChessGameOngoing newCgs Nothing True [] "" False $ getMovesForSide newCgs where newCgs = ChessGameState Black newBoard
runNewGamePvE White = runGamePvE $ Right $ (,) True $ ChessGameOngoing newCgs Nothing True [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White newBoard

runGamePvP :: Either [Char] ChessGameWorld -> IO ()
runGamePvP = either error loop
  where loop cgo = do 
                    print $ gameState cgo
                    cmd <- promptForInput
                    let cor = (getCor cmd)
                    let nxt = (\c -> either (const $ cgo{displayMsg="", selectedSquare=Nothing}) (flip testMoveValidity cgo{selectedSquare=Just $ fst c} $ snd c) (flip checkMove c $ gameState cgo)) <$> cor
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
                    let nxt = (\c -> either (const $ cgo{displayMsg="", selectedSquare=Nothing}) (flip testMoveValidity cgo{selectedSquare=Just $ fst c} $ snd c) (flip checkMove c $ gameState cgo)) <$> cor
                    either ((>> loop (True,cgo)) . putStrLn) (continue True cgo) nxt
        loop (False,cgo) = do 
                    print $ gameState cgo
                    continue False cgo $ promptAImoveUncached cgo
        continue _ _ cgo@ChessGameOngoing{endReached=True} = do
            putStrLn $ displayMsg cgo
            print $ gameState cgo
        continue player old_cgo new_cgo@ChessGameOngoing{endReached=False}
          | (turn . gameState $ old_cgo) == (turn . gameState $ new_cgo) = (>> loop (True,new_cgo)) . putStrLn $ displayMsg new_cgo
          | otherwise = (>> loop (not player,new_cgo)) . putStrLn $ displayMsg new_cgo

-- runGamePvE :: Either [Char] (Bool,ChessGameWorld,MovesTree) -> IO ()
-- runGamePvE = either error loop
--   where loop (True,cgo,tree) = do 
--                     print $ gameState cgo
--                     cmd <- promptForInput
--                     let cor = (getCor cmd)
--                     let nxt = (\c -> either (const $ cgo{displayMsg="", selectedSquare=Nothing}) (flip testMoveValidity cgo{selectedSquare=Just $ fst c} $ snd c) (flip checkMove c $ gameState cgo)) <$> cor
--                     either ((>> loop (True,cgo,tree)) . putStrLn) (continue True cgo $ updateTree cor tree) nxt
--         loop (False,cgo,tree) = do 
--                     print $ gameState cgo
--                     uncurry (continue False cgo) $ promptAImoveCached cgo tree
--         continue _ _ _ cgo@ChessGameOngoing{endReached=True} = do
--             putStrLn $ displayMsg cgo
--             print $ gameState cgo
--         continue player old_cgo tree new_cgo@ChessGameOngoing{endReached=False} 
--           | (turn . gameState $ old_cgo) == (turn . gameState $ new_cgo) = (>> loop (True,new_cgo,tree)) . putStrLn $ displayMsg new_cgo
--           | otherwise = (>> loop (not player,new_cgo,tree)) . putStrLn $ displayMsg new_cgo

-- updateTree :: (Either a Command) -> MovesTree -> MovesTree
-- updateTree (Right cmd) tree@Node{nextMoves} = maybe tree snd $ find ((== cmd) . fst) nextMoves
-- updateTree _ tree = tree

-- main "" = startGameFromMenu
-- main path = do{ result <- parseFromFile unserializeGame path
--             ; case result of
--                 Left err  -> print err
--                 Right xs  -> startGameFromLoad xs
--             }          
{-# LANGUAGE RecordWildCards #-}

module Interface(startGameFromLoad, startGameFromMenu) where
  import Data.Char (toUpper, digitToInt,isAlphaNum)
  import Data.List (intercalate,find)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Data.Maybe (maybeToList)
  import Utils
  import Board
  import BoardLogic
  import Persistent
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game

  translateAsBoard :: Float -> Float -> Picture -> Picture
  translateAsBoard x y = translate (x*100 - 350) (357 - y*100)

  toBoardCoordinate :: (Float,Float) -> Coordinate_t 
  toBoardCoordinate (x,y) = Coordinate (round $ (x + 350)/100)  (round $ ((7-y) + 350)/100 )

  displayMenu :: ChessGameWorld -> Picture
  displayMenu cgw = pictures $ (++) [
    color white $ translateAsBoard (-2) 0 $ text "Chess game menu",
    color white $ translateAsBoard 1.6 2.45 $ scale 0.4 0.5 $ text "AI",
    color white $ translateAsBoard 4.6 2.45 $ scale 0.4 0.5 $ text "PVP"] $
    [selectionSide,selectionMode,selectionStart,selectionSideRect] <*> [cgw]
    where 
      selectionSide ChessGameMenu{chosenSide=Just White,chosenMode=Just True} = color white $ translateAsBoard 2 4 $ rectangleWire 120 120
      selectionSide ChessGameMenu{chosenSide=Just Black,chosenMode=Just True} = color white $ translateAsBoard 5 4 $ rectangleWire 120 120
      selectionSide _ = Blank
      selectionMode ChessGameMenu{chosenMode=Just True} = color white $ translateAsBoard 2 2 $ rectangleWire 120 120
      selectionMode ChessGameMenu{chosenMode=Just False} = color white $ translateAsBoard 5 2 $ rectangleWire 120 120
      selectionMode _ = Blank
      selectionStart ChessGameMenu{chosenSide=Just _,chosenMode=Just True} = color white $ translateAsBoard 2 7 $ text "Start"
      selectionStart ChessGameMenu{chosenMode=Just False} = color white $ translateAsBoard 2 7 $ text "Start"
      selectionStart _ = Blank
      selectionSideRect ChessGameMenu{chosenMode=Just True} = pictures [color white $ translateAsBoard 2 4 $ rectangleSolid 100 100, color black $ translateAsBoard 5 4 $ rectangleSolid 100 100]
      selectionSideRect _ = Blank

  displayWorld :: ChessGameWorld -> Picture
  displayWorld cgm@ChessGameMenu{} = displayMenu cgm
  displayWorld cgo@ChessGameOngoing{gameState,displayMsg} = displayBoardWindow gameState displayMsg $ selectedRedSquaresconcat cgo
    where   selectedRedSquaresconcat ChessGameOngoing{selectedSquare=Just c,possibleMoves} = concat $ snd <$> find ((== c) . snd . fst) possibleMoves
            selectedRedSquaresconcat ChessGameOngoing{selectedSquare=Nothing} = []

  displayBoardWindow :: ChessGameState -> String -> [Coordinate_t] -> Picture
  displayBoardWindow (ChessGameState _ board) msg possibleMoves = --add flip afterwards upon turn
    pictures $ 
    [translate (x - 350) (-450) $ scale 0.5 0.5 $ text $ s:[] | (x, s) <- zip [0,100..800] ['A' .. 'H']] ++
    [translate (-450) (y - 350) $ scale 0.5 0.5 $ text $ show s | (y, s) <- zip [0,100..800] [1..8]] ++
    [color clr $ translateAsBoard x y $ rectangleSolid 100 100 | 
      x <- [0..7], y <- [0..7], let clr = colorConverter x y] ++
    [color (if playSide p == White then white else black) $ translateAsBoard x y $ getPicture $ piecetype p | 
      (y, r) <- zip [0..7] board, (x, p) <- zip [0..7] r, p /= NoPiece] ++
    [translateAsBoard 10 7.4 $ text "<", translateAsBoard 10 6.3 $ scale 0.3 0.3 $ text "save",translateAsBoard 0 (-1) $ scale 0.3 0.3 $ text msg]
    where 
      colorConverter x y
        | uncurry Coordinate (round x, round y) `elem` possibleMoves = (if even $ round (x + y) then id else dark) green
        | even $ round (x + y) = light orange
        | otherwise = dark orange

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

  changeWorld :: Event -> ChessGameWorld -> IO ChessGameWorld
  changeWorld (EventKey (MouseButton LeftButton) Down _ c) = changeWorldBoard $ toBoardCoordinate c
  changeWorld _ = return

  startGameFromLoad :: ChessGameWorld -> IO ()
  startGameFromLoad cgw = playIO FullScreen (greyN 0.5) 20 cgw (return . displayWorld) changeWorld (\tick world -> return world)

  startGameFromMenu :: IO ()
  startGameFromMenu = startGameFromLoad $ ChessGameMenu Nothing Nothing
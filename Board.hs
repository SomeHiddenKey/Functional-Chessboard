{-# LANGUAGE RecordWildCards #-}

module Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,displayBoard,pieceBoard,pieceValue,nextTurn,nextGameState,displayBoardWindow,toBoardCoordinate,History,HistoryModifier(..),Moves,displayWorld,pieceBoard',emptyBoard) where
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

  pieceBoard :: Board
  pieceBoard = [[NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, (Piece King Black True), NoPiece, NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, (Piece Pawn Black True), (Piece Queen White True)],
    [NoPiece,NoPiece,NoPiece,(Piece King White True), NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece,NoPiece,NoPiece,(Piece Pawn Black True), NoPiece, NoPiece, (Piece Knight White True), NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece]]

  pieceBoard' :: Board
  pieceBoard' = [[(Piece Rook Black True),NoPiece, NoPiece, (Piece Queen Black True), NoPiece, (Piece Bishop Black True), (Piece Knight Black True), (Piece Rook Black True)],
    [(Piece Pawn Black True),(Piece Pawn Black True), NoPiece, NoPiece, (Piece King Black False), NoPiece, (Piece Pawn Black True), (Piece Pawn Black True)],
    [NoPiece,NoPiece, NoPiece, (Piece Pawn Black False), NoPiece, (Piece Pawn Black False), NoPiece, NoPiece],
    [NoPiece, (Piece Bishop White True), (Piece Pawn Black False), NoPiece, (Piece Knight White True), NoPiece, (Piece Bishop White True), NoPiece],
    [NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece,NoPiece, (Piece Pawn White False), (Piece Pawn Black False),NoPiece, NoPiece, NoPiece, NoPiece],
    [(Piece Pawn White True), (Piece Pawn White True), NoPiece, NoPiece, NoPiece, (Piece Pawn White True), (Piece Pawn White True), (Piece Pawn White True)],
    [(Piece Rook White True),(Piece Knight White True), NoPiece, (Piece Bishop Black True), (Piece Rook White True), NoPiece, (Piece King White True), NoPiece]]

  displayBoard :: Show a => Bool -> [[a]] -> String
  displayBoard upRight board = 
    let 
      flip = if upRight then (id) else (reverse) 
    in unlines $ 
    [show i ++ " | " ++ (intercalate " " $ map (show) $ flip row) | (row, i) <- zip (flip board) $ flip [8,7..1]] ++ 
    ["--+" ++ replicate 16 '-'] ++ 
    ["  | " ++ (intercalate " " $ map (:[]) $ flip ['A' .. 'H'])]

  translateAsBoard :: Float -> Float -> Picture -> Picture
  translateAsBoard x y = translate (x*100 - 350) (357 - y*100)

  toBoardCoordinate :: (Float,Float) -> Coordinate_t 
  toBoardCoordinate (x,y) = Coordinate (round $ (x + 350)/100)  (round $ ((7-y) + 350)/100 )

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

  type Moves = [((Piece, Coordinate_t), [Coordinate_t])]
  type History = [(PieceType, Coordinate_t, Coordinate_t, Maybe HistoryModifier)]
  data HistoryModifier = Capture { caputuredPiece :: PieceType} | CastlingL | CastlingR | Promotion { caputuredPiece' :: Maybe PieceType}
  data ChessGameWorld = ChessGameMenu {chosenSide :: Maybe Side , chosenMode :: Maybe Bool} | ChessGameOngoing { gameState :: ChessGameState, selectedSquare :: (Maybe Coordinate_t), activeAI :: Bool, history :: History, displayMsg :: String, endReached :: Bool, possibleMoves :: Moves}

  displayWorld :: ChessGameWorld -> Picture
  displayWorld cgm@ChessGameMenu{} = displayMenu cgm
  displayWorld cgo@ChessGameOngoing{gameState,displayMsg} = displayBoardWindow gameState displayMsg $ selectedRedSquaresconcat cgo
    where   selectedRedSquaresconcat ChessGameOngoing{selectedSquare=Just c,possibleMoves} = concat $ snd <$> find ((== c) . snd . fst) possibleMoves
            selectedRedSquaresconcat ChessGameOngoing{selectedSquare=Nothing} = []

  instance Show HistoryModifier where
    show (Capture p) = show p
    show CastlingL = "O-O"
    show CastlingR = "O-O-O"
    show (Promotion (Just p)) = "=Q " ++ show p
    show (Promotion Nothing) = "=Q"
{-# LANGUAGE RecordWildCards #-}

module Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,displayBoard,pieceBoard,pieceValue,nextTurn,nextGameState,displayBoardWindow,selectedRedSquaresconcat,toBoardCoordinate,History,HistoryModifier(..),Moves,displayWorld) where
  import Data.Char (toUpper, digitToInt,isAlphaNum)
  import Data.List (intercalate,find)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Utils
  import Graphics.Gloss

  data Side = Black | White deriving(Eq)
  data PieceType = Pawn | Rook | Knight | Bishop | King | Queen deriving(Eq, Enum)
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
    show (Piece piecetype Black _) = map toUpper $ show piecetype
    show (Piece piecetype White _) = show piecetype

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

  pieceBoard :: Board
  pieceBoard = (replicate 5 $ replicate 8 NoPiece) ++
    [[NoPiece,NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [(Piece Rook White True),(Piece Rook White True), NoPiece, NoPiece, NoPiece, NoPiece, NoPiece, NoPiece],
    [NoPiece, NoPiece, NoPiece, NoPiece, (Piece King Black True), NoPiece, NoPiece, (Piece King White True)]]

  displayBoard :: Show a => Bool -> [[a]] -> String
  displayBoard upRight board = 
    let flip = if upRight then (id) else (reverse) in
    unlines $ 
    [show i ++ " | " ++ (intercalate " " $ map (show) $ flip row) | (row, i) <- zip (flip board) $ flip [1..8]] ++ 
    ["--+" ++ replicate 16 '-'] ++ 
    ["  | " ++ (intercalate " " $ take 8 $ map (:[]) $ flip ['A' .. 'H'])]

  translateAsBoard :: Float -> Float -> Picture -> Picture
  translateAsBoard x y = translate (x*100 - 350) (y*100 - 350)

  toBoardCoordinate :: (Float,Float) -> Coordinate_t 
  toBoardCoordinate (x,y) = Coordinate (round $ (x + 350)/100)  (round $ (y + 350)/100 )

  displayBoardWindow :: ChessGameState -> String -> [Coordinate_t] -> Picture
  displayBoardWindow (ChessGameState _ board) msg possibleMoves = --add flip afterwards upon turn
    pictures $ 
    [translate (x - 350) (-450) $ scale 0.5 0.5 $ text $ s:[] | (x, s) <- zip [0,100..800] ['A' .. 'H']] ++
    [translate (-450) (y - 350) $ scale 0.5 0.5 $ text $ show s | (y, s) <- zip [0,100..800] [1..8]] ++
    [color clr $ translateAsBoard x y $ rectangleSolid 100 100 | 
      x <- [0..7], y <- [0..7], let clr = colorConverter x y] ++
    [color (if playSide p == White then white else black) $ translateAsBoard x y $ getPicture $ piecetype p | 
      (y, r) <- zip [0..7] board, (x, p) <- zip [0..7] r, p /= NoPiece] ++
    [translateAsBoard 10 7 $ text "<", translateAsBoard 0 8.2 $ scale 0.4 0.4 $ text msg]
    where 
      colorConverter x y
        | uncurry Coordinate (round x, round y) `elem` possibleMoves = green
        | even $ round (x + y) = light orange
        | otherwise = dark orange

  displayMenu :: Picture
  displayMenu = pictures [
    translateAsBoard 0 7 $ text "pick your side",
    color white $ translateAsBoard 2 4 $ rectangleSolid 100 100,
    color black $ translateAsBoard 5 4 $ rectangleSolid 100 100]
  
  type Moves = [((Piece, Coordinate_t), [Coordinate_t])]
  type History = [(PieceType, Coordinate_t, Coordinate_t, Maybe HistoryModifier)]
  data HistoryModifier = Capture { caputuredPiece :: PieceType} | Castling | Promotion
  data ChessGameWorld = ChessGameMenu | ChessGameOngoing { gameState :: ChessGameState, selectedSquare :: (Maybe Coordinate_t), history :: History, displayMsg :: String, endReached :: Bool, possibleMoves :: Moves}

  displayWorld :: ChessGameWorld -> Picture
  displayWorld ChessGameMenu = displayMenu
  displayWorld cgo = displayBoardWindow (gameState cgo) (displayMsg cgo) (selectedRedSquaresconcat cgo)
    -- map ((<$>) concat) $ getAllThyMoves -- > [((Piece, Coordinate_t), [Coordinate_t])]
    -- map ((<$>) concat) $ uncurry getAllThyMoves $ board &&& turn
  selectedRedSquaresconcat (ChessGameOngoing _ (Just c) _ _ _ possibleMoves) = concat $ snd <$> find ((== c) . snd . fst) possibleMoves
  selectedRedSquaresconcat (ChessGameOngoing _ Nothing _ _ _ _) = []

  instance Show HistoryModifier where
    show (Capture p) = show p
    show Castling = "O-O"
    show Promotion = "=Q"

  serializeHistory :: ChessGameWorld -> String
  serializeHistory = intercalate ", " . map (\(a,b,c,d) -> intercalate " " [show a, show b, show c ++ (maybe "" ((:) ' ') $ show <$> d)]) <. history

  serializePlayer :: ChessGameWorld -> String
  serializePlayer cgw = "Player  (" ++ (show . turn . gameState) cgw ++ ") \n"

  -- serializer :: ChessGameWorld -> IO ()
  -- serializer = writeFile <. (<*>)


  -- putStr $ serializePieces (ChessGameOngoing (ChessGameState Black newBoard) Nothing [(King, Coordinate 0 0, Coordinate 1 1, Nothing),(King, Coordinate 0 0, Coordinate 1 1, Nothing),(King, Coordinate 0 0, Coordinate 1 1, Just $ Capture Queen),(King, Coordinate 0 0, Coordinate 1 1, Just Castling)] [])


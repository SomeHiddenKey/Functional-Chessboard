{-# LANGUAGE RecordWildCards #-}

module BoardMovement where  
  import Data.Maybe (fromJust)
  import Data.Char (isAlphaNum, toUpper, digitToInt)
  import Data.List (intercalate)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),newBoard,displayBoard)

  type BoardCrumb = ([Piece], [Piece] ,[[Piece]], [[Piece]]) 

  data Coordinate a = Coordinate { x::a, y::a} deriving(Show, Eq)
  type Coordinate_t = Coordinate Int

  instance Functor Coordinate where
    fmap f (Coordinate x1 y1) = Coordinate (f x1) (f y1)

  instance Applicative Coordinate where
    pure e = Coordinate e e 
    (Coordinate fx fy) <*> (Coordinate x y) = Coordinate (fx x) (fy y)

  -- (>>>) = flip (>>=)
  revappend :: [a] -> [a] -> [a]
  revappend l r = foldl (flip (:)) r l

  iterate' :: Monad f => (a -> f a) -> Int -> f a -> f a
  iterate' fun 0 n = n
  iterate' fun i n = (iterate' fun ((abs i) - 1) n ) >>= fun

  goLeft :: BoardCrumb -> Maybe BoardCrumb
  goLeft (x:xs_left, xs_right, ys_top, ys_bot) = Just (xs_left, x:xs_right, ys_top, ys_bot)
  goLeft (_, _, _, _) = Nothing

  goRight :: BoardCrumb -> Maybe BoardCrumb
  goRight (xs_left, x:xs_right, ys_top, ys_bot) = Just (x:xs_left, xs_right, ys_top, ys_bot)
  goRight (_, _, _, _) = Nothing

  goOut :: BoardCrumb -> BoardCrumb
  goOut (xs_left, xs_right, ys_top, y:ys_bot) = ([],[],ys_top, (revappend xs_left xs_right):ys_bot)

  goUp :: BoardCrumb -> Maybe BoardCrumb 
  goUp (xs_left, xs_right, y:ys_top, _:ys_bot) = Just ([], y, ys_top, y:(revappend xs_left xs_right):ys_bot)
  goUp (_, _, _, _) = Nothing

  goDown :: BoardCrumb -> Maybe BoardCrumb
  goDown (xs_left, xs_right, ys_top, _:y:ys_bot) = Just ([], y, (revappend xs_left xs_right):ys_top, y:ys_bot)
  goDown (_, _, _, _) = Nothing

  -- goTo :: Board -> Coordinate_t -> BoardCrumb
  -- goTo board (Coordinate x y) = (!!!) x $ iterate goRight $ goIn $ (!!!) y $ iterate goDown ([], [], [], board)

  goTo :: Board -> Coordinate_t -> BoardCrumb
  goTo (firstRow:board) (Coordinate x y) = fromJust $ iterate' goRight x $ iterate' goDown y (Just ([], firstRow, [], firstRow:board))

  moveBy :: Coordinate_t -> BoardCrumb -> Maybe BoardCrumb
  moveBy (Coordinate x y) breadcrum = iterate' moveX x $ iterate' moveY y (Just breadcrum) where
    (moveX) = if x > 0 then (goRight) else (goLeft)
    (moveY) = if y > 0 then (goDown) else (goUp)

  getElement :: BoardCrumb -> Piece
  getElement (_, x:_, _, _) = x

  setElement :: Piece -> BoardCrumb -> (Piece, BoardCrumb)
  setElement (Piece piecetype side _) (xs_left, x:xs_right, ys_top, ys_bot) = (x, (xs_left, (Piece piecetype side False):xs_right, ys_top, ys_bot))

  getRow :: BoardCrumb -> [Piece]
  getRow (_, _, _, y:_) = y

  getBoard :: BoardCrumb -> Board
  getBoard ([], [], ys_top, ys_bot) = revappend ys_top ys_bot
  getBoard crum = getBoard $ goOut crum

  compare_element :: (Maybe Piece -> Bool) -> Maybe BoardCrumb -> Bool
  compare_element f = (f . ((<$>) getElement))

  availableSquare :: Side -> Piece -> Bool
  availableSquare Black (Piece _ Black _) = False
  availableSquare White (Piece _ White _) = False
  availableSquare _ _ = True

  checkMove :: ChessGameState -> Coordinate_t -> Coordinate_t -> Bool
  checkMove ChessGameState{..} startC@(Coordinate xstart ystart) endC@(Coordinate xend yend) = (availableSquare turn $ getElement $ goTo board endC) && (checkMovePiece $ getElement startPieceCrumb) where
    startPieceCrumb = goTo board startC
    checkMovePiece (Piece Tower side firstMove)
      | xstart==xend = side==turn && checkAllEmpty (abs $ yend - ystart) (Coordinate 0 $ signum $ yend - ystart) (Just startPieceCrumb)
      | ystart==yend = side==turn && checkAllEmpty (abs $ xend - xstart) (Coordinate (signum $ xend - xstart) 0) (Just startPieceCrumb)
    checkMovePiece (Piece Bishop side firstMove)
      | (abs $ xend - xstart)==(abs $ yend - ystart) = side==turn && checkAllEmpty (abs $ yend - ystart) (Coordinate (signum $ xend - xstart) $ signum $ yend - ystart) (Just startPieceCrumb)
    checkMovePiece (Piece Queen side firstMove)
      | checkMovePiece (Piece Tower side firstMove) = side==turn
      | checkMovePiece (Piece Bishop side firstMove) = side==turn
    checkMovePiece (Piece Pawn Black True) = ((yend - ystart) == 2) && xstart==xend && turn==Black && checkAllEmpty 2 (Coordinate 1 0) (Just startPieceCrumb)
    checkMovePiece (Piece Pawn White True) = ((ystart - yend) == 2) && xstart==xend && turn==White && checkAllEmpty 2 (Coordinate (-1) 0) (Just startPieceCrumb)
    checkMovePiece (Piece Pawn Black _)
      | xstart==xend = ((yend - ystart) == 1) && turn==Black && checkAllEmpty 1 (Coordinate 1 0) (Just startPieceCrumb)
      | (abs $ xend - xstart)==1 = ((yend - ystart) == 1) && turn==Black 
    checkMovePiece (Piece Pawn White _) = 
      | xstart==xend = ((ystart - yend) == 1) && turn==White && checkAllEmpty 1 (Coordinate (-1) 0) (Just startPieceCrumb)
      | (abs $ xend - xstart)==1 = ((ystart - yend) == 1) && turn==Black 
    checkMovePiece (Piece King White _) =  (abs $ xend - xstart) < 2 && (abs $ xend - xstart) < 2 && side==turn
    checkMovePiece (Piece Horse side _) =  
      | (abs $ xend - xstart) == 2 && (abs $ yend - ystart) == 1 = side==turn
      | (abs $ xend - xstart) == 1 && (abs $ yend - ystart) == 2 = side==turn
    checkMovePiece _ = False

  checkAllEmpty :: Int -> Coordinate_t -> Maybe BoardCrumb -> Bool
  checkAllEmpty n moveC crum = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n (iterate (\scr -> scr >>= (moveBy moveC)) crum)

  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement pieceStart $ fromJust $ moveBy (Coordinate xend (yend - ystart)) crumbStart where
    (pieceStart, crumbStart) = setElement NoPiece $ goTo board startC
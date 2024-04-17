module BoardMovement where  
  import Data.Maybe (fromJust)
  import Data.Char (isAlphaNum, toUpper, digitToInt)
  import Data.List (intercalate)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Board(Side,PieceType(..),Piece(..),Board,newBoard,displayBoard)

  type BoardCrumb = ([Piece], [Piece] ,[[Piece]], [[Piece]]) 

  data Coordinate a = Coordinate { x::a, y::a} deriving(Show)
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

  goTo :: Board -> Coordinate_t -> Maybe BoardCrumb
  goTo (firstRow:board) (Coordinate x y) = iterate' goRight x $ iterate' goDown y (Just ([], firstRow, [], firstRow:board))

  moveBy :: BoardCrumb -> Coordinate_t -> Maybe BoardCrumb
  moveBy breadcrum (Coordinate x y) = iterate' moveX x $ iterate' moveY y (Just breadcrum) where
    (moveX) = if x > 0 then (goRight) else (goLeft)
    (moveY) = if y > 0 then (goDown) else (goUp)

  getElement :: BoardCrumb -> Piece
  getElement (_, x:_, _, _) = x

  setElement :: Piece -> BoardCrumb -> (Piece, BoardCrumb)
  setElement newPiece (xs_left, x:xs_right, ys_top, ys_bot) = (x, (xs_left, newPiece:xs_right, ys_top, ys_bot))

  getRow :: BoardCrumb -> [Piece]
  getRow (_, _, _, y:_) = y

  getBoard :: BoardCrumb -> Board
  getBoard ([], [], ys_top, ys_bot) = revappend ys_top ys_bot
  getBoard crum = getBoard $ goOut crum

  -- checkMove :: ChessGameState -> Coordinate_t -> Coordinate_t -> Bool
  -- checkMove ChessGameState{..} startC endC = True

  --movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement pieceStart $ fromJust $ moveBy crumbStart (Coordinate xend (yend - ystart)) where
    (pieceStart, crumbStart) = setElement NoPiece $ fromJust $ goTo board startC
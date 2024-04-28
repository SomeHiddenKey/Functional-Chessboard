{-# LANGUAGE RecordWildCards #-}

module BoardMovement where  
  import Data.Maybe (fromJust, isJust, catMaybes)
  import Data.Char (isAlphaNum, toUpper, digitToInt)
  import Data.List (intercalate)
  import Data.Set (Set, insert, fromList, union)
  import qualified Data.Set as S (take, empty)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),newBoard,displayBoard,pieceBoard)

  type BoardCrumb = ([Piece], [Piece] ,[[Piece]], [[Piece]], Int, Int) 

  data Coordinate a = Coordinate { x::a, y::a} deriving(Show, Eq)
  type Coordinate_t = Coordinate Int

  instance Functor Coordinate where
    fmap f (Coordinate x1 y1) = Coordinate (f x1) (f y1)

  instance Ord a => Ord (Coordinate a) where
    compare (Coordinate x1 y1) (Coordinate x2 y2)
      | y1 > y2 = GT
      | x1 > x2 = GT
      | y1 == y2 && x1 == x2 = EQ
      | otherwise = LT

  toFst :: (a -> b) -> a -> (b, a)
  toFst f e = (f e, e)

  takeWhileAvailable :: Side -> [Maybe BoardCrumb] -> [Coordinate_t]
  takeWhileAvailable _ [] = []
  takeWhileAvailable _ (Nothing:_) = []
  takeWhileAvailable turn ((Just crum):rest) = case getElement crum of
    (Piece _ side _) -> if side==turn then [] else [getCoordinate crum]
    NoPiece -> getCoordinate crum : takeWhileAvailable turn rest

  instance Applicative Coordinate where
    pure e = Coordinate e e 
    (Coordinate fx fy) <*> (Coordinate x y) = Coordinate (fx x) (fy y)

  -- (>>>) = flip (>>=)
  revappend :: [a] -> [a] -> [a]
  revappend (e:rest) r = e:r `seq` revappend rest (e:r)
  revappend [] r = r

  iterate' :: Monad f => (a -> f a) -> Int -> f a -> f a
  iterate' fun 0 n = n
  iterate' fun i n = (iterate' fun ((abs i) - 1) n ) >>= fun

  goLeft :: BoardCrumb -> Maybe BoardCrumb
  goLeft (x:xs_left, xs_right, ys_top, ys_bot, xlvl, ylvl) = Just (xs_left, x:xs_right, ys_top, ys_bot, xlvl - 1, ylvl)
  goLeft (_, _, _, _, _, _) = Nothing

  goRight :: BoardCrumb -> Maybe BoardCrumb
  goRight (xs_left, x:xx:xs_right, ys_top, ys_bot, xlvl, ylvl) = Just (x:xs_left, xx:xs_right, ys_top, ys_bot, xlvl + 1, ylvl)
  goRight (_, _, _, _, _, _) = Nothing

  goOut :: BoardCrumb -> BoardCrumb
  goOut (xs_left, xs_right, ys_top, y:ys_bot, xlvl, ylvl) = ([],[],ys_top, (revappend xs_left xs_right):ys_bot, 0, ylvl)

  goUp :: BoardCrumb -> Maybe BoardCrumb 
  goUp (xs_left, xs_right, y:ys_top, _:ys_bot, xlvl, ylvl) = Just ([], y, ys_top, y:(revappend xs_left xs_right):ys_bot, 0, ylvl - 1)
  goUp (_, _, _, _, _, _) = Nothing

  goDown :: BoardCrumb -> Maybe BoardCrumb
  goDown (xs_left, xs_right, ys_top, _:y:ys_bot, xlvl, ylvl) = Just ([], y, (revappend xs_left xs_right):ys_top, y:ys_bot, 0, ylvl + 1)
  goDown (_, _, _, _, _, _) = Nothing

  getX :: BoardCrumb -> Int
  getX (_, _, _, _, xlvl, _) = xlvl

  getY :: BoardCrumb -> Int
  getY (_, _, _, _, _, ylvl) = ylvl

  getCoordinate :: BoardCrumb -> Coordinate_t
  getCoordinate (_, _, _, _, xlvl, ylvl) = Coordinate xlvl ylvl

  -- goTo :: Board -> Coordinate_t -> BoardCrumb
  -- goTo board (Coordinate x y) = (!!!) x $ iterate goRight $ goIn $ (!!!) y $ iterate goDown ([], [], [], board)

  goTo :: Board -> Coordinate_t -> BoardCrumb
  goTo (firstRow:board) (Coordinate x y) = fromJust $ iterate' goRight x $ iterate' goDown y (Just ([], firstRow, [], firstRow:board, 0, 0))

  -- iterate' goDown 1 (Just (goTo (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3)) (Coordinate 4 0)))
  -- iterate' goRight 0 $ iterate' goDown 1 (Just (goTo (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3)) (Coordinate 4 0)))

  --(x + getX breadcrum) -- y
  moveBy :: BoardCrumb -> Coordinate_t -> Maybe BoardCrumb
  moveBy breadcrum (Coordinate x 0) = iterate' moveX x $ (Just breadcrum) where
    (moveX) = if x > 0 then (goRight) else (goLeft)
  moveBy breadcrum (Coordinate x y) = iterate' moveX (x + getX breadcrum) $ iterate' moveY y (Just breadcrum) where
    (moveX) = if x + getX breadcrum > 0 then (goRight) else (goLeft)
    (moveY) = if y > 0 then (goDown) else (goUp)

  getElement :: BoardCrumb -> Piece
  getElement (_, x:_, _, _, _, _) = x
  getElement _ = NoPiece

  setElement :: Piece -> BoardCrumb -> (Piece, BoardCrumb)
  setElement (Piece piecetype playSide _) (xs_left, x:xs_right, ys_top, ys_bot, xlvl, ylvl) = (x, (xs_left, (Piece piecetype playSide False):xs_right, ys_top, ys_bot, xlvl, ylvl))
  setElement NoPiece (xs_left, x:xs_right, ys_top, ys_bot, xlvl, ylvl) = (x, (xs_left, NoPiece:xs_right, ys_top, ys_bot, xlvl, ylvl))

  getRow :: BoardCrumb -> [Piece]
  getRow (_, _, _, y:_, _, _) = y

  setRow :: BoardCrumb -> [Piece] -> BoardCrumb
  setRow (xs_left, xs_right, ys_bot, _:ys_top, xlvl, ylvl) y =  ([], y, ys_bot, y:ys_top, xlvl, ylvl)

  getBoard :: BoardCrumb -> Board
  getBoard ([], [], ys_top, ys_bot, _, _) = revappend ys_top ys_bot
  getBoard crum = getBoard $ goOut crum

  compare_element :: (Maybe Piece -> Bool) -> Maybe BoardCrumb -> Bool
  compare_element f = (f . ((<$>) getElement))

  availableSquare :: Side -> Piece -> Bool
  availableSquare Black (Piece _ Black _) = False
  availableSquare White (Piece _ White _) = False
  availableSquare _ _ = True

  oppositeSquare :: Side -> Piece -> Bool
  oppositeSquare Black (Piece _ White _) = True
  oppositeSquare White (Piece _ White _) = True
  oppositeSquare _ _ = False

  nextTurn :: Side -> Side
  nextTurn Black = White
  nextTurn White = Black
  -- pawn: 
  -- checkMove (ChessGameState 0 Black newBoard) (Coordinate 1 1) (Coordinate 1 2)
  -- horse:
  -- checkMove (ChessGameState 0 Black newBoard) (Coordinate 1 0) (Coordinate 2 2)
  -- checkMove (ChessGameState 0 Black newBoard) (Coordinate 1 0) (Coordinate 0 2)

  -- checkMovePiece (getElement $ goTo newBoard (Coordinate 1 1)) ((-) <$> abs <$> (Coordinate 1 1) <*> (Coordinate 1 2)) (goTo newBoard (Coordinate 1 1))
  -- (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3))

  -- queen: 
  -- checkMove (ChessGameState 0 Black (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3))) (Coordinate 4 0) (Coordinate 3 1)
  --tower: 
  -- checkMove (ChessGameState 0 Black (movePiece newBoard (Coordinate 0 1) (Coordinate 0 3))) (Coordinate 0 0) (Coordinate 0 1)
  -- checkMove (ChessGameState 0 White (movePiece newBoard (Coordinate 7 6) (Coordinate 7 4))) (Coordinate 7 7) (Coordinate 7 6)

  -- checkMove (ChessGameState 0 White pieceBoard) ((Coordinate 4 0), (Coordinate 2 0))
  checkMove :: ChessGameState -> (Coordinate_t, Coordinate_t) -> Either String ChessGameState
  checkMove ChessGameState{..} (startC@(Coordinate xstart ystart), endC@(Coordinate xend yend)) |
    (availableSquare turn $ getElement $ goTo board endC) && (checkMovePiece startPiece ((-) <$> abs <$> endC <*> startC) startPieceCrumb) && (turn == playSide startPiece) = Right (ChessGameState moveCount (nextTurn turn) (movePiece board startC endC)) where
    startPieceCrumb = goTo board startC
    startPiece = getElement startPieceCrumb
  checkMove (ChessGameState moveCount Black board) ((Coordinate 4 0), (Coordinate 2 0)) = 
     ChessGameState 0 White <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState moveCount Black board) ((Coordinate 4 0), (Coordinate 6 0)) = 
     ChessGameState 0 White <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState moveCount White board) ((Coordinate 4 7), (Coordinate 2 7)) = 
     ChessGameState 0 Black <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 7) 
  checkMove (ChessGameState moveCount White board) ((Coordinate 4 7), (Coordinate 6 7)) = 
     ChessGameState 0 Black <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 7)
  checkMove _ _ = Left "Illegal move"

  checkCastling :: [Piece] -> Either String [Piece]
  checkCastling [a, b, c, d, (Piece King Black True), NoPiece , NoPiece, (Piece Tower Black True)] = Right [a, b, c, d, NoPiece , (Piece Tower Black False) , (Piece King Black False), NoPiece]
  checkCastling [a, b, c, d, (Piece King White True), NoPiece , NoPiece, (Piece Tower White True)] = Right [a, b, c, d, NoPiece , (Piece Tower White False) , (Piece King White False), NoPiece]
  checkCastling [(Piece Tower Black True), NoPiece, NoPiece, NoPiece, (Piece King Black True), a , b , c] = Right [NoPiece, NoPiece, (Piece King Black False), (Piece King Black False), NoPiece, a , b , c]
  checkCastling [(Piece Tower White True), NoPiece, NoPiece, NoPiece, (Piece King White True), a , b , c] = Right [NoPiece, NoPiece, (Piece King White False), (Piece King White False), NoPiece, a , b , c]
  checkCastling _ = Left "Illegal move"

  -- piece -> movement -> crum
  checkMovePiece :: Piece -> Coordinate_t -> BoardCrumb-> Bool
  checkMovePiece (Piece Tower playSide _) c@(Coordinate 0 ydif) crum = checkAllEmpty (abs $ ydif) c crum
  checkMovePiece (Piece Tower playSide _) c@(Coordinate xdif 0) crum = checkAllEmpty (abs $ xdif) c crum
 
  checkMovePiece (Piece Bishop playSide _) c@(Coordinate xdif ydif) crum
    | (abs $ xdif)==(abs $ ydif) = checkAllEmpty (abs $ ydif) c crum
    | otherwise = False
  
  checkMovePiece (Piece Queen playSide firstMove) c crum
    | checkMovePiece (Piece Tower playSide firstMove) c crum = True
    | checkMovePiece (Piece Bishop playSide firstMove) c crum = True
    | otherwise = False

  checkMovePiece (Piece Pawn Black True) c@(Coordinate 0 2) crum = checkAllEmpty 2 c crum
  checkMovePiece (Piece Pawn White True) c@(Coordinate 0 (-2)) crum = checkAllEmpty 2 c crum

  checkMovePiece (Piece Pawn Black _) c@(Coordinate 0 1) crum = checkAllEmpty 1 c crum
  checkMovePiece (Piece Pawn Black _) c@(Coordinate 1 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn Black _) c@(Coordinate (-1) 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate 0 (-1)) crum = checkAllEmpty 1 c crum
  checkMovePiece (Piece Pawn White _) c@(Coordinate 1 (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate (-1) (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)

  checkMovePiece (Piece King _ _) (Coordinate xdif ydif) crum = (abs $ xdif) < 2 && (abs $ ydif) < 2

  checkMovePiece (Piece Horse _ _) (Coordinate xdif ydif) crum 
    | (==) [1,2] $ abs <$> [xdif,ydif] = True
    | (==) [2,1] $ abs <$> [xdif,ydif] = True
    | otherwise = False

  checkMovePiece a b c = False

  -- putStr $ displayBoard True (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3))
  -- checkAllEmpty 2 (Coordinate (-2) 2) $ goTo (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3)) (Coordinate 4 0)
  -- take 2 (iterate (\scr -> scr >>= (flip moveBy $ signum <$> (Coordinate (-2) 2))) (Just ( goTo (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3)) (Coordinate 4 0))))
  -- getElement <$> (moveBy (goTo (movePiece newBoard (Coordinate 3 1) (Coordinate 3 3)) (Coordinate 4 0)) (Coordinate 1 1))
  -- moveBy (snd $ setElement NoPiece $ goTo newBoard (Coordinate 3 1)) (Coordinate 0 2)

  checkElementSide :: Side -> Piece -> Bool
  checkElementSide Black (Piece _ Black _) = True
  checkElementSide White (Piece _ White _) = True
  checkElementSide _ _ = False

  checkAllEmpty :: Int -> Coordinate_t -> BoardCrumb -> Bool
  checkAllEmpty n c crum = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n (iterate (\scr -> scr >>= (flip moveBy $ signum <$> c)) (Just crum))

  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement pieceStart $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    (pieceStart, crumbStart) = setElement NoPiece $ goTo board startC

  getAllCrumbs :: Board -> Side -> [BoardCrumb]
  getAllCrumbs board turn = filter (checkElementSide turn . getElement) $ catMaybes $ concatMap (\c -> takeWhile isJust $ iterate (>>= goRight) c) $ takeWhile isJust $ iterate (>>= goDown) $ (Just $ goTo board (Coordinate 0 0))

  getMovesInDirection :: Foldable t => Side -> BoardCrumb -> t Coordinate_t -> [Coordinate_t]
  getMovesInDirection turn crum = concatMap (\c -> takeWhileAvailable turn $ drop 1 $ iterate (>>= ((flip moveBy) c)) $ Just crum)

  availableSquareInDir :: Side -> BoardCrumb -> Coordinate_t -> Bool
  availableSquareInDir turn crum dir = or $ availableSquare turn <$> getElement <$> moveBy crum dir
   
  getAllMovesPiece :: Piece -> BoardCrumb -> Set Coordinate_t
  getAllMovesPiece p@(Piece King turn _) crum = fromList $!
      [transformPos $ Coordinate x y | x <- [(-1)..1], y <- [(-1)..1], availableSquareInDir turn crum $ Coordinate x y]
      where transformPos =  (<*>) $! (+) <$> getCoordinate crum
  getAllMovesPiece p@(Piece Bishop turn _) crum = fromList $! 
      getMovesInDirection turn crum [Coordinate x y | x <- [(-1),1], y <- [(-1),1]]
  getAllMovesPiece p@(Piece Tower turn _) crum = fromList $! 
      getMovesInDirection turn crum $ [Coordinate 0 y |y <- [(-1),1]] ++ [Coordinate x 0 |x <- [(-1),1]]
  getAllMovesPiece p@(Piece Queen turn _) crum = fromList $! 
      getMovesInDirection turn crum [Coordinate x y | x <- [(-1),1], y <- [(-1),1], x/=0 || y/=0]
  getAllMovesPiece p@(Piece Horse turn _) crum = fromList $! 
      [transformPos $ Coordinate x y | x <- [(-1),1], y <- [(-2),2], availableSquareInDir turn crum $ Coordinate x y] ++ 
      [transformPos $ Coordinate x y | y <- [(-1),1], x <- [(-2),2], availableSquareInDir turn crum $ Coordinate x y] 
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece p@(Piece Pawn Black _) crum = fromList $! 
      [transformPos $ Coordinate x 1 | x <- [(-1)..1], checkMovePiece p (Coordinate x 1) crum] ++ 
      [transformPos $ Coordinate 0 2 | checkMovePiece p (Coordinate 0 2) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece p@(Piece Pawn _ _) crum = fromList $! 
      [transformPos $ Coordinate x (-1) | x <- [(-1)..1], checkMovePiece p (Coordinate x (-1)) crum] ++ 
      [transformPos $ Coordinate 0 (-2) | checkMovePiece p (Coordinate 0 (-2)) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece _ _ = S.empty 
  -- getAllMovesPiece (getElement (goTo newBoard (Coordinate 1 1))) (goTo newBoard (Coordinate 1 1))

  getAllMoves :: Board -> Side -> Set Coordinate_t
  getAllMoves board turn = foldr (($!) union) S.empty $ map (\crum -> (uncurry getAllMovesPiece) $ toFst getElement crum) $ getAllCrumbs board turn
  -- foldr (($!) union) S.empty $ 
  
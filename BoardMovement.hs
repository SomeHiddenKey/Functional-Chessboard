{-# LANGUAGE RecordWildCards #-}

module BoardMovement where  
  import Data.Maybe (fromJust, isJust, catMaybes, fromMaybe)
  import Data.Char (isAlphaNum, toUpper, digitToInt)
  import Data.List (intercalate)
  import Data.Set (Set, insert, fromList, union, member, intersection, isSubsetOf, difference)
  import qualified Data.Set as S (take, empty)
  import Data.Either (isRight)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Utils (Coordinate(..), Coordinate_t, getUntilElement, revappend, iterate', toFst, toSnd, concatJust, (&&&), flatTupple,maxWith,minWith,(<.),snd4,consIf,parMaxWith,parMinWith)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,pieceBoard,pieceValue,nextTurn,nextGameState,History,HistoryModifier(..),Moves)
  
  type BoardCrumb = ([Piece], [Piece] ,[[Piece]], [[Piece]], Int, Int) 

  takeWhileAvailable :: Side -> (Side -> Piece -> Bool) -> [Maybe BoardCrumb] -> [Coordinate_t]
  takeWhileAvailable _ _ [] = []
  takeWhileAvailable _ _ (Nothing:_) = []
  takeWhileAvailable turn validator ((Just crum):rest) = case getElement crum of
    p@(Piece _ _ _) -> if validator turn p then [getCoordinate crum] else []
    NoPiece -> getCoordinate crum : takeWhileAvailable turn validator rest

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

  goTo :: Board -> Coordinate_t -> BoardCrumb
  goTo (firstRow:board) (Coordinate x y) = fromJust $ iterate' goRight x $ iterate' goDown y (Just ([], firstRow, [], firstRow:board, 0, 0))

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
  setElement (Piece piecetype playSide firstMove) (xs_left, x:xs_right, ys_top, ys_bot, xlvl, ylvl) = (x, (xs_left, (Piece piecetype playSide firstMove):xs_right, ys_top, ys_bot, xlvl, ylvl))
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
  oppositeSquare White (Piece _ Black _) = True
  oppositeSquare _ _ = False

  checkElementSide :: Side -> Piece -> Bool
  checkElementSide Black (Piece _ Black _) = True
  checkElementSide White (Piece _ White _) = True
  checkElementSide _ _ = False

  checkAllEmpty :: Int -> BoardCrumb -> Coordinate_t -> Bool
  checkAllEmpty n crum c = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n $ iterate (\scr -> scr >>= (flip moveBy $ signum <$> c)) (Just crum)

  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement (Piece piecetype side False) $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    ((Piece piecetype side _), crumbStart) = setElement NoPiece $ goTo board startC

  recreatePiece :: History -> Coordinate_t -> Side -> PieceType -> Piece
  recreatePiece _ (Coordinate x 1) Black Pawn = Piece Pawn Black True
  recreatePiece _ (Coordinate x 6) White Pawn = Piece Pawn White True
  recreatePiece history c s Rook = Piece Rook s $ not $ any ((== c) . snd4) history
  recreatePiece history c s King = Piece King s $ not $ any ((== c) . snd4) history
  recreatePiece _ _ s p = Piece p s False

  resetPiece :: Board -> History -> Coordinate_t -> Coordinate_t -> Piece -> Board
  resetPiece board history startC@(Coordinate xstart ystart) endC@(Coordinate xend yend) newPiece = 
    getBoard $ snd $ setElement recreatedStartPiece $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    (pieceStart, crumbStart) = setElement newPiece $ goTo board startC
    recreatedStartPiece = recreatePiece history endC (playSide pieceStart) $ piecetype pieceStart

  setPiece :: Board -> Coordinate_t -> Piece -> Board
  setPiece board position NoPiece = getBoard . snd $ setElement NoPiece $ goTo board position
  setPiece board position (Piece piecetype side _) = getBoard . snd $ setElement (Piece piecetype side False) $ goTo board position

  getAllCrumbs :: Board -> Side -> [BoardCrumb]
  getAllCrumbs board turn = filter (checkElementSide turn . getElement) $ catMaybes $ concatMap (\c -> takeWhile isJust $ iterate (>>= goRight) c) $ takeWhile isJust $ iterate (>>= goDown) $ (Just $ goTo board (Coordinate 0 0))

  getMovesInDirection :: Side -> BoardCrumb ->  (Side -> Piece -> Bool) -> [Coordinate_t] -> [[Coordinate_t]]
  getMovesInDirection turn crum validator = map (\c -> takeWhileAvailable turn validator $ drop 1 $ iterate (>>= ((flip moveBy) c)) $ Just crum)

  checkAvailableSquareAt :: Side -> BoardCrumb -> (Side -> Piece -> Bool) -> Coordinate_t -> Bool
  checkAvailableSquareAt turn crum validator dir = or $ validator turn <$> getElement <$> moveBy crum dir

  getAllElementsOf :: Board -> Side -> [(Piece, Coordinate_t)]
  getAllElementsOf board turn = [(item, Coordinate x y) | (row, y) <- zip board [0..], (item, x) <- zip row [0..], item /= NoPiece, playSide item == turn]

  getAllElements :: Board -> [Piece]
  getAllElements board = [item | row <- board, item <- row, item /= NoPiece]

  getBestBoardValue :: Side -> Int
  getBestBoardValue White = maxBound :: Int
  getBestBoardValue Black = minBound :: Int

  getAllThyMoves :: (Piece -> BoardCrumb -> [[Coordinate_t]]) -> Board -> Side -> [((Piece, Coordinate_t), [[Coordinate_t]])]
  getAllThyMoves f board turn = getElement &&& getCoordinate &&& (uncurry f) . toFst getElement <$> getAllCrumbs board turn
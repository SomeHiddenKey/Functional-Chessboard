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
  import Utils (Coordinate(..), Coordinate_t, getUntilElement, revappend, iterate', toFst, toSnd, concatJust, (&&&), flatTupple,maxWith,minWith,snd4,consIf,parMaxWith,parMinWith)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),History,HistoryModifier(..),Moves)
  
  type BoardCrumb = ([Piece], [Piece] ,[[Piece]], [[Piece]], Int, Int) 

  -- keeps concatting coordinates of boardcrumbs until 
  -- * the end of the board is reached (Nothing)
  -- * any piece is hit
  -- if the validator returns true, it includes the last piece in the list of coordinates
  takeWhileAvailable :: (Piece -> Bool) -> [Maybe BoardCrumb] -> [Coordinate_t]
  takeWhileAvailable _ [] = []
  takeWhileAvailable _ (Nothing:_) = []
  takeWhileAvailable validator ((Just crum):rest) = case getElement crum of
    p@(Piece _ _ _) -> if validator p then [getCoordinate crum] else []
    NoPiece -> getCoordinate crum : takeWhileAvailable validator rest

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

  -- move the BoardCrum in a certain direction given by the coordinate
  moveBy :: BoardCrumb -> Coordinate_t -> Maybe BoardCrumb
  moveBy breadcrum (Coordinate x 0) = iterate' moveX x $ (Just breadcrum) where
    (moveX) = if x > 0 then (goRight) else (goLeft)
  moveBy breadcrum (Coordinate x y) = iterate' moveX (x + getX breadcrum) $ iterate' moveY y (Just breadcrum) where
    (moveX) = if x + getX breadcrum > 0 then (goRight) else (goLeft)
    (moveY) = if y > 0 then (goDown) else (goUp)

  -- get the element at the given crum location
  getElement :: BoardCrumb -> Piece
  getElement (_, x:_, _, _, _, _) = x
  getElement _ = NoPiece

  -- set a specific piece at the current location, returning the updated crum and the old piece at that location
  setElement :: Piece -> BoardCrumb -> (Piece, BoardCrumb)
  setElement (Piece piecetype playSide firstMove) (xs_left, x:xs_right, ys_top, ys_bot, xlvl, ylvl) = (x, (xs_left, (Piece piecetype playSide firstMove):xs_right, ys_top, ys_bot, xlvl, ylvl))
  setElement NoPiece (xs_left, x:xs_right, ys_top, ys_bot, xlvl, ylvl) = (x, (xs_left, NoPiece:xs_right, ys_top, ys_bot, xlvl, ylvl))

  getRow :: BoardCrumb -> [Piece]
  getRow (_, _, _, y:_, _, _) = y

  setRow :: BoardCrumb -> [Piece] -> BoardCrumb
  setRow (xs_left, xs_right, ys_bot, _:ys_top, xlvl, ylvl) y =  ([], y, ys_bot, y:ys_top, xlvl, ylvl)

  -- rebuilds the board from the current crum
  getBoard :: BoardCrumb -> Board
  getBoard ([], [], ys_top, ys_bot, _, _) = revappend ys_top ys_bot
  getBoard crum = getBoard $ goOut crum

  compare_element :: (Maybe Piece -> Bool) -> Maybe BoardCrumb -> Bool
  compare_element f = (f . ((<$>) getElement))

  -- any opposing colour or empty square location
  availableSquare :: Side -> Piece -> Bool
  availableSquare Black (Piece _ Black _) = False
  availableSquare White (Piece _ White _) = False
  availableSquare _ _ = True

  -- any capturable piece
  oppositeSquare :: Side -> Piece -> Bool
  oppositeSquare Black (Piece _ White _) = True
  oppositeSquare White (Piece _ Black _) = True
  oppositeSquare _ _ = False

  checkElementSide :: Side -> Piece -> Bool
  checkElementSide Black (Piece _ Black _) = True
  checkElementSide White (Piece _ White _) = True
  checkElementSide _ _ = False

  -- check if all squares in a specific direction are empty for N iterations, starting from the given crum. Iteration is done with moveBy in the c direction 
  checkAllEmpty :: Int -> BoardCrumb -> Coordinate_t -> Bool
  checkAllEmpty n crum c = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n $ iterate (>>= (flip moveBy $ signum <$> c)) (Just crum)

  -- moves a piece to a specific other location by first setting the start location to NoPiece, moving to the new location by the difference in coordinates, then set the old piece at the End location
  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement (Piece piecetype side False) $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    ((Piece piecetype side _), crumbStart) = setElement NoPiece $ goTo board startC

  -- recreates a piece based on its history, if applicable
  recreatePiece :: History -> Coordinate_t -> Side -> PieceType -> Piece
  -- first rank pawn hasn't moved yet
  recreatePiece _ (Coordinate x 1) Black Pawn = Piece Pawn Black True
  recreatePiece _ (Coordinate x 6) White Pawn = Piece Pawn White True
  -- if the rook coordinate does not appear in the history, then it hasn't moved yet
  recreatePiece history c s Rook = Piece Rook s $ not $ any ((== c) . snd4) history
  -- if the king coordinate does not appear in the history, then it hasn't moved yet
  recreatePiece history c s King = Piece King s $ not $ any ((== c) . snd4) history
  recreatePiece _ _ s p = Piece p s False

  -- same as movePiece, except it recreates the moved piece. Used for Undoing an action
  resetPiece :: Board -> History -> Coordinate_t -> Coordinate_t -> Piece -> Board
  resetPiece board history startC@(Coordinate xstart ystart) endC@(Coordinate xend yend) newPiece = 
    getBoard $ snd $ setElement recreatedStartPiece $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    (pieceStart, crumbStart) = setElement newPiece $ goTo board startC
    recreatedStartPiece = recreatePiece history endC (playSide pieceStart) $ piecetype pieceStart

  -- set a piece at a specific location on the board by first moving the crum to that location, putting the piece and rebuilding the board. A set piece is considered "moved", hence we force it as False
  setPiece :: Board -> Coordinate_t -> Piece -> Board
  setPiece board position NoPiece = getBoard . snd $ setElement NoPiece $ goTo board position
  setPiece board position p@Piece{} = getBoard . snd $ setElement p{firstMove=False} $ goTo board position

  -- iterate through the whole board and concat all the crums from all the pieces of the given side
  -- itteration is done first on the X axis, then on the Y axis, using goRight and goDown respectively. Coordinate 0 0 makes it start it the top upper corner of the board
  getAllCrumbs :: Board -> Side -> [BoardCrumb]
  getAllCrumbs board turn = filter (checkElementSide turn . getElement) $ catMaybes $ concatMap (\c -> takeWhile isJust $ iterate (>>= goRight) c) $ takeWhile isJust $ iterate (>>= goDown) $ (Just $ goTo board (Coordinate 0 0))

  -- concat of all coordinates assembled by takeWhileAvailable for all directions given with [Coordinate_t]
  getMovesInDirection :: Side -> BoardCrumb ->  (Side -> Piece -> Bool) -> [Coordinate_t] -> [[Coordinate_t]]
  getMovesInDirection turn crum validator = map (\c -> takeWhileAvailable (validator turn) $ drop 1 $ iterate (>>= ((flip moveBy) c)) $ Just crum)

  -- checks if the validator returns true when moving by a given coordinate starting from the given crum.
  -- (or) ensures that pieces out of bounce, returning Nothing, will give False
  checkAvailableSquareAt :: Side -> BoardCrumb -> (Side -> Piece -> Bool) -> Coordinate_t -> Bool
  checkAvailableSquareAt turn crum validator dir = or $ validator turn <$> getElement <$> moveBy crum dir


  -- list of all pieces and their coordinates for the given side. Done by itterating over all elements and filtering out NoPiece and everything from the opposing side
  getAllElementsOf :: Board -> Side -> [(Piece, Coordinate_t)]
  getAllElementsOf board turn = [(item, Coordinate x y) | (row, y) <- zip board [0..], (item, x) <- zip row [0..], item /= NoPiece, playSide item == turn]

  -- positive/negative infinite value, representing an impossible board value (when the King is left in check)
  getBestBoardValue :: Side -> Int
  getBestBoardValue White = maxBound :: Int
  getBestBoardValue Black = minBound :: Int

  -- concat all pieces, their coordinates and the individual coordinates it can go to per direction, generated by the given function f
  generateAllMoves :: (Piece -> BoardCrumb -> [[Coordinate_t]]) -> Board -> Side -> [((Piece, Coordinate_t), [[Coordinate_t]])]
  generateAllMoves f board turn = getElement &&& getCoordinate &&& (uncurry f) . toFst getElement <$> getAllCrumbs board turn
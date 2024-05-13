{-# LANGUAGE RecordWildCards #-}

module BoardMovement where  
  import Data.Maybe (fromJust, isJust, catMaybes)
  import Data.Char (isAlphaNum, toUpper, digitToInt)
  import Data.List (intercalate)
  import Data.Set (Set, insert, fromList, union, member, intersection, isSubsetOf, difference)
  import qualified Data.Set as S (take, empty)
  import Control.Monad (unless)
  import Text.Read (readEither)
  import GHC.IO.Handle (hFlush)
  import GHC.IO.Handle.FD (stdout)
  import Utils (Coordinate(..), Coordinate_t, getUntilElement, revappend, iterate', toFst, toSnd, concatJust, (&&&), flatTupple,maxWith,minWith,(<.),snd4)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,displayBoard,pieceBoard,pieceValue,nextTurn,nextGameState,displayBoardWindow,selectedRedSquaresconcat,toBoardCoordinate,History,HistoryModifier(..),Moves,displayWorld)
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Interact

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

  noneEmptySquare :: Side -> Piece -> Bool
  noneEmptySquare _ NoPiece = False
  noneEmptySquare _ _ = True

  --setPiece :: Board -> Coordinate_t -> Piece -> Board
  checkMove :: ChessGameState -> (Coordinate_t, Coordinate_t) -> Either String (ChessGameState, (PieceType, Maybe HistoryModifier))
  checkMove ChessGameState{..} (startC@(Coordinate xstart ystart), endC@(Coordinate xend yend)) 
    | checkPromotion startC endC startPiece startPieceCrumb && (turn == playSide startPiece) = 
      Right (ChessGameState (nextTurn turn) (setPiece (setPiece board endC (Piece Queen (playSide startPiece) False)) startC NoPiece) , (piecetype startPiece, Just Promotion))
    | (availableSquare turn endPiece) && (checkMovePiece startPiece ((-) <$> abs <$> endC <*> startC) startPieceCrumb) && (turn == playSide startPiece) = 
      Right (ChessGameState (nextTurn turn) (movePiece board startC endC) , (piecetype startPiece, if endPiece == NoPiece then Nothing else Just $ Capture $ piecetype endPiece)) 
    where
    startPieceCrumb = goTo board startC
    startPiece = getElement startPieceCrumb
    endPiece = getElement $ goTo board endC
  
  checkMove (ChessGameState Black board) ((Coordinate 4 0), (Coordinate 2 0)) = 
     (flip (,) $ (King, Just Castling)) <$> ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState Black board) ((Coordinate 4 0), (Coordinate 6 0)) = 
     (flip (,) $ (King, Just Castling)) <$> ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState White board) ((Coordinate 4 7), (Coordinate 2 7)) = 
     (flip (,) $ (King, Just Castling)) <$> ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 7) 
  checkMove (ChessGameState White board) ((Coordinate 4 7), (Coordinate 6 7)) = 
     (flip (,) $ (King, Just Castling)) <$> ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastling $ getRow crum) where crum = goTo board (Coordinate 0 7)
  checkMove _ _ = Left "Illegal move"

  checkPromotion :: Coordinate_t -> Coordinate_t -> Piece -> BoardCrumb -> Bool
  checkPromotion (Coordinate xstart 6) (Coordinate xend 7) (Piece Pawn Black _) crum
    | xstart == xend = checkAllEmpty 1 (Coordinate 0 1) crum
    | (==) 1 $ abs $ xend - xstart = or $ oppositeSquare Black <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) 1))
    | otherwise = False
  checkPromotion (Coordinate xstart 1) (Coordinate xend 0) (Piece Pawn White _) crum
    | xstart == xend = checkAllEmpty 1 (Coordinate 0 (-1)) crum
    | (==) 1 $ abs $ xend - xstart = or $ oppositeSquare White <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) (-1)))
    | otherwise = False
  checkPromotion _ _ _ _ = False

  checkCastling :: [Piece] -> Either String [Piece]
  checkCastling [a, b, c, d, (Piece King Black True), NoPiece , NoPiece, (Piece Rook Black True)] = Right [a, b, c, d, NoPiece , (Piece Rook Black False) , (Piece King Black False), NoPiece]
  checkCastling [a, b, c, d, (Piece King White True), NoPiece , NoPiece, (Piece Rook White True)] = Right [a, b, c, d, NoPiece , (Piece Rook White False) , (Piece King White False), NoPiece]
  checkCastling [(Piece Rook Black True), NoPiece, NoPiece, NoPiece, (Piece King Black True), a , b , c] = Right [NoPiece, NoPiece, (Piece King Black False), (Piece King Black False), NoPiece, a , b , c]
  checkCastling [(Piece Rook White True), NoPiece, NoPiece, NoPiece, (Piece King White True), a , b , c] = Right [NoPiece, NoPiece, (Piece King White False), (Piece King White False), NoPiece, a , b , c]
  checkCastling _ = Left "Illegal move"

  -- piece -> movement -> crum
  checkMovePiece :: Piece -> Coordinate_t -> BoardCrumb -> Bool
  checkMovePiece (Piece Rook playSide _) c@(Coordinate 0 ydif) crum = checkAllEmpty (abs $ ydif) c crum
  checkMovePiece (Piece Rook playSide _) c@(Coordinate xdif 0) crum = checkAllEmpty (abs $ xdif) c crum
 
  checkMovePiece (Piece Bishop playSide _) c@(Coordinate xdif ydif) crum
    | (abs $ xdif)==(abs $ ydif) = checkAllEmpty (abs $ ydif) c crum
    | otherwise = False
  
  checkMovePiece (Piece Queen playSide firstMove) c crum
    | checkMovePiece (Piece Rook playSide firstMove) c crum = True
    | checkMovePiece (Piece Bishop playSide firstMove) c crum = True
    | otherwise = False

  checkMovePiece (Piece Pawn Black True) c@(Coordinate 0 2) crum = checkAllEmpty 2 c crum
  checkMovePiece (Piece Pawn White True) c@(Coordinate 0 (-2)) crum = checkAllEmpty 2 c crum

  checkMovePiece (Piece Pawn Black _) c@(Coordinate 0 1) crum = checkAllEmpty 2 c crum
  checkMovePiece (Piece Pawn Black _) c@(Coordinate 1 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn Black _) c@(Coordinate (-1) 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate 0 (-1)) crum = checkAllEmpty 2 c crum
  checkMovePiece (Piece Pawn White _) c@(Coordinate 1 (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate (-1) (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)

  checkMovePiece (Piece King _ _) (Coordinate xdif ydif) crum = (abs $ xdif) < 2 && (abs $ ydif) < 2

  checkMovePiece (Piece Knight _ _) (Coordinate xdif ydif) crum 
    | (==) [1,2] $ abs <$> [xdif,ydif] = True
    | (==) [2,1] $ abs <$> [xdif,ydif] = True
    | otherwise = False

  checkMovePiece a b c = False

  checkElementSide :: Side -> Piece -> Bool
  checkElementSide Black (Piece _ Black _) = True
  checkElementSide White (Piece _ White _) = True
  checkElementSide _ _ = False

  checkAllEmpty :: Int -> Coordinate_t -> BoardCrumb -> Bool
  checkAllEmpty n c crum = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n $ iterate (\scr -> scr >>= (flip moveBy $ signum <$> c)) (Just crum)

  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement (Piece piecetype side False) $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    ((Piece piecetype side _), crumbStart) = setElement NoPiece $ goTo board startC

  resetPiece :: Board -> History -> Coordinate_t -> Coordinate_t -> Board
  resetPiece board history startC@(Coordinate xstart ystart) endC@(Coordinate xend yend) = 
    getBoard $ snd $ setElement recreatedStartPiece $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    (pieceStart, crumbStart) = setElement NoPiece $ goTo board startC
    recreatedStartPiece = recreatePiece (piecetype pieceStart) history endC $ playSide pieceStart 

  setPiece :: Board -> Coordinate_t -> Piece -> Board
  setPiece board position NoPiece = getBoard . snd $ setElement NoPiece $ goTo board position
  setPiece board position (Piece piecetype side _) = getBoard . snd $ setElement (Piece piecetype side False) $ goTo board position

  getAllCrumbs :: Board -> Side -> [BoardCrumb]
  getAllCrumbs board turn = filter (checkElementSide turn . getElement) $ catMaybes $ concatMap (\c -> takeWhile isJust $ iterate (>>= goRight) c) $ takeWhile isJust $ iterate (>>= goDown) $ (Just $ goTo board (Coordinate 0 0))

  getMovesInDirection :: Side -> BoardCrumb ->  (Side -> Piece -> Bool) -> [Coordinate_t] -> [[Coordinate_t]]
  getMovesInDirection turn crum validator = map (\c -> takeWhileAvailable turn validator $ drop 1 $ iterate (>>= ((flip moveBy) c)) $ Just crum)

  checkAvailableSquareAt :: Side -> BoardCrumb -> (Side -> Piece -> Bool) -> Coordinate_t -> Bool
  checkAvailableSquareAt turn crum validator dir = or $ validator turn <$> getElement <$> moveBy crum dir
  
  getAllMovesPieceDropTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceDropTarget = getAllMovesPiece availableSquare

  getAllMovesPieceWithTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceWithTarget = getAllMovesPiece noneEmptySquare

  getAllMovesPiece :: (Side -> Piece -> Bool) -> Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPiece validator p@(Piece King turn _) crum = 
      [[transformPos $ Coordinate x y]| x <- [(-1)..1], y <- [(-1)..1], x/=0 || y/=0,checkAvailableSquareAt turn crum validator $ Coordinate x y]
      where transformPos =  (<*>) $! (+) <$> getCoordinate crum
  getAllMovesPiece validator p@(Piece Bishop turn _) crum  = 
      getMovesInDirection turn crum validator [Coordinate x y | x <- [(-1),1], y <- [(-1),1]]
  getAllMovesPiece validator p@(Piece Rook turn _) crum  = 
      getMovesInDirection turn crum validator $ [Coordinate 0 y |y <- [(-1),1]] ++ [Coordinate x 0 |x <- [(-1),1]]
  getAllMovesPiece validator p@(Piece Queen turn _) crum  = 
      getMovesInDirection turn crum validator [Coordinate x y | x <- [(-1)..1], y <- [(-1)..1], x/=0 || y/=0]
  getAllMovesPiece validator p@(Piece Knight turn _) crum  = 
      [[transformPos $ Coordinate x y] | x <- [(-1),1], y <- [(-2),2], checkAvailableSquareAt turn crum validator $ Coordinate x y] ++
      [[transformPos $ Coordinate x y] | y <- [(-1),1], x <- [(-2),2], checkAvailableSquareAt turn crum validator $ Coordinate x y] 
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece validator p@(Piece Pawn Black _) crum  = 
      [[transformPos $ Coordinate x 1] | x <- [(-1)..1], checkMovePiece p (Coordinate x 1) crum] ++ 
      [[transformPos $ Coordinate 0 2] | checkMovePiece p (Coordinate 0 2) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece validator p@(Piece Pawn _ _) crum  =
      [[transformPos $ Coordinate x (-1)] | x <- [(-1)..1], checkMovePiece p (Coordinate x (-1)) crum] ++ 
      [[transformPos $ Coordinate 0 (-2)] | checkMovePiece p (Coordinate 0 (-2)) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece _ _ _ = [[]]

  getAllMoves :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t
  getAllMoves c = foldr combineSets S.empty $ map (($!) fromList . concat . snd) c where 
    combineSets movesA movesB = movesA `seq` movesB `seq` union movesA movesB
  
  checkMateBlockers :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t -> Coordinate_t -> [Set Coordinate_t]
  checkMateBlockers attackingMoves allDefensiveMoves king_coordinate = map (\movesForPiece -> (flip intersection) allDefensiveMoves <$> concatJust union S.empty $ map (\moves -> fromList <$> getUntilElement king_coordinate moves) movesForPiece) $ map snd attackingMoves

  getAllElementsOf :: Board -> Side -> [(Piece, Coordinate_t)]
  getAllElementsOf board turn = [(item, Coordinate x y) | (row, y) <- zip board [0..], (item, x) <- zip row [0..], item /= NoPiece, playSide item == turn]

  getAllElements :: Board -> [Piece]
  getAllElements board = [item | row <- board, item <- row, item /= NoPiece]

  -- checkMoveLegality :: Coordinate_t -> ChessGameState -> Either String (Coordinate_t, ChessGameState)
  -- checkMoveLegality kingCorSelf cgs@(ChessGameState turn board)
  --   | kingCorSelf `member` getAllMoves movesOpponent = 
  --       Left "Can't put yourself in check"
  --   | all (all null) movesOpponent = if kingCorOpponent `member` redSquaresSelf  
  --       then Left $ (++) "Checkmate for" $ show $ nextTurn turn 
  --       else Left $ (++) "Stale mate for" $ show $ nextTurn turn 
  --   | (kingMovesOpponent `isSubsetOf` redSquaresSelf) && 
  --     (all null $ checkMateBlockers movesOpponent redSquaresSelf kingCorSelf) = Left $ (++) "Checkmate for " $ show $ nextTurn turn 
  --   | otherwise = Right (kingCorOpponent, cgs)
  --   where 
  --     redSquaresSelf = getAllMoves $ getAllThyMoves getAllMovesPieceWithTarget board $ nextTurn turn --w/o king
  --     movesOpponent = getAllThyMoves getAllMovesPieceDropTarget board turn
  --     kingCorOpponent = getKingCoordinate cgs
  --     kingMovesOpponent = fromList $ (:) kingCorOpponent $ concat $ getAllMovesPieceDropTarget (Piece King turn False) $ goTo board kingCorOpponent

  checkLegality :: Set Coordinate_t -> Coordinate_t -> Bool
  checkLegality = flip member

  checkEndGame :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t -> ChessGameState -> Maybe String
  checkEndGame movesBlack allWhiteMovesNoKing cgs@(ChessGameState turn board)
    | null allWhiteMovesNoKing && (null $ difference kingMovesWhite allMovesBlack) = 
      if member kingCorWhite allMovesBlack
      then Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ "won"
      else Just $ "Stalemate: " ++ (show turn) ++ "won"
    | (isSubsetOf kingMovesWhite allMovesBlack) && (all null $ checkMateBlockers movesBlack allWhiteMovesNoKing kingCorWhite) = 
      Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ "won"
    | otherwise = Nothing
    where 
      kingCorWhite = getKingCoordinate cgs turn
      kingMovesWhite = fromList $ (:) kingCorWhite $ concat $ getAllMovesPieceDropTarget (Piece King turn False) $ goTo board kingCorWhite
      allMovesBlack = getAllMoves movesBlack

  boardValue :: Board -> Int
  boardValue = (foldr add 0) . getAllElements  where
  add (Piece piecetype Black _) x = pieceValue piecetype `seq` x - pieceValue piecetype `seq` x - pieceValue piecetype
  add (Piece piecetype White _) x = pieceValue piecetype `seq` x + pieceValue piecetype `seq` x + pieceValue piecetype

  getKingCoordinate :: ChessGameState -> Side -> Coordinate_t
  getKingCoordinate ChessGameState{..} side = head [c | ((Piece King _ _), c) <- getAllElementsOf board side] 

  getBestMove :: (Eq t, Num t) => t -> Coordinate_t -> ChessGameState -> Maybe ((ChessGameState, (Coordinate_t, Coordinate_t)), Int)
  getBestMove 0 kingCor cgs@(ChessGameState turn currentBoard)
    | member kingCorNext $ getAllMoves $ getAllThyMoves getAllMovesPieceDropTarget currentBoard (nextTurn turn) = Nothing
    | otherwise = Just $ ((cgs , (Coordinate 0 0, Coordinate 0 0)), boardValue currentBoard)
    where
      allMoves = getAllMovesFor currentBoard turn
      kingCorNext = getKingCoordinate cgs turn

  getBestMove depth kingCor cgs@(ChessGameState turn currentBoard)
    -- | member kingCor $ fromList $ map snd allMoves = Nothing
    | member kingCorNext $ getAllMoves $ getAllThyMoves getAllMovesPieceDropTarget currentBoard (nextTurn turn) = Nothing
    | otherwise = Just $ bestWith turn ((getBestBoardValue turn) . (getBestMove (depth - 1) kingCorNext) . fst) $ map (toFst $ nextGameState cgs . uncurry (movePiece currentBoard)) allMoves
    where
      allMoves = getAllMovesFor currentBoard turn
      kingCorNext = getKingCoordinate cgs turn

  getAllMovesFor :: Board -> Side -> [(Coordinate_t, Coordinate_t)]
  getAllMovesFor board turn = flatTupple $ getCoordinate &&& concat . (uncurry getAllMovesPieceDropTarget) . toFst getElement <$> getAllCrumbs board turn

  bestWith Black = minWith
  bestWith White = maxWith

  getBestBoardValue :: Side -> Maybe (a,Int) -> Int
  getBestBoardValue Black Nothing = maxBound :: Int
  getBestBoardValue White Nothing  = minBound :: Int
  getBestBoardValue _ (Just (_,e)) = e

  getAllThyMoves :: (Piece -> BoardCrumb -> [[Coordinate_t]]) -> Board -> Side -> [((Piece, Coordinate_t), [[Coordinate_t]])]
  getAllThyMoves f board turn = getElement &&& getCoordinate &&& (uncurry f) . toFst getElement <$> getAllCrumbs board turn

  startGameFromLoad :: ChessGameState -> History -> IO ()
  startGameFromLoad cgs@(ChessGameState side board) history = play FullScreen (greyN 0.3) 5 (ChessGameOngoing cgs Nothing history "" False $ getMovesForSide cgs) displayWorld changeWorld (\tick world -> world)

  startGameFromMenu :: IO ()
  startGameFromMenu = play FullScreen (greyN 0.3) 20 ChessGameMenu displayWorld changeWorld (\tick world -> world)

  changeWorld :: Event -> ChessGameWorld -> ChessGameWorld
  changeWorld (EventKey (MouseButton LeftButton) Down _ c) = changeWorldBoard $ toBoardCoordinate c
  changeWorld _ = id

  changeWorldBoard :: Coordinate_t -> ChessGameWorld -> ChessGameWorld

  changeWorldBoard (Coordinate 2 4) ChessGameMenu = ChessGameOngoing newCgs Nothing [] "White to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState White newBoard
  changeWorldBoard (Coordinate 5 4) ChessGameMenu = ChessGameOngoing newCgs Nothing [] "Black to begin" False $ getMovesForSide newCgs where newCgs = ChessGameState Black newBoard

  changeWorldBoard (Coordinate 10 7) cgw@(ChessGameOngoing _ _ _ _ _ _) = undo cgw

  changeWorldBoard c (ChessGameOngoing gs Nothing hs _ False pm) = ChessGameOngoing gs (Just c) hs "" False pm
  
  changeWorldBoard c cgo@(ChessGameOngoing gs (Just sq) hs _ False _) = either (const $ resetSelection cgo) (testsdd c cgo) (checkMove gs (sq, c))
  
  changeWorldBoard _ world = world

  resetSelection :: ChessGameWorld -> ChessGameWorld
  resetSelection ChessGameOngoing{..} = ChessGameOngoing gameState Nothing history "" endReached possibleMoves

  getMovesForSide :: ChessGameState -> [((Piece, Coordinate_t), [Coordinate_t])]
  getMovesForSide (ChessGameState turn board) = map ((<$>) concat) $ getAllThyMoves getAllMovesPieceDropTarget board turn

  testsdd :: Coordinate_t -> ChessGameWorld -> (ChessGameState,(PieceType,Maybe HistoryModifier)) -> ChessGameWorld
  testsdd c (ChessGameOngoing gs (Just sq) hs _ False pm) (cgs@(ChessGameState turn board), (pt, mod))
    | isJust perhapsWinMessage = 
      ChessGameOngoing cgs Nothing ((pt,sq,c,mod) : hs) (fromJust perhapsWinMessage) True []
    | checkLegality allWhiteMovesNoKing $ getKingCoordinate cgs $ nextTurn turn =
      ChessGameOngoing gs Nothing hs "can't put yourself in check" False pm
    | otherwise = 
      ChessGameOngoing cgs Nothing ((pt,sq,c,mod) : hs) "" False $ getMovesForSide cgs
    where
      movesBlack = getAllThyMoves getAllMovesPieceWithTarget board $ nextTurn turn
      allWhiteMovesNoKing = getAllMoves $ filter ((/= King).piecetype.fst.fst) $ getAllThyMoves getAllMovesPieceDropTarget board turn
      perhapsWinMessage = checkEndGame movesBlack allWhiteMovesNoKing cgs

  checkDraw :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> [((Piece, Coordinate_t), [[Coordinate_t]])] -> Bool
  checkDraw [] [((Piece Bishop _ _),_)]
  checkDraw [((Piece Bishop _ _),_)] []
  checkDraw [] [((Piece Knight _ _),_)]
  checkDraw [((Piece Knight _ _),_)] []
  checkDraw [((Piece Bishop _ _),_)] [((Piece Bishop _ _),_)]

  undo :: ChessGameWorld -> ChessGameWorld
  undo cgw@(ChessGameOngoing _ _ [] _ _ _) = cgw

  undo (ChessGameOngoing ChessGameState{..} selectedSquare ((pt,startC,endC,Nothing):historyRest) dMsg _ possibleMoves) = 
    ChessGameOngoing updatedChessGameState Nothing historyRest dMsg False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece board historyRest endC startC

  undo (ChessGameOngoing ChessGameState{..} selectedSquare ((pt,startC,endC,Just (Capture cp)):historyRest) dMsg _ possibleMoves) = 
    ChessGameOngoing updatedChessGameState Nothing historyRest dMsg False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ setPiece (movePiece board endC startC) endC $ recreatePiece cp historyRest endC turn

  recreatePiece :: PieceType -> History -> Coordinate_t -> Side -> Piece
  recreatePiece Pawn _ (Coordinate x 1) Black = Piece Pawn Black True
  recreatePiece Pawn _ (Coordinate x 6) White = Piece Pawn White True
  recreatePiece Rook history c s = Piece Rook s $ not $ any ((== c) . snd4) history
  recreatePiece King history c s = Piece Rook s $ not $ any ((== c) . snd4) history
  recreatePiece p _ _ s = Piece p s False

  -- [(PieceType, Coordinate_t, Coordinate_t, Maybe HistoryModifier)]
  -- displayBoardWindowP (ChessGameState 0 Black newBoard) []

  serializePieces :: ChessGameWorld -> String
  serializePieces cgw = intercalate "\n" .concat $ streamer ([flip getAllElementsOf Black, flip getAllElementsOf White] <*> [(board . gameState) cgw] ) where streamer = map . map $ (\(piece, cor::Coordinate_t) -> intercalate ", " [show $ piecetype piece, show $ playSide piece, show cor])
   
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
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),newBoard,displayBoard,pieceBoard,pieceValue,nextTurn,nextGameState,displayBoardWindow,toBoardCoordinate,History,HistoryModifier(..),Moves,displayWorld)
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

  checkMove :: ChessGameState -> (Coordinate_t, Coordinate_t) -> Either String (ChessGameState, (PieceType, Maybe HistoryModifier))
  checkMove ChessGameState{..} (startC@(Coordinate xstart ystart), endC@(Coordinate xend yend)) 
    | checkPromotion startC endC startPiece startPieceCrumb && (turn == playSide startPiece) = 
      Right (ChessGameState (nextTurn turn) (setPiece (setPiece board endC (Piece Queen (playSide startPiece) False)) startC NoPiece) , (piecetype startPiece, Just $ Promotion endPieceCaptured))
    | (availableSquare turn endPiece) && (checkMovePiece startPiece ((-) <$> abs <$> endC <*> startC) startPieceCrumb) && (turn == playSide startPiece) = 
      Right (ChessGameState (nextTurn turn) (movePiece board startC endC) , (piecetype startPiece, Capture <$> endPieceCaptured)) 
    where
    startPieceCrumb = goTo board startC
    startPiece = getElement startPieceCrumb
    endPiece = getElement $ goTo board endC
    endPieceCaptured = if endPiece == NoPiece then Nothing else Just $ piecetype endPiece
  
  checkMove (ChessGameState Black board) ((Coordinate 4 0), (Coordinate 2 0)) = 
     (flip (,) $ (King, Just CastlingL)) <$> ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastlingLeft $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState Black board) ((Coordinate 4 0), (Coordinate 6 0)) = 
     (flip (,) $ (King, Just CastlingR)) <$> ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastlingRight $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove (ChessGameState White board) ((Coordinate 4 7), (Coordinate 2 7)) = 
     (flip (,) $ (King, Just CastlingL)) <$> ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastlingLeft $ getRow crum) where crum = goTo board (Coordinate 0 7) 
  checkMove (ChessGameState White board) ((Coordinate 4 7), (Coordinate 6 7)) = 
     (flip (,) $ (King, Just CastlingR)) <$> ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastlingRight $ getRow crum) where crum = goTo board (Coordinate 0 7)
  checkMove _ _ = Left "Illegal move"

  checkPromotion :: Coordinate_t -> Coordinate_t -> Piece -> BoardCrumb -> Bool
  checkPromotion (Coordinate xstart 6) (Coordinate xend 7) (Piece Pawn Black _) crum
    | xstart == xend = checkAllEmpty 2 crum $ Coordinate 0 1
    | (==) 1 $ abs $ xend - xstart = or $ oppositeSquare Black <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) 1))
    | otherwise = False
  checkPromotion (Coordinate xstart 1) (Coordinate xend 0) (Piece Pawn White _) crum
    | xstart == xend = checkAllEmpty 2 crum $ Coordinate 0 (-1)
    | (==) 1 $ abs $ xend - xstart = or $ oppositeSquare White <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) (-1)))
    | otherwise = False
  checkPromotion _ _ _ _ = False

  checkCastlingLeft :: [Piece] -> Either String [Piece]
  checkCastlingLeft [(Piece Rook Black True), NoPiece, NoPiece, NoPiece, (Piece King Black True), a , b , c] = Right [NoPiece, NoPiece, (Piece King Black False), (Piece Rook Black False), NoPiece, a , b , c]
  checkCastlingLeft [(Piece Rook White True), NoPiece, NoPiece, NoPiece, (Piece King White True), a , b , c] = Right [NoPiece, NoPiece, (Piece King White False), (Piece Rook White False), NoPiece, a , b , c]
  checkCastlingLeft _ = Left "Illegal move"

  checkCastlingRight :: [Piece] -> Either String [Piece]
  checkCastlingRight [a, b, c, d, (Piece King Black True), NoPiece , NoPiece, (Piece Rook Black True)] = Right [a, b, c, d, NoPiece , (Piece Rook Black False) , (Piece King Black False), NoPiece]
  checkCastlingRight [a, b, c, d, (Piece King White True), NoPiece , NoPiece, (Piece Rook White True)] = Right [a, b, c, d, NoPiece , (Piece Rook White False) , (Piece King White False), NoPiece]
  checkCastlingRight _ = Left "Illegal move"

  uncastleLeft :: [Piece] -> [Piece]
  uncastleLeft [NoPiece, NoPiece, king, rook, NoPiece, a, b, c] = [rook{firstMove=True}, NoPiece, NoPiece, NoPiece, king{firstMove=True}, a, b, c]
  
  uncastleRight :: [Piece] -> [Piece]
  uncastleRight [a, b, c, d, NoPiece , rook , king, NoPiece] = [a, b, c, d, king{firstMove=True}, NoPiece , NoPiece, rook{firstMove=True}]

  -- piece -> movement -> crum
  checkMovePiece :: Piece -> Coordinate_t -> BoardCrumb -> Bool
  checkMovePiece (Piece Rook playSide _) c@(Coordinate 0 ydif) crum = checkAllEmpty (abs $ ydif) crum c
  checkMovePiece (Piece Rook playSide _) c@(Coordinate xdif 0) crum = checkAllEmpty (abs $ xdif) crum c
 
  checkMovePiece (Piece Bishop playSide _) c@(Coordinate xdif ydif) crum
    | (abs $ xdif)==(abs $ ydif) = checkAllEmpty (abs $ ydif) crum c
    | otherwise = False
  
  checkMovePiece (Piece Queen playSide firstMove) c crum
    | checkMovePiece (Piece Rook playSide firstMove) c crum = True
    | checkMovePiece (Piece Bishop playSide firstMove) c crum = True
    | otherwise = False

  checkMovePiece (Piece Pawn Black True) c@(Coordinate 0 2) crum = checkAllEmpty 3 crum c
  checkMovePiece (Piece Pawn White True) c@(Coordinate 0 (-2)) crum = checkAllEmpty 3 crum c

  checkMovePiece (Piece Pawn Black _) c@(Coordinate 0 1) crum = checkAllEmpty 2 crum c
  checkMovePiece (Piece Pawn Black _) c@(Coordinate 1 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn Black _) c@(Coordinate (-1) 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate 0 (-1)) crum = checkAllEmpty 2 crum c
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

  checkAllEmpty :: Int -> BoardCrumb -> Coordinate_t -> Bool
  checkAllEmpty n crum c = all (compare_element (== (Just NoPiece))) $ drop 1 $ take n $ iterate (\scr -> scr >>= (flip moveBy $ signum <$> c)) (Just crum)

  movePiece :: Board -> Coordinate_t -> Coordinate_t -> Board
  movePiece board startC@(Coordinate xstart ystart) (Coordinate xend yend) = 
    getBoard $ snd $ setElement (Piece piecetype side False) $ fromJust $ moveBy crumbStart $ Coordinate (xend - xstart) (yend - ystart) where
    ((Piece piecetype side _), crumbStart) = setElement NoPiece $ goTo board startC

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
  
  getAllMovesPieceDropTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceDropTarget = getAllMovesPiece availableSquare

  getAllMovesPieceWithTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceWithTarget = getAllMovesPiece $ const . const True

  getAllMovesPiece :: (Side -> Piece -> Bool) -> Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPiece validator p@(Piece King turn firstMove) crum
    | firstMove = addCastleL $ addCastleR [[transformPos $ Coordinate x y]| x <- [(-1)..1], y <- [(-1)..1], x/=0 || y/=0,checkAvailableSquareAt turn crum validator $ Coordinate x y]
    | otherwise = [[transformPos $ Coordinate x y]| x <- [(-1)..1], y <- [(-1)..1], x/=0 || y/=0,checkAvailableSquareAt turn crum validator $ Coordinate x y]
      where 
        transformPos =  (<*>) $! (+) <$> getCoordinate crum
        addCastleL = consIf (isRight $ checkCastlingLeft $ getRow crum) $ [transformPos $ Coordinate (-2) 0]
        addCastleR = consIf (isRight $ checkCastlingRight $ getRow crum) $ [transformPos $ Coordinate 2 0]
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
      [[transformPos $ Coordinate x 1] | x <- [(-1),1], not $ checkAllEmpty 2 crum $ Coordinate x 1, checkAvailableSquareAt Black crum validator $ Coordinate x 1] ++ 
      [[transformPos $ Coordinate 0 y] | y <- [1,2], checkMovePiece p (Coordinate 0 y) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece validator p@(Piece Pawn White _) crum =
      [[transformPos $ Coordinate x (-1)] | x <- [(-1),1], not $ checkAllEmpty 2 crum $ Coordinate x (-1), checkAvailableSquareAt White crum validator $ Coordinate x (-1)] ++ 
      [[transformPos $ Coordinate 0 y] | y <- [(-1),(-2)], checkMovePiece p (Coordinate 0 y) crum]
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
      then Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ " won"
      else Just $ "Stalemate: " ++ (show turn) ++ " won"
    | (isSubsetOf kingMovesWhite allMovesBlack) && (all null $ checkMateBlockers movesBlack allWhiteMovesNoKing kingCorWhite) = 
      Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ " won"
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

  getBestMove :: Int -> ChessGameState -> ((ChessGameState, (Coordinate_t, Coordinate_t)), Int)
  getBestMove 0 cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ getAllThyMoves getAllMovesPieceWithTarget currentBoard turn = ((cgs , (Coordinate 0 0, Coordinate 0 0)), getBestBoardValue turn) 
    | otherwise = ((cgs , (Coordinate 0 0, Coordinate 0 0)), boardValue currentBoard)
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  getBestMove depth cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ getAllThyMoves getAllMovesPieceWithTarget currentBoard turn = ((cgs , (Coordinate 0 0, Coordinate 0 0)), getBestBoardValue turn) 
    | otherwise = bestWith (snd . (getBestMove (depth - 1)) . fst) $ map (toFst $ nextGameState cgs . uncurry (movePiece currentBoard)) allMoves
    where
      allMoves = getAllMovesFor currentBoard turn
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn
      bestWith
        | turn == Black && (depth > 3) = parMinWith
        | turn == Black = minWith
        | turn == White && (depth > 3) = parMaxWith
        | otherwise = maxWith

  promptAImove :: ChessGameWorld -> ChessGameWorld
  promptAImove cgo@ChessGameOngoing{..} = ChessGameOngoing newcgs Nothing True ((piecetype pieceEnd,startC,endC,mod) : history) "" False $ getMovesForSide newcgs
    where 
      (newcgs, (startC, endC)) = fst $ getBestMove 4 gameState
      pieceStart = getElement $ (flip goTo) startC $ board gameState
      mod = if pieceStart == NoPiece then Nothing else Just $ Capture $ piecetype pieceStart
      pieceEnd = getElement $ (flip goTo) endC $ board gameState

  getAllMovesFor :: Board -> Side -> [(Coordinate_t, Coordinate_t)]
  getAllMovesFor board turn = flatTupple $ getCoordinate &&& concat . (uncurry getAllMovesPieceDropTarget) . toFst getElement <$> getAllCrumbs board turn

  getBestBoardValue :: Side -> Int
  getBestBoardValue White = maxBound :: Int
  getBestBoardValue Black = minBound :: Int

  getAllThyMoves :: (Piece -> BoardCrumb -> [[Coordinate_t]]) -> Board -> Side -> [((Piece, Coordinate_t), [[Coordinate_t]])]
  getAllThyMoves f board turn = getElement &&& getCoordinate &&& (uncurry f) . toFst getElement <$> getAllCrumbs board turn

  getMovesForSide :: ChessGameState -> [((Piece, Coordinate_t), [Coordinate_t])]
  getMovesForSide (ChessGameState turn board) = map ((<$>) concat) $ getAllThyMoves getAllMovesPieceDropTarget board turn

  testsdd :: Coordinate_t -> ChessGameWorld -> (ChessGameState,(PieceType,Maybe HistoryModifier)) -> ChessGameWorld
  testsdd c (ChessGameOngoing gs (Just sq) activeAI hs _ False pm) (cgs@(ChessGameState turn board), (pt, mod))
    | isJust perhapsWinMessage = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) (fromJust perhapsWinMessage) True []
    | checkDraw movesWhite $ filter ((/= King).piecetype.fst.fst) movesBlack = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) "Draw by insufficient material" True []
    | checkLegality allWhiteMoves $ getKingCoordinate cgs $ nextTurn turn =
      ChessGameOngoing gs Nothing activeAI hs "can't put yourself in check" False pm
    | otherwise = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) "" False $ getMovesForSide cgs
    where
      movesBlack = getAllThyMoves getAllMovesPieceWithTarget board $ nextTurn turn
      movesWhite = filter ((/= King).piecetype.fst.fst) $ getAllThyMoves getAllMovesPieceDropTarget board turn
      allWhiteMoves = getAllMoves movesWhite
      perhapsWinMessage = checkEndGame movesBlack allWhiteMoves cgs

  checkDraw :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> [((Piece, Coordinate_t), [[Coordinate_t]])] -> Bool
  checkDraw [] [((Piece Bishop _ _, _),_)] = True
  checkDraw [((Piece Bishop _ _, _),_)] [] = True
  checkDraw [] [((Piece Knight _ _, _),_)] = True
  checkDraw [((Piece Knight _ _, _),_)] [] = True
  checkDraw [((Piece Bishop _ _,Coordinate x1 y1),_)] [((Piece Bishop _ _,Coordinate x2 y2),_)] = even $ x1 + y1 + x2 + y2
  checkDraw _ _ = False

  undo :: ChessGameWorld -> ChessGameWorld
  undo cgw@ChessGameOngoing{history=[]} = cgw

  undo cgw@ChessGameOngoing{activeAI=True,history=[_]} = cgw
  undo cgw@ChessGameOngoing{activeAI=True} = undo . undo $ cgw

  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Nothing):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece board historyRest endC startC NoPiece

  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just (Capture cp)):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece board historyRest endC startC $ recreatePiece historyRest endC turn cp

  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just (Promotion cp)):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece (setPiece board endC $ Piece Pawn (nextTurn turn) False) historyRest endC startC $ maybe NoPiece (recreatePiece historyRest startC turn) cp

  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just CastlingL):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where
      crum = goTo board endC
      updatedChessGameState = ChessGameState (nextTurn turn) $ getBoard $ setRow crum $ uncastleLeft $ getRow crum

  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just CastlingR):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where 
      crum = goTo board endC
      updatedChessGameState = ChessGameState (nextTurn turn) $ getBoard $ setRow crum $ uncastleRight $ getRow crum

  recreatePiece :: History -> Coordinate_t -> Side -> PieceType -> Piece
  recreatePiece _ (Coordinate x 1) Black Pawn = Piece Pawn Black True
  recreatePiece _ (Coordinate x 6) White Pawn = Piece Pawn White True
  recreatePiece history c s Rook = Piece Rook s $ not $ any ((== c) . snd4) history
  recreatePiece history c s King = Piece King s $ not $ any ((== c) . snd4) history
  recreatePiece _ _ s p = Piece p s False
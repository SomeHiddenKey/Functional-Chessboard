{-# LANGUAGE RecordWildCards #-}

module BoardLogic where  
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
  import Utils (Coordinate(..), Coordinate_t, getUntilElement, toFst, toSnd, concatJust,flatTupple,maxWith,minWith,consIf,parMaxWith,parMinWith,(&&&))
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),History,HistoryModifier(..),Moves,nextTurn,nextGameState,pieceValue)
  import BoardMovement

  -- generator to get all available squares per given direction, only including the last coordiante if the piece is of the opposite colour 
  -- used to see all movable locations, including capturing an opponent's piece
  getAllMovesPieceDropTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceDropTarget = getAllMovesPiece availableSquare

  -- generator to get all available squares per given direction, including the last piece regardless of its side
  -- used to include the coordinate of a piece protecting another piece
  getAllMovesPieceWithTarget :: Piece -> BoardCrumb -> [[Coordinate_t]]
  getAllMovesPieceWithTarget = getAllMovesPiece $ const . const True

  -- generate all moves for the current state, by flattening the list of possible coordinates per individual direction 
  getMovesForSide :: ChessGameState -> [((Piece, Coordinate_t), [Coordinate_t])]
  getMovesForSide (ChessGameState turn board) = map (fmap concat) $ generateAllMoves getAllMovesPieceDropTarget board turn
  
  -- generate all possible Commands to available squares
  getAllMovesFor :: Board -> Side -> [(Coordinate_t, Coordinate_t)]
  getAllMovesFor board turn = flatTupple $ getCoordinate &&& concat . (uncurry getAllMovesPieceDropTarget) . toFst getElement <$> getAllCrumbs board turn

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
  checkCastlingLeft [(Piece Rook Black True), NoPiece, NoPiece, NoPiece, (Piece King Black True), a, b, c] = Right [NoPiece, NoPiece, (Piece King Black False), (Piece Rook Black False), NoPiece, a, b, c]
  checkCastlingLeft [(Piece Rook White True), NoPiece, NoPiece, NoPiece, (Piece King White True), a, b, c] = Right [NoPiece, NoPiece, (Piece King White False), (Piece Rook White False), NoPiece, a, b, c]
  checkCastlingLeft _ = Left "Illegal move"

  checkCastlingRight :: [Piece] -> Either String [Piece]
  checkCastlingRight [a, b, c, d, (Piece King Black True), NoPiece , NoPiece, (Piece Rook Black True)] = Right [a, b, c, d, NoPiece , (Piece Rook Black False) , (Piece King Black False), NoPiece]
  checkCastlingRight [a, b, c, d, (Piece King White True), NoPiece , NoPiece, (Piece Rook White True)] = Right [a, b, c, d, NoPiece , (Piece Rook White False) , (Piece King White False), NoPiece]
  checkCastlingRight _ = Left "Illegal move"

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
  boardValue = foldr (add.fst) 0 . concat . (<*>) [flip getAllElementsOf White, flip getAllElementsOf Black] . (:[]) where
    add (Piece piecetype Black _) x = pieceValue piecetype `seq` x - pieceValue piecetype `seq` x - pieceValue piecetype
    add (Piece piecetype White _) x = pieceValue piecetype `seq` x + pieceValue piecetype `seq` x + pieceValue piecetype

  checkLegality :: Set Coordinate_t -> Coordinate_t -> Bool
  checkLegality = flip member

  getKingCoordinate :: ChessGameState -> Side -> Coordinate_t
  getKingCoordinate ChessGameState{..} side = head [c | ((Piece King _ _), c) <- getAllElementsOf board side] 

  getBestMoveUncached :: Int -> ChessGameState -> (Int, (ChessGameState, (Coordinate_t, Coordinate_t)))
  getBestMoveUncached 0 cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = (getBestBoardValue turn, (cgs , (Coordinate 0 0, Coordinate 0 0))) 
    | otherwise = (boardValue currentBoard, (cgs , (Coordinate 0 0, Coordinate 0 0)))
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  getBestMoveUncached depth cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = (getBestBoardValue turn, (cgs , (Coordinate 0 0, Coordinate 0 0))) 
    | otherwise = bestWith' (fst . (getBestMoveUncached (depth - 1)) . fst) $ map (toFst $ nextGameState cgs . uncurry (movePiece currentBoard)) allMoves where
      allMoves = getAllMovesFor currentBoard turn
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn
      bestWith'
        | turn == Black && (depth > 3) = parMinWith
        | turn == Black = minWith
        | turn == White && (depth > 3) = parMaxWith
        | otherwise = maxWith

  type Command = (Coordinate_t,Coordinate_t)      
  data MovesTree 
    = Node {nodeTurn :: Side, nextMoves :: [(Command,MovesTree)]} 
    | Leaf {leafState :: ChessGameState}
    | Bottom {bottomValue :: Int}
    deriving(Show)

  buildMovesTree :: Int -> ChessGameState -> (Int,MovesTree)
  buildMovesTree 0 cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = toSnd Bottom $ getBestBoardValue turn
    | otherwise = (boardValue currentBoard, Leaf cgs)
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  buildMovesTree depth cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = toSnd Bottom $ getBestBoardValue turn
    | otherwise = fmap (Node turn . (fmap $ fmap snd)) $ toFst (fst . bestWith turn (fst . snd)) $ map (toSnd $ buildMovesTree (depth - 1) . nextGameState cgs . uncurry (movePiece currentBoard)) $ getAllMovesFor currentBoard turn
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  traverseTree :: MovesTree -> (Int,MovesTree)
  traverseTree Node{nodeTurn,nextMoves} = fmap (Node nodeTurn . (fmap $ fmap snd)) $ toFst (fst . bestWith nodeTurn (fst . snd)) $ map (fmap traverseTree) nextMoves

  traverseTree Leaf{leafState} = buildMovesTree 2 leafState

  traverseTree Bottom{bottomValue} = toSnd Bottom bottomValue

  getBestMoveCached :: Int -> MovesTree -> (Command, MovesTree)
  getBestMoveCached depth (Leaf cgs@(ChessGameState turn currentBoard)) = fmap snd $ snd . bestWith turn (fst . snd) $ map (toSnd $ buildMovesTree depth . nextGameState cgs . uncurry (movePiece currentBoard)) $ getAllMovesFor currentBoard turn

  getBestMoveCached _ Node{nodeTurn,nextMoves} = fmap snd $ snd . bestWith nodeTurn (fst . snd) $ map (fmap traverseTree) nextMoves

  bestWith :: Side -> (a -> Int) -> [a] -> (Int,a)
  bestWith Black = minWith
  bestWith White = maxWith

  promptAImoveUncached :: ChessGameWorld -> ChessGameWorld
  promptAImoveUncached cgo@ChessGameOngoing{..} = ChessGameOngoing newcgs Nothing True ((piecetype pieceEnd,startC,endC,mod) : history) "" False $ getMovesForSide newcgs
    where 
      (newcgs, (startC, endC)) = snd $ getBestMoveUncached 3 gameState
      pieceStart = getElement $ (flip goTo) startC $ board gameState
      mod = if pieceStart == NoPiece then Nothing else Just $ Capture $ piecetype pieceStart
      pieceEnd = getElement $ (flip goTo) endC $ board gameState

  promptAImoveCached :: ChessGameWorld -> MovesTree -> (MovesTree,ChessGameWorld)
  promptAImoveCached cgo@ChessGameOngoing{..} tree = (,) newTree $ ChessGameOngoing newcgs Nothing True ((piecetype pieceEnd,startC,endC,mod) : history) "" False $ getMovesForSide newcgs
    where 
      ((startC, endC), newTree) = getBestMoveCached 3 tree
      newcgs = nextGameState gameState $ movePiece (board gameState) startC endC
      pieceStart = getElement $ (flip goTo) startC $ board gameState
      mod = if pieceStart == NoPiece then Nothing else Just $ Capture $ piecetype pieceStart
      pieceEnd = getElement $ (flip goTo) endC $ board gameState

  testMoveValidity :: Coordinate_t -> ChessGameWorld -> (ChessGameState,(PieceType,Maybe HistoryModifier)) -> ChessGameWorld
  testMoveValidity c (ChessGameOngoing gs (Just sq) activeAI hs _ False pm) (cgs@(ChessGameState turn board), (pt, mod))
    | isJust perhapsWinMessage = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) (fromJust perhapsWinMessage) True []
    | checkDraw movesWhite $ filter ((/= King).piecetype.fst.fst) movesBlack = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) "Draw by insufficient material" True []
    | checkLegality allWhiteMoves $ getKingCoordinate cgs $ nextTurn turn =
      ChessGameOngoing gs Nothing activeAI hs "can't put yourself in check" False pm
    | otherwise = 
      ChessGameOngoing cgs Nothing activeAI ((pt,sq,c,mod) : hs) "" False $ getMovesForSide cgs
    where
      movesBlack = generateAllMoves getAllMovesPieceWithTarget board $ nextTurn turn
      movesWhite = filter ((/= King).piecetype.fst.fst) $ generateAllMoves getAllMovesPieceDropTarget board turn
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

  uncastleLeft :: [Piece] -> [Piece]
  uncastleLeft [NoPiece, NoPiece, king, rook, NoPiece, a, b, c] = [rook{firstMove=True}, NoPiece, NoPiece, NoPiece, king{firstMove=True}, a, b, c]
  
  uncastleRight :: [Piece] -> [Piece]
  uncastleRight [a, b, c, d, NoPiece , rook , king, NoPiece] = [a, b, c, d, king{firstMove=True}, NoPiece , NoPiece, rook{firstMove=True}]
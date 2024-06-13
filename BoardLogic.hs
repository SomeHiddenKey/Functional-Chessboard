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
  import Utils (Coordinate(..), Coordinate_t, getUntilElement, toFst, toSnd, concatJust,flatTupple,maxWith,minWith,consIf,parMaxWith,parMinWith,(&&&),first)
  import Board(Side(..),PieceType(..),Piece(..),Board,ChessGameState(..),ChessGameWorld(..),History,HistoryItem,HistoryModifier(..),Moves,nextTurn,nextGameState,pieceValue)
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
  
  -- makes sure a given command for the current word is correct by the rules. Gives a string back in case it isn't. 
  -- The extra pass to (testMoveValidity) will make sure to check we aren't putting ourself in check or having reached any end positions (draws/wins)
  checkMove :: ChessGameWorld -> Coordinate_t -> Coordinate_t -> Either String ChessGameWorld
  checkMove cgo@ChessGameOngoing{gameState=ChessGameState{..},..} startC@(Coordinate xstart ystart) endC@(Coordinate xend yend)
    -- a promotion position is checked separately for a pawn reaching the back rank. Also makes sure we are actually moving a piece of our own colour
    -- history registers a new Promotion with any captured piece, if any. start square is cleared and the end square recieves a new queen piece
    | checkPromotion startC endC startPiece startPieceCrumb && (turn == playSide startPiece) = 
      testMoveValidity cgo (piecetype startPiece, startC, endC, Just $ Promotion endPieceCaptured) 
      $ ChessGameState (nextTurn turn) 
      $ setPiece (setPiece board endC (Piece Queen (playSide startPiece) False)) startC NoPiece

    -- en-passant position is checked separately for a pawn moving behind a newly double moved pawn. Also makes sure we are actually moving a piece of our own colour
    -- history registers a new Capture with the captured piece. start square and captured square are cleared and the end square recieves the moved pawn piece
    | checkEnPassant startC endC startPiece history && (turn == playSide startPiece) = 
      testMoveValidity cgo (piecetype startPiece, startC, endC, Capture <$> endPieceCaptured) 
      $ ChessGameState (nextTurn turn) 
      $ setPiece (movePiece board startC endC) (Coordinate xend ystart) NoPiece 

    -- a standard move is checked separately for a piece using (checkMovePiece). Also makes sure we are actually moving a piece of our own colour
    -- history registers a new Capture with the captured piece. start square and captured square are cleared and the end square recieves the moved pawn piece
    | (availableSquare turn endPiece) 
      && (checkMovePiece startPiece ((-) <$> abs <$> endC <*> startC) startPieceCrumb) 
      && (turn == playSide startPiece) 
      = testMoveValidity cgo (piecetype startPiece, startC, endC, Capture <$> endPieceCaptured) $ ChessGameState (nextTurn turn) $ movePiece board startC endC 
    where           
    startPieceCrumb = goTo board startC
    startPiece = getElement startPieceCrumb
    endPiece = getElement $ goTo board endC
    endPieceCaptured = if endPiece == NoPiece then Nothing else Just $ piecetype endPiece
  
  -- check for the specific casting conditions for specific Commands and comparing their row to a template using checkCastlingLeft or checkCastlingRight
  checkMove cgo@ChessGameOngoing{gameState=(ChessGameState Black board),..} startC@(Coordinate 4 0) endC@(Coordinate 2 0) = 
     (testMoveValidity cgo (King, startC, endC, Just CastlingL)) =<< ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastlingLeft $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove cgo@ChessGameOngoing{gameState=(ChessGameState Black board),..} startC@(Coordinate 4 0) endC@(Coordinate 6 0) = 
     (testMoveValidity cgo (King, startC, endC, Just CastlingR)) =<< ChessGameState White <$> getBoard <$> (setRow crum) <$> (checkCastlingRight $ getRow crum) where crum = goTo board (Coordinate 0 0) 
  checkMove cgo@ChessGameOngoing{gameState=(ChessGameState White board),..} startC@(Coordinate 4 7) endC@(Coordinate 2 7) = 
     (testMoveValidity cgo (King, startC, endC, Just CastlingL)) =<< ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastlingLeft $ getRow crum) where crum = goTo board (Coordinate 0 7) 
  checkMove cgo@ChessGameOngoing{gameState=(ChessGameState White board),..} startC@(Coordinate 4 7) endC@(Coordinate 6 7) = 
     (testMoveValidity cgo (King, startC, endC, Just CastlingR)) =<< ChessGameState Black <$> getBoard <$> (setRow crum) <$> (checkCastlingRight $ getRow crum) where crum = goTo board (Coordinate 0 7)
  
  -- any other given command not accepted by the others is considered invalid
  checkMove _ _ _ = Left "Illegal move"

  -- Checks the moving condition of a normal move based on its type
  checkMovePiece :: Piece -> Coordinate_t -> BoardCrumb -> Bool
  -- rook can move when x or y is equal to 0 and that all squares on its path are empty, using (checkAllEmpty)
  checkMovePiece (Piece Rook playSide _) c@(Coordinate 0 ydif) crum = checkAllEmpty (abs $ ydif) crum c
  checkMovePiece (Piece Rook playSide _) c@(Coordinate xdif 0) crum = checkAllEmpty (abs $ xdif) crum c
  
  -- bishop can move when the absulte of x and y are equal to one another and that all squares on its path are empty, using (checkAllEmpty)
  checkMovePiece (Piece Bishop playSide _) c@(Coordinate xdif ydif) crum
    | (abs $ xdif)==(abs $ ydif) = checkAllEmpty (abs $ ydif) crum c
    | otherwise = False
  
  -- bishop can move in any direction the rook or bishop can
  checkMovePiece (Piece Queen playSide firstMove) c crum
    | checkMovePiece (Piece Rook playSide firstMove) c crum = True
    | checkMovePiece (Piece Bishop playSide firstMove) c crum = True
    | otherwise = False

  -- first move on a pawn can move twice in the opponent's direction as long as both squares are empty
  checkMovePiece (Piece Pawn Black True) c@(Coordinate 0 2) crum = checkAllEmpty 3 crum c
  checkMovePiece (Piece Pawn White True) c@(Coordinate 0 (-2)) crum = checkAllEmpty 3 crum c

  -- a pawn can move once in the opponent's direction as long as both squares are empty
  checkMovePiece (Piece Pawn Black _) c@(Coordinate 0 1) crum = checkAllEmpty 2 crum c
  checkMovePiece (Piece Pawn White _) c@(Coordinate 0 (-1)) crum = checkAllEmpty 2 crum c

  -- a pawn can move once diagonally in the opponent's direction as long as it camptures an opponent doing so. This is done by comparing oppositeSquare with getElement
  checkMovePiece (Piece Pawn Black _) c@(Coordinate 1 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn Black _) c@(Coordinate (-1) 1) crum = or $ oppositeSquare Black <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate 1 (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)
  checkMovePiece (Piece Pawn White _) c@(Coordinate (-1) (-1)) crum = or $ oppositeSquare White <$> getElement <$> (moveBy crum c)

  -- a king can move one move in any direction, that's why we take the absolute value. The end-square is empty, which is already checked in (checkMove)
  checkMovePiece (Piece King _ _) (Coordinate xdif ydif) crum = (abs $ xdif) < 2 && (abs $ ydif) < 2

  -- a knight can move 2 in one axis and once in the other axis for any given direction, hence we take the absolute value
  checkMovePiece (Piece Knight _ _) (Coordinate xdif ydif) crum 
    | (==) [1,2] $ abs <$> [xdif,ydif] = True
    | (==) [2,1] $ abs <$> [xdif,ydif] = True
    | otherwise = False

  -- any other combination of moves is not a legal move
  checkMovePiece a b c = False

  -- a black pawn on the 6th rank or a white pawn on the 1st rank can promote to a queen either by :
  -- 1. advancing forward if the square is empty -> first condition with (checkAllEmpty)
  -- 2. capturing a piece once diagonically left or right of the square -> second condition with (oppositeSquare)
  checkPromotion :: Coordinate_t -> Coordinate_t -> Piece -> BoardCrumb -> Bool
  checkPromotion (Coordinate xstart 6) (Coordinate xend 7) (Piece Pawn Black _) crum
    | xstart == xend = checkAllEmpty 2 crum $ Coordinate 0 1
    | (==1) . abs $ xend - xstart = or $ oppositeSquare Black <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) 1))
    | otherwise = False
  checkPromotion (Coordinate xstart 1) (Coordinate xend 0) (Piece Pawn White _) crum
    | xstart == xend = checkAllEmpty 2 crum $ Coordinate 0 (-1)
    | (==1) . abs $ xend - xstart = or $ oppositeSquare White <$> getElement <$> (moveBy crum (Coordinate (xend - xstart) (-1)))
    | otherwise = False
  checkPromotion _ _ _ _ = False

  -- a pawn can capture an opposing pawn be going behind it only if the last history move is a pawn moving forward by 2 squares
  checkEnPassant :: Coordinate_t -> Coordinate_t -> Piece -> History -> Bool
  checkEnPassant (Coordinate xstart 4) (Coordinate xend 5) (Piece Pawn Black _) ((Pawn, (Coordinate x 6), (Coordinate _ 4), _):_) = 
    (&&) (xend==x) $ (==1) . abs $ xend - xstart 
  checkEnPassant (Coordinate xstart 3) (Coordinate xend 2) (Piece Pawn White _) ((Pawn, (Coordinate x 1), (Coordinate _ 3), _):_) = 
    (&&) (xend==x) $ (==1) . abs $ xend - xstart
  checkEnPassant _ _ _ _ = False

  -- check the 2nd or 6th rank row with the given castling templates for a left castle, returning the castled row if applicable
  checkCastlingLeft :: [Piece] -> Either String [Piece]
  checkCastlingLeft [(Piece Rook Black True), NoPiece, NoPiece, NoPiece, (Piece King Black True), a, b, c] = 
    Right [NoPiece, NoPiece, (Piece King Black False), (Piece Rook Black False), NoPiece, a, b, c]
  checkCastlingLeft [(Piece Rook White True), NoPiece, NoPiece, NoPiece, (Piece King White True), a, b, c] = 
    Right [NoPiece, NoPiece, (Piece King White False), (Piece Rook White False), NoPiece, a, b, c]
  checkCastlingLeft _ = Left "Illegal move"

  -- check the 2nd or 6th rank row with the given castling templates for a right castle, returning the castled row if applicable
  checkCastlingRight :: [Piece] -> Either String [Piece]
  checkCastlingRight [a, b, c, d, (Piece King Black True), NoPiece , NoPiece, (Piece Rook Black True)] = 
    Right [a, b, c, d, NoPiece , (Piece Rook Black False) , (Piece King Black False), NoPiece]
  checkCastlingRight [a, b, c, d, (Piece King White True), NoPiece , NoPiece, (Piece Rook White True)] = 
    Right [a, b, c, d, NoPiece , (Piece Rook White False) , (Piece King White False), NoPiece]
  checkCastlingRight _ = Left "Illegal move"

  -- concat all moves for a specific piece at a given crumb per direction
  -- the validator is used to differentiate adding a piece of your own team or not (see which pieces cover each other => king can't take a protected piece)
  getAllMovesPiece :: (Side -> Piece -> Bool) -> Piece -> BoardCrumb -> [[Coordinate_t]]

  -- king can move in any direction around itself that is empty or available. if it's its first move, check and conditionally cons the coordinates for castling as well
  getAllMovesPiece validator p@(Piece King turn firstMove) crum
    | firstMove = addCastleL $ addCastleR [
      [transformPos $ Coordinate x y]| 
      x <- [(-1)..1], 
      y <- [(-1)..1], 
      x/=0 || y/=0,
      checkAvailableSquareAt turn crum validator $ Coordinate x y]
    | otherwise = [
      [transformPos $ Coordinate x y]| 
      x <- [(-1)..1], 
      y <- [(-1)..1], 
      x/=0 || y/=0,
      checkAvailableSquareAt turn crum validator $ Coordinate x y]
      where 
        transformPos =  (<*>) $! (+) <$> getCoordinate crum
        addCastleL = consIf (isRight $ checkCastlingLeft $ getRow crum) $ [transformPos $ Coordinate (-2) 0]
        addCastleR = consIf (isRight $ checkCastlingRight $ getRow crum) $ [transformPos $ Coordinate 2 0]

  -- Bishop can move in all vertical/horizontal directions, accumulated until it hits outside the board or any piece
  getAllMovesPiece validator p@(Piece Bishop turn _) crum  = 
      getMovesInDirection turn crum validator [Coordinate x y | x <- [(-1),1], y <- [(-1),1]]

  -- Rook can move in all diagonal directions, accumulated until it hits outside the board or any piece
  getAllMovesPiece validator p@(Piece Rook turn _) crum  = 
      getMovesInDirection turn crum validator $ [Coordinate 0 y |y <- [(-1),1]] ++ [Coordinate x 0 |x <- [(-1),1]]

  -- Rook can move in all directions, accumulated until it hits outside the board or any piece
  getAllMovesPiece validator p@(Piece Queen turn _) crum  = 
      getMovesInDirection turn crum validator [Coordinate x y | x <- [(-1)..1], y <- [(-1)..1], x/=0 || y/=0]

  -- Knight can move to any available location moving 1 step on one axis and 2 on the other axis. We add the direction to the current location with (transformPos)
  getAllMovesPiece validator p@(Piece Knight turn _) crum  = 
      [[transformPos $ Coordinate x y] | x <- [(-1),1], y <- [(-2),2], checkAvailableSquareAt turn crum validator $ Coordinate x y] ++
      [[transformPos $ Coordinate x y] | y <- [(-1),1], x <- [(-2),2], checkAvailableSquareAt turn crum validator $ Coordinate x y] 
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum

  -- a pawn can move once/twice forward if those squares are empty and once diagonally if there is an opposing piece there using (checkMovePiece)
  getAllMovesPiece validator p@(Piece Pawn Black _) crum  = 
      [[transformPos $ Coordinate x 1] | x <- [(-1),1], not $ checkAllEmpty 2 crum $ Coordinate x 1, checkAvailableSquareAt Black crum validator $ Coordinate x 1] ++ 
      [[transformPos $ Coordinate 0 y] | y <- [1,2], checkMovePiece p (Coordinate 0 y) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece validator p@(Piece Pawn White _) crum =
      [[transformPos $ Coordinate x (-1)] | x <- [(-1),1], not $ checkAllEmpty 2 crum $ Coordinate x (-1), checkAvailableSquareAt White crum validator $ Coordinate x (-1)] ++ 
      [[transformPos $ Coordinate 0 y] | y <- [(-1),(-2)], checkMovePiece p (Coordinate 0 y) crum]
      where transformPos =  (<*>) $ (+) <$> getCoordinate crum
  getAllMovesPiece _ _ _ = [[]]

  -- flattens and accumulates the coordinates from (getAllMovesPiece) using a set to avoid duplicates
  getAllMoves :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t
  getAllMoves c = foldr combineSets S.empty $ map (($!) fromList . concat . snd) c where 
    combineSets movesA movesB = movesA `seq` movesB `seq` union movesA movesB
  
  -- intersect the opponents movesets with movepaths starting from a piece and reaching the opponents king. 
  -- If the path does not reach the king, it is empty, as checked by (getUntilElement) 
  checkMateBlockers :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t -> Coordinate_t -> [Set Coordinate_t]
  checkMateBlockers attackingMoves allDefensiveMoves king_coordinate = intersection allDefensiveMoves <$> concatJust union S.empty . map ((fmap fromList) . getUntilElement king_coordinate) <$> map snd attackingMoves

  -- check if we have hit an end condition. Returns a display message if that's the case, else None
  checkEndGame :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> Set Coordinate_t -> ChessGameState -> Maybe String
  checkEndGame attackingMoves allDefensiveMoves cgs@(ChessGameState turn board)
    -- defender can't move -> its either stuck in stalemate or checkmate, depending if the king is being targetted (using the Set difference)
    | null allDefensiveMoves && (null $ difference kingMovesWhite allMovesBlack) = 
      if member kingCorWhite allMovesBlack
      then Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ " won"
      else Just $ "Stalemate: Draw"
    -- if the king has no-where to move out of the check and no piece is there to block the attacking path, then it's check mate
    | (isSubsetOf kingMovesWhite allMovesBlack) && (all null $ checkMateBlockers attackingMoves allDefensiveMoves kingCorWhite) = 
      Just $ "Checkmate: " ++ (show $ nextTurn turn) ++ " won"
    | otherwise = Nothing
    where 
      kingCorWhite = getKingCoordinate cgs turn
      kingMovesWhite = fromList $ (:) kingCorWhite $ concat $ getAllMovesPieceDropTarget (Piece King turn False) $ goTo board kingCorWhite
      allMovesBlack = getAllMoves attackingMoves

  -- folding the piece values of both side's elements together. White sums, Black subtracts
  boardValue :: Board -> Int
  boardValue = foldr (add.fst) 0 . concat . (<*>) [flip getAllElementsOf White, flip getAllElementsOf Black] . (:[]) where
    add (Piece piecetype Black _) x = pieceValue piecetype `seq` x - pieceValue piecetype `seq` x - pieceValue piecetype
    add (Piece piecetype White _) x = pieceValue piecetype `seq` x + pieceValue piecetype `seq` x + pieceValue piecetype

  -- get the coordinate of the king for the given side
  getKingCoordinate :: ChessGameState -> Side -> Coordinate_t
  getKingCoordinate ChessGameState{..} side = first (Coordinate 0 0) [c | ((Piece King _ _), c) <- getAllElementsOf board side] 

  -- get the best move with the given depth using a min-max algorithm and not caching any of the intermediate states
  getBestMoveUncached :: Int -> ChessGameState -> (Int, (ChessGameState, (Coordinate_t, Coordinate_t)))
  -- the bottom depth just makes sure that we are a valid position and not an end-position with value +/- infinity. Otherwise return the current game state and its board value
  getBestMoveUncached 0 cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = (getBestBoardValue turn, (cgs , (Coordinate 0 0, Coordinate 0 0))) 
    | otherwise = (boardValue currentBoard, (cgs , (Coordinate 0 0, Coordinate 0 0)))
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  -- makes ure we are a valid position and not an end-position with value +/- infinity. Otherwise, return the maximum/minimum boardvalue from it's childs
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

  -- caching tree data construct
  type Command = (Coordinate_t,Coordinate_t)      
  data MovesTree 
    = Node {nodeTurn :: Side, nextMoves :: [(Command,MovesTree)]} 
    | Leaf {leafState :: ChessGameState}
    | Bottom {bottomValue :: Int}
    deriving(Show)

  -- builds the caching tree from an initial game state, returning the best value and its cached tree
  buildMovesTree :: Int -> ChessGameState -> (Int,MovesTree)
  -- makes ure we are a valid position and not a Bottom with value +/- infinity. Otherwise return the current game state Leaf and its board value
  buildMovesTree 0 cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = toSnd Bottom $ getBestBoardValue turn
    | otherwise = (boardValue currentBoard, Leaf cgs)
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  -- makes ure we are a valid position and not a Bottom with value +/- infinity. 
  -- Otherwise return the tree Node containing all it's next moves a nd the coordinates to get there as children, as well as the optimal board value, depending on its side
  buildMovesTree depth cgs@(ChessGameState turn currentBoard)
    | member kingCorOpp $ getAllMoves $ generateAllMoves getAllMovesPieceWithTarget currentBoard turn = toSnd Bottom $ getBestBoardValue turn
    | otherwise = fmap (Node turn . (fmap $ fmap snd)) $ toFst (fst . bestWith turn (fst . snd)) $ map (toSnd $ buildMovesTree (depth - 1) . nextGameState cgs . uncurry (movePiece currentBoard)) $ getAllMovesFor currentBoard turn
    where
      kingCorOpp = getKingCoordinate cgs $ nextTurn turn

  -- traverse the cached tree, expanding its leaves by a new depth of 2 and returning the updated tree with its best best
  traverseTree :: MovesTree -> (Int,MovesTree)
  traverseTree Node{nodeTurn,nextMoves} = fmap (Node nodeTurn . (fmap $ fmap snd)) $ toFst (fst . bestWith nodeTurn (fst . snd)) $ map (fmap traverseTree) nextMoves

  traverseTree Leaf{leafState} = buildMovesTree 2 leafState

  traverseTree Bottom{bottomValue} = toSnd Bottom bottomValue

  -- calls cached tree traversal, returning the best next move with the updated tree
  getBestMoveCached :: Int -> MovesTree -> (Command, MovesTree)
  getBestMoveCached depth (Leaf cgs@(ChessGameState turn currentBoard)) = fmap snd $ snd . bestWith turn (fst . snd) $ map (toSnd $ buildMovesTree depth . nextGameState cgs . uncurry (movePiece currentBoard)) $ getAllMovesFor currentBoard turn

  getBestMoveCached _ Node{nodeTurn,nextMoves} = fmap snd $ snd . bestWith nodeTurn (fst . snd) $ map (fmap traverseTree) nextMoves

  -- Black will aim towards a minimum board value for its turn and vice versa for White
  bestWith :: Side -> (a -> Int) -> [a] -> (Int,a)
  bestWith Black = minWith
  bestWith White = maxWith

  -- calls for and applies the best next AI move using uncached min-max lookup
  promptAImoveUncached :: ChessGameWorld -> ChessGameWorld
  promptAImoveUncached cgo@ChessGameOngoing{..} = ChessGameOngoing newcgs Nothing True ((piecetype pieceStart,startC,endC,mod) : history) msg False $ getMovesForSide newcgs
    where 
      (newcgs, (startC, endC)) = snd $ getBestMoveUncached 4 gameState
      msg = "AI moved " ++ show startC ++ " to " ++ show endC
      pieceStart = getElement $ (flip goTo) startC $ board gameState
      pieceEnd = getElement $ (flip goTo) endC $ board gameState
      mod = if pieceEnd == NoPiece then Nothing else Just $ Capture $ piecetype pieceEnd

  -- calls for and applies the best next AI move using the cached tree, returing the updated world and updated tree
  promptAImoveCached :: ChessGameWorld -> MovesTree -> (MovesTree,ChessGameWorld)
  promptAImoveCached cgo@ChessGameOngoing{..} tree = (,) newTree $ ChessGameOngoing newcgs Nothing True ((piecetype pieceEnd,startC,endC,mod) : history) "" False $ getMovesForSide newcgs
    where 
      ((startC, endC), newTree) = getBestMoveCached 3 tree
      newcgs = nextGameState gameState $ movePiece (board gameState) startC endC
      pieceStart = getElement $ (flip goTo) startC $ board gameState
      pieceEnd = getElement $ (flip goTo) endC $ board gameState
      mod = if pieceEnd == NoPiece then Nothing else Just $ Capture $ piecetype pieceEnd

  -- check if the king's coordinate is part of the attacking set of possible moves
  isKingHit :: Set Coordinate_t -> Coordinate_t -> Bool
  isKingHit = flip member

  -- test if the world has hit any invalid- or end- position, returning the old or new state depending on its validity
  testMoveValidity :: ChessGameWorld -> HistoryItem -> ChessGameState -> Either String ChessGameWorld
  testMoveValidity ChessGameOngoing{..} historyItem new_cgs@(ChessGameState turn board)
    -- an endstate displays its message and turns on the end-state flag
    | isJust endStateMessage = 
      Right $ ChessGameOngoing new_cgs Nothing activeAI (historyItem : history) (fromJust endStateMessage) True []
    -- a force-draw turns on the end-state flag and shows the end-state message
    | checkDraw movesWhite $ filter ((/= King).piecetype.fst.fst) movesBlack = 
      Right $ ChessGameOngoing new_cgs Nothing activeAI (historyItem : history) "Draw by insufficient material" True []
    -- you can't leave your king in check, if it isn't a checkmate (per prior condition check). Returns the old state instead
    | isKingHit allWhiteMoves $ getKingCoordinate new_cgs $ nextTurn turn =
      Left "can't put yourself in check"
    -- no corner-cases will simply update the ongoing world with the new history
    | otherwise = 
      Right $ ChessGameOngoing new_cgs Nothing activeAI (historyItem : history) "" False $ getMovesForSide new_cgs
    where
      movesBlack = generateAllMoves getAllMovesPieceWithTarget board $ nextTurn turn
      movesWhite = filter ((/= King).piecetype.fst.fst) $ generateAllMoves getAllMovesPieceDropTarget board turn
      allWhiteMoves = getAllMoves movesWhite
      endStateMessage = checkEndGame movesBlack allWhiteMoves new_cgs

  -- check if all Side's pieces hit any of the forced-draw conditions. Kings are left out because they always appear on the board
  checkDraw :: [((Piece, Coordinate_t), [[Coordinate_t]])] -> [((Piece, Coordinate_t), [[Coordinate_t]])] -> Bool
  checkDraw [] [((Piece Bishop _ _, _),_)] = True
  checkDraw [((Piece Bishop _ _, _),_)] [] = True
  checkDraw [] [((Piece Knight _ _, _),_)] = True
  checkDraw [((Piece Knight _ _, _),_)] [] = True
  -- check oppositte bishops by comparing their coordinates
  checkDraw [((Piece Bishop _ _,Coordinate x1 y1),_)] [((Piece Bishop _ _,Coordinate x2 y2),_)] = even $ x1 + y1 + x2 + y2 
  checkDraw _ _ = False

  -- go back one move in the history, depending on the last history's type
  undo :: ChessGameWorld -> ChessGameWorld

  -- no history applies no change
  undo cgw@ChessGameOngoing{history=[]} = cgw

  -- undoing the first move made by the AI is not possible and applies no change
  undo cgw@ChessGameOngoing{activeAI=True,history=[_]} = cgw
  -- undoing a move with an AI should automatically remove the previous AI move and yours (so applied twice as non-AI undo)
  undo cgw@ChessGameOngoing{activeAI=True} = (undo . undo $ cgw{activeAI=False}){activeAI=True}

  -- undoing a normal move (no modifier) makes that we reset the piece back to its original place and re-calculate the possible moves
  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Nothing):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece board historyRest endC startC NoPiece

  -- undoing a capturing move makes that we reset the piece back to its original place, recreate and replace the captured piece on its original coordinate and re-calculate the possible moves
  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just (Capture cp)):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece board historyRest endC startC $ recreatePiece historyRest endC turn cp

  -- undoing a promoting move makes that we put either NoPiece or the recreated caputured piece back on its original square while placing a pawn back to the start square and re-calculating the possible moves
  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just (Promotion cp)):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where updatedChessGameState = ChessGameState (nextTurn turn) $ resetPiece (setPiece board endC $ Piece Pawn (nextTurn turn) False) historyRest endC startC $ maybe NoPiece (recreatePiece historyRest startC turn) cp

  -- undoing a left castle applies the uncasting method on the given row and re-calculating the possible moves
  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just CastlingL):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where
      crum = goTo board endC
      updatedChessGameState = ChessGameState (nextTurn turn) $ getBoard $ setRow crum $ uncastleLeft $ getRow crum

  -- undoing a right castle applies the uncasting method on the given row and re-calculating the possible moves
  undo ChessGameOngoing{gameState=ChessGameState{..},history=((pt,startC,endC,Just CastlingR):historyRest),activeAI} = 
    ChessGameOngoing updatedChessGameState Nothing activeAI historyRest "" False $ getMovesForSide updatedChessGameState
    where 
      crum = goTo board endC
      updatedChessGameState = ChessGameState (nextTurn turn) $ getBoard $ setRow crum $ uncastleRight $ getRow crum

  -- matches and shuffles the row of pieces to the position before the left castling, resetting them as firstMove
  uncastleLeft :: [Piece] -> [Piece]
  uncastleLeft [NoPiece, NoPiece, king, rook, NoPiece, a, b, c] = [rook{firstMove=True}, NoPiece, NoPiece, NoPiece, king{firstMove=True}, a, b, c]
  
  -- matches and shuffles the row of pieces to the position before the right castling, resetting them as firstMove
  uncastleRight :: [Piece] -> [Piece]
  uncastleRight [a, b, c, d, NoPiece , rook , king, NoPiece] = [a, b, c, d, king{firstMove=True}, NoPiece , NoPiece, rook{firstMove=True}]
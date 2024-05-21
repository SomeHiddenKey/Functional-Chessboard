module Persistent where
  import Data.List (intercalate)
  import BoardMovement
  import Board
  import Utils
  import Text.Parsec
  import Data.Char (ord)
  import Text.Read (readMaybe)

  serializeHistory :: ChessGameWorld -> String
  serializeHistory = intercalate ", " . map (\(a,b,c,d) -> intercalate " " [show a, show b, show c ++ (maybe "" ((:) ' ') $ show <$> d)]) <. history

  serializePlayer :: ChessGameWorld -> String
  serializePlayer cgw = "Player (" ++ (show . turn . gameState) cgw ++ (if activeAI cgw then " AI" else "")++ ")"

  serializePieces :: ChessGameWorld -> String
  serializePieces cgw = intercalate "\n" .concat $ streamer ([flip getAllElementsOf Black, flip getAllElementsOf White] <*> [(board . gameState) cgw] ) where streamer = map . map $ (\(piece, cor::Coordinate_t) -> intercalate ", " [show $ piecetype piece, show $ playSide piece, show cor])

  serializeGame :: ChessGameWorld ->  IO ChessGameWorld
  serializeGame cgw = do {
    writeFile "test.txt" $ intercalate "\n" [serializePlayer cgw, serializeHistory cgw, serializePieces cgw] ;
    return $ cgw { displayMsg = "game saved" } 
    }

  unserializeGame :: Stream s m Char => ParsecT s u m ChessGameWorld
  unserializeGame = do 
    (side, playstyle) <- parsePlayer
    newline
    history <- parseHistory
    newline
    pieces <- parsePieces history
    let cgs = ChessGameState side $ foldr (\(c,piece) board -> setPiece board c piece) emptyBoard pieces
    return $ ChessGameOngoing cgs Nothing playstyle history "game loaded" False $ getMovesForSide cgs

  parsePieces :: Stream s m Char => History -> ParsecT s u m [(Coordinate_t,Piece)]
  parsePieces history = (flip sepBy) newline $ do 
    spaces
    pt <- parsePiecetype
    side <- char ',' *> spaces *> parseSide
    endC <- char ',' *> spaces *> parseCoordinate
    return $ (,) endC $ recreatePiece pt history endC side
 
  parseHistory :: Stream s m Char => ParsecT s u m History
  parseHistory = do { skipMany $ char ' ' ; pt <- parsePiecetype ; spaces ; startC <- parseCoordinate ; spaces ; endC <- parseCoordinate ; skipMany $ char ' ' ; mod <- optionMaybe (parsecMap Capture parsePiecetype <|> do {string "O-O"; (string "-O" *> return CastlingR) <|> return CastlingL} <|> do {string "=Q"; return Promotion}) ; return (pt, startC, endC, mod)} `sepBy` char ',' 

  parsePlayer :: Stream s m Char => ParsecT s u m (Side,Bool)
  parsePlayer = string "Player" *> spaces *> (between (char '(') (char ')') $ do
    side <- parseSide
    playstyle <- option False $ do {string "AI"; return True}
    return (side, playstyle))

  parsePiecetype :: Stream s m Char => ParsecT s u m PieceType
  parsePiecetype = choice [
    do {char 'k'; (string "ing" *> return King) <|> (string "night" *> return Knight)},
    do {string "queen"; return Queen },
    do {string "pawn"; return Pawn },
    do {string "rook"; return Rook },
    do {string "bishop"; return Bishop }]

  parseSide :: Stream s m Char => ParsecT s u m Side
  parseSide = choice [
    do {string "B"; return Black },
    do {string "W"; return White }]

  parseCoordinate :: Stream s m Char => ParsecT s u m Coordinate_t
  parseCoordinate = do {
    x <- satisfy $ inRange (97,105) . ord ;
    y <- satisfy $ inRange (49,57) . ord ;
    return $ Coordinate (ord x - 97) (56 - ord y) }
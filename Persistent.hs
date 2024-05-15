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
  serializePlayer cgw = "Player (" ++ (show . turn . gameState) cgw ++ ")"

  serializePieces :: ChessGameWorld -> String
  serializePieces cgw = intercalate "\n" .concat $ streamer ([flip getAllElementsOf Black, flip getAllElementsOf White] <*> [(board . gameState) cgw] ) where streamer = map . map $ (\(piece, cor::Coordinate_t) -> intercalate ", " [show $ piecetype piece, show $ playSide piece, show cor])

  serializeGame :: ChessGameWorld ->  IO ChessGameWorld
  serializeGame cgw = do {
    writeFile "test.txt" $ intercalate "\n" [serializePlayer cgw, serializeHistory cgw, serializePieces cgw] ;
    return $ cgw { displayMsg = "game saved" } 
    }

  parseHistory :: Stream s m Char => ParsecT s u m History
  parseHistory = do { pt <- parsePiecetype ; spaces ; startC <- parseCoordinate ; spaces ; endC <- parseCoordinate ; spaces ; mod <- optionMaybe (parsecMap Capture parsePiecetype <|> do {string "O-O"; return Castling} <|> do {string "=Q"; return Promotion}) ; return (pt, startC, endC, mod)} `sepBy` newline 

  parsePlayer :: Stream s m Char => ParsecT s u m Side
  parsePlayer = string "Player" *> spaces *> between (char '(') (char ')') parseSide

  parsePiecetype :: Stream s m Char => ParsecT s u m PieceType
  parsePiecetype = choice [
    do {string "king"; return King },
    do {string "queen"; return Queen },
    do {string "pawn"; return Pawn },
    do {string "knight"; return Knight },
    do {string "rook"; return Rook },
    do {string "bishop"; return Bishop }]

  parseSide :: Stream s m Char => ParsecT s u m Side
  parseSide = choice [
    do {string "B"; return Black },
    do {string "W"; return White }]

  parseCoordinate :: Stream s m Char => ParsecT s u m Coordinate_t
  parseCoordinate = do {
    x <- satisfy $ inRange (97,104) . ord ;
    y <- satisfy $ inRange (49,57) . ord ;
    return $ Coordinate (ord x - 97) (ord y - 49) }
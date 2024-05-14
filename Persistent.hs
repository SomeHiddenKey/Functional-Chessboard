module Persistent where
  import Data.List (intercalate)
  import BoardMovement
  import Board
  import Utils

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
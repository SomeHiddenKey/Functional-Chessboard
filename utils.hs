module Utils(Coordinate(..), getUntilElement, revappend, iterate', toFst, concatJust) where
  import Data.Maybe (fromJust, isJust, catMaybes)

  data Coordinate a = Coordinate { x::a, y::a} deriving(Show, Eq)

  instance Functor Coordinate where
    fmap f (Coordinate x1 y1) = Coordinate (f x1) (f y1)

  instance Ord a => Ord (Coordinate a) where
    compare (Coordinate x1 y1) (Coordinate x2 y2)
      | y1 > y2 = GT
      | x1 > x2 = GT
      | y1 == y2 && x1 == x2 = EQ
      | otherwise = LT

  instance Applicative Coordinate where
    pure e = Coordinate e e 
    (Coordinate fx fy) <*> (Coordinate x y) = Coordinate (fx x) (fy y)

  getUntilElement :: (Eq a) => a -> [a] -> Maybe [a]
  getUntilElement _ [] = Nothing
  getUntilElement e (head:rest)
    | e == head = Just []
    | otherwise = (:) <$> Just head <*> getUntilElement e rest

    -- (>>>) = flip (>>=)
  revappend :: [a] -> [a] -> [a]
  revappend (e:rest) r = e:r `seq` revappend rest (e:r)
  revappend [] r = r

  iterate' :: Monad f => (a -> f a) -> Int -> f a -> f a
  iterate' fun 0 n = n
  iterate' fun i n = (iterate' fun ((abs i) - 1) n ) >>= fun

  toFst :: (a -> b) -> a -> (b, a)
  toFst f e = (f e, e)

  concatJust :: (t1 -> t2 -> t2) -> t2 -> [Maybe t1] -> t2
  concatJust f n = foldr (f $!) n . catMaybes
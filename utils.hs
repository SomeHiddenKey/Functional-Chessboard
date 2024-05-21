module Utils(Coordinate(..),Coordinate_t,getUntilElement, revappend, iterate', toFst,toSnd, concatJust,(&&&), flatTupple,maxWith,minWith, parMaxWith,parMinWith,(<.),snd4,inRange,consIf) where
  import Data.Maybe (fromJust, isJust, catMaybes)
  import Data.Char (chr)
  import Control.Parallel

  data Coordinate a = Coordinate { x::a, y::a} deriving(Eq,Ord)
  type Coordinate_t = Coordinate Int

  instance Functor Coordinate where
    fmap f (Coordinate x1 y1) = Coordinate (f x1) (f y1)

  instance Applicative Coordinate where
    pure e = Coordinate e e 
    (Coordinate fx fy) <*> (Coordinate x y) = Coordinate (fx x) (fy y)

  instance Show Coordinate_t where
    show (Coordinate x y) = [chr $ x + 97, chr $ 56 - y]
    
  getUntilElement :: (Eq a) => a -> [a] -> Maybe [a]
  getUntilElement _ [] = Nothing
  getUntilElement e (head:rest)
    | e == head = Just []
    | otherwise = (:) <$> Just head <*> getUntilElement e rest

  inRange :: (Int,Int) -> Int -> Bool
  inRange (min, max) n = n >= min && n < max

    -- (>>>) = flip (>>=)
  revappend :: [a] -> [a] -> [a]
  revappend (e:rest) r = e:r `seq` revappend rest (e:r)
  revappend [] r = r

  iterate' :: Monad f => (a -> f a) -> Int -> f a -> f a
  iterate' fun 0 n = n
  iterate' fun i n = (iterate' fun ((abs i) - 1) n ) >>= fun

  toFst :: (a -> b) -> a -> (b, a)
  toFst f e = (f e, e)

  toSnd :: (a -> b) -> a -> (a, b)
  toSnd f e = (e, f e)

  snd4 :: (a,b,c,d) -> b
  snd4 (a,b,c,d) = b

  concatJust :: (t1 -> t2 -> t2) -> t2 -> [Maybe t1] -> t2
  concatJust f n = foldr (f $!) n . catMaybes

  infixl 7 &&&
  (&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
  (&&&) f g e = (f e, g e)

  maxWith :: (a -> Int) -> [a] -> (a,Int)
  maxWith f [a] = (a , f a)
  maxWith f (a:rest)
    | fa == 9223372036854775807 = (a, fa)
    | fa > fb = (a, fa)
    | otherwise = (b, fb)
    where 
      (b, fb) = maxWith f rest
      fa = f a

  parMaxWith :: (a -> Int) -> [a] -> (a,Int)
  parMaxWith f [a] = (a , f a)
  parMaxWith f (a:rest)
    | fa == 9223372036854775807 = (a, fa)
    | fa > fb = (a, fa)
    | otherwise = (b, fb)
    where 
      (b, fb) = parMaxWith f rest
      fa = fb `par` f a `pseq` f a

  minWith :: (a -> Int) -> [a] -> (a,Int)
  minWith f [a] = (a , f a)
  minWith f (a:rest)
    | fa == (-922337203685477580) = (a , fa)
    | fa < fb = (a, fa)
    | otherwise = (b, fb)
    where 
      (b, fb) = minWith f rest
      fa = f a

  parMinWith :: (a -> Int) -> [a] -> (a,Int)
  parMinWith f [a] = (a , f a)
  parMinWith f (a:rest)
    | fa == (-922337203685477580) = (a , fa)
    | fa < fb = (a, fa)
    | otherwise = (b, fb)
    where 
      (b, fb) = parMinWith f rest
      fa = fb `par` f a `pseq` f a

  prunMaxWith :: (n -> (Int,Int) -> (Int,Int)) -> (Int,Int) -> [n] -> (Int,Int)
  prunMaxWith f (a,b) [n]
    | a >= b = (a,b)
    | otherwise = f n (a,b)
  prunMaxWith f (a,b) (n:rest)
    | a >= b = (a,b)
    | otherwise = prunMaxWith f ( a `min` fa `min` fb , b ) rest
    where 
      (fa, fb) = f n (a,b)

  prunMinWith :: (n -> (Int,Int) -> (Int,Int)) -> (Int,Int) -> [n] -> (Int,Int)
  prunMinWith f (a,b) [n]
    | a >= b = (a,b)
    | otherwise = f n (a,b)
  prunMinWith f (a,b) (n:rest)
    | a >= b = (a,b)
    | otherwise = prunMinWith f ( a , b `max` fa `max` fb ) rest
    where
      (fa, fb) = f n (a,b)

  flatTupple :: [(a, [b])] -> [(a, b)]
  flatTupple ((a, (b:brest)):arest)= (a, b) : flatTupple ((a, brest):arest)
  flatTupple ((a, []):arest) = flatTupple arest
  flatTupple [] = []

  infixl 1 <.
  (<.) :: (b -> c) -> (a -> b) -> a -> c
  (<.) = (.)

  consIf :: Bool -> a -> [a] -> [a]
  consIf True = (:)
  consIf False = flip const
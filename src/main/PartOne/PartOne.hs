{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, EmptyDataDecls #-}
module PartOne.PartOne where
{-
 - Code based on the Leuven Haskell User Group talk "Haskell: A Universe of Types".
 -
 - Tom Schrijvers
 -}

-- * Isomorphism

data a <~> b
  =  Iso { to :: a -> b
         , from :: b -> a }

-- Round-Trip Laws:
--   to iso . from iso = id
--   from iso . to iso = id

-- * Generic Universe

data Zero

data One
  =  One
  deriving Show

-- Ex1: Show that One is isomorphic to the built-in `unit' type
--         data () = ()

isoOneUnit :: One <~> ()
isoOneUnit = Iso (\One -> ()) (\() -> One)

data (x + y)
  =  Inl x
  |  Inr y
  deriving Show

-- Ex2: Show that (+) is isomorphic to the predefined Either type
-- 		data Either x y  =  Left x | Right y
toEither :: (a + b) -> Either a b
toEither (Inl x) = Left x
toEither (Inr y) = Right y

toPlus :: Either a b -> (a + b)
toPlus (Left x) = Inl x
toPlus (Right y) = Inr y

isoPlusEither :: (a + b) <~> Either a b
isoPlusEither = Iso toEither toPlus

data (x * y)
  =  x :*: y
  deriving Show

-- Ex3: Show that (*) is isomorphic to the built-in tuple type
-- 		data (x,y) = (x,y)

toTuple :: (a * b) -> (a, b)
toTuple (a :*: b) = (a,b)

toStar :: (a,b) -> (a * b)
toStar (a, b) = (a :*: b)

isoStarTuple :: (a * b) <~> (a, b)
isoStarTuple = Iso toTuple toStar

-- * Arithmethic Laws

-- x + 0 = x
rightZero :: (x + Zero) <~> x
rightZero = Iso t f where
  t (Inl x) =  x
  f x = Inl x

-- (x + y) + z = x + (y + z)
assocPlus :: ((x + y) + z) <~> (x + (y + z))
assocPlus = Iso t f where
  t (Inl (Inl x))  =  Inl x
  t (Inl (Inr y))  =  Inr (Inl y)
  t (Inr z)        =  Inr (Inr z)

  f (Inl x) = Inl (Inl x)
  f (Inr (Inl y)) = Inl (Inr y)
  f (Inr (Inr z)) = Inr z

-- Ex5: write the isomorphisms for
--
--  (a)  x + y = y + x
--  (b)  x * 1 = x
--  (c)  x * y = y * x
--  (d)  x * (y * z) = (x * y) * z
--  (e)  x * (y + z) = x * y + x * z

-- (a)
commuPlus :: (x + y) <~> (y + x)
commuPlus  = Iso t f where
  t (Inl x) = Inr x
  t (Inr y) = Inl y

  f (Inl y) = Inr y
  f (Inr x) = Inl x

-- (b)
rightOne :: (x * One) <~> x
rightOne = Iso t f where
  t (x :*: One) = x
  f x = (x :*: One)

-- (c)
commuProd :: (x * y) <~> (y * x)
commuProd = Iso t f where
  t (x :*: y) = y :*: x
  f (y :*: x) = x :*: y

-- (d)
assocProd :: (x * (y * z)) <~> ((x * y) * z)
assocProd = Iso t f where
  t (x :*: ( y :*: z)) = ((x :*: y) :*: z)
  f ((x :*: y) :*: z)  = (x :*: (y :*: z))


plusMaalWhatever :: (x * (y + z)) <~> ((x * y) + (x * z))
plusMaalWhatever = Iso t f where
  t (x :*: (Inl y)) = Inl (x :*: y)
  t (x :*: (Inr z)) = Inr (x :*: z)
  f (Inl (x :*: y)) = x :*: (Inl y)
  f (Inr (x :*: z)) = x :*: (Inr z)
-- Ex6:
--   (a)  Come up with a way to compose isomorphisms (a <~> b) and (b <~> c)
--   (b)  Use this composition to create an isomorphism for
--        the arithmetic law: 0 + x = x
composeIso :: (a <~> b) -> (b <~> c) -> (a <~> c)
composeIso isoAB isoAC = Iso t f where
  t = to isoAC . to isoAB
  f = from isoAB . from isoAC

zeroLeftIso :: (Zero + x) <~> x
zeroLeftIso = composeIso commuPlus rightZero
-- Ex7: Come up with a definition for the identity isomorhism  (a <~> a)

identityIso :: a <~> a
identityIso = Iso id id

-- Ex8:
--   (a) Come up with a way to promote isomorphisms (a <~> b) to
--       isomorphisms (a + c <~> b + c) and also to isomorphisms (a * c <~> b * c).
--   (b) Use this to show that:
--       (x + y) + z <~> (y + x) <~> z
promoteIso :: (a <~> b) -> ((a + c) <~> (b + c))
promoteIso isoAB = Iso t f where
  t (Inl a) = Inl (to isoAB a)
  t (Inr c) = Inr c
  f (Inl b) = Inl (from isoAB b)
  f (Inr c) = Inr c


-- Ex10: write the isomorphisms for
--
--  (a)  x ^ (y + z) = (x ^ y) * (x ^ z)
--  (b)  x ^ (y * z) = x ^ y ^ z
--  (c)  (x * y) ^ z = (x ^ z) * (y ^ z)

type a ^ b = b -> a

isoA :: (x ^ (y + z)) <~> ((x ^ y) * (x ^ z))
isoA = Iso t f where
--   t (\yOrZ) = yOrzToX :*: yOrzToX
  t yOrzTox = (\y -> yOrzTox (Inl y)) :*: (\z -> yOrzTox (Inr z))
  f (yToX :*: zToX) = \inp -> case inp of
                                Inl y -> yToX y
                                Inr z -> zToX z

isoB :: (x ^ (y * z)) <~> (x ^ y ^ z)
isoB = Iso t f where
  t tupleYZToX = (\z y -> tupleYZToX (y :*: z))
  f zToyTox = (\(y :*: z) -> zToyTox z y)

isoC :: ((x * y) ^ z) <~> ((x ^ z) * (y ^ z))
isoC = Iso t f where
--   t zToXandY = ((\inp -> let (l :*: r) = zToXandY inp in l) :*: (\inp -> let (l :*: r) = zToXandY inp in l))
  t zToXandY = ((\z -> let (links :*: rechts) = zToXandY z in links )
                :*:
               (\z -> let (links :*: rechts) = zToXandY z in rechts))
  f (zToX :*: zToy) = (\z -> (zToX z) :*: (zToy z))
-- Ex11: write Ex10 (b) in a different way
--   using a combination of Ex3
--   and the Prelude functions curry and uncurry
--   as well as some glue code.

--stolen from data tuple
swap                    :: (a,b) -> (b,a)
swap (a,b)              = (b,a)

isoB' :: (x ^ (y * z)) <~> (x ^ y ^ z)
isoB'= Iso t f where
  t tupleYZToX = curry $ tupleYZToX . toStar . swap
  f zToyTox = (uncurry zToyTox) . swap . toTuple

-- Ex18: write the isomorphism   (Bool -> Int) <~> (Int * Int)

-- * Generic Representation

class Representable a where
  type Rep a

  repIso :: a <~> Rep a

toRep :: Representable a => a -> Rep a
toRep = to repIso

fromRep :: Representable a => Rep a -> a
fromRep = from repIso


instance Representable Bool where
  type Rep Bool  =  One + One

  repIso = Iso t f where
    t True   =  Inl One
    t False  =  Inr One

    f (Inl One)  =  True
    f (Inr One)  =  False


instance Representable (Maybe a) where
  type Rep (Maybe a)  =  One + a

  repIso = Iso t f where
    t Nothing   =  Inl One
    t (Just x)  =  Inr x

    f (Inl One)  =  Nothing
    f (Inr x)    =  Just x

instance Representable [a] where
  type Rep [a]  =  One + (a * [a])

  repIso = Iso t f where
    t []      =  Inl One
    t (x:xs)  =  Inr (x :*: xs)

    f (Inl One)         =  []
    f (Inr (x :*: xs))  =  x : xs

-- Ex12: create an instance for the predefined Prelude type
--         data Ordering = LT | EQ | GT
instance Representable Ordering where
  type Rep Ordering = (One + (One + One))

  repIso = Iso t f where
    t LT = Inl One
    t EQ = Inr . Inl $ One
    t GT = Inr . Inr $ One

    f (Inl One) = LT
    f (Inr (Inl One)) = EQ
    f (Inr (Inr One)) = GT


-- Ex13: create an instance for
data Tree a  =  Empty | Leaf a | Fork (Tree a) (Tree a)

instance Representable (Tree a) where
  type Rep (Tree a) = (One + (a + ((Tree a) * (Tree a))))

  repIso = Iso t f where
    t Empty = Inl One
    t (Leaf a) = Inr (Inl a)
    t (Fork left right) =Inr (Inr (left :*: right))

    f (Inl One) = Empty
    f (Inr (Inl a)) = Leaf a
    f (Inr (Inr (left :*: right))) = Fork left right


-- Ex14: create an instance for
data RoseTree a  =  Node a [RoseTree a]

instance Representable (RoseTree a) where
  type Rep (RoseTree a) = (a * (One + (RoseTree a * ([RoseTree a]))))

  repIso = Iso t f where
    t (Node a []) = (a :*: (Inl One))
    t (Node a (tree:trees)) = (a :*: (Inr (tree :*: trees)))
    f (a :*: (Inl One)) = Node a []
    f (a :*: (Inr (tree :*: trees))) = Node a (tree:trees)

-- * Generic Equality

class GEq a where
  geq :: a -> a -> Bool

-- * Equality for Universe

instance GEq One where
  geq One One  =  True

instance (GEq x, GEq y) => GEq (x + y) where
  geq (Inl x1) (Inl x2)  =  geq x1 x2
  geq (Inr y1) (Inr y2)  =  geq y1 y2
  geq _        _         =  False

instance (GEq x, GEq y) => GEq (x * y) where
  geq (x1 :*: y1) (x2 :*: y2)  =  geq x1 x2 && geq y1 y2

-- * Equality for Primitive Types

instance GEq Int where
  geq = (==)

instance GEq Char where
  geq = (==)

-- * Equality for Representable Types

deriveGEq :: (Representable a, GEq (Rep a)) => a -> a -> Bool
deriveGEq x y  =  geq (toRep x) (toRep y)

instance GEq Bool where
  geq  =  deriveGEq

instance GEq a => GEq (Maybe a) where
  geq  =  deriveGEq

instance GEq a => GEq [a] where
  geq  =  deriveGEq

-- Ex14: add instances for
--  (a)  Ordering
--  (b)  Tree a
--  (c)  RoseTree a

-- Ex15: Create a generic definition for ordering
class GEq a => GOrd a where
  gcompare :: a -> a -> Ordering

-- Ex16: Create a generic definition for serialisation.
--       We use a list of booleans to represent the serialised bitstream.
class Serialisable a where
  serialise    ::  a -> [Bool]
  deserialise  ::  [Bool] -> a

-- Ex17: Advanced
--   Figure out how to serialise to and from a "ByteString" instead of [Bool].
--   See the bytestring and bytestring-builder packages on Hackage.

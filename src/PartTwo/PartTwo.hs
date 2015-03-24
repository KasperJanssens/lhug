{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances #-}
module PartTwo.PartTwo where
{-
 - Code based on the Leuven Haskell User Group talk "The Expression Problem".
 -
 - Tom Schrijvers
 -}

-- * INFRASTRUCTURE

infixr 5  .+

-- ** Type-level fixpoint operator

newtype Fix f = In { out :: f (Fix f) }

-- ** Functor sum

data (f .+ g) a = InlF (f a) | InrF (g a)

-- ** Functor injection into a sum
--
--   forall i in 1..n:
--     fi <: (f1 .+ ... .+ fn)

class f <: g where
  inj :: f a -> g a

instance f <: f where
  inj = id

instance f <: (f .+ g) where
  inj = InlF

instance (f <: g) => f <: (h .+ g) where
  inj = InrF . inj

-- * SHAPES

type Vector = (Float, Float)
type Point  = (Float, Float)

-- ** Squares

data Square r  =  Square Float

square :: (Square <: f) => Float -> Fix f
square s  =  In (inj (Square s))

-- ** Circles

data Circle r  =  Circle Float

circle :: (Circle <: f) => Float -> Fix f
circle r  =  In (inj (Circle r))

-- ** Translated Shapes

data Translated r  =  Translated Vector r

translated :: (Translated <: f) => Vector -> Fix f -> Fix f
translated v s  =  In (inj (Translated v s))

-- ** Examples

type Shape1 = Fix Square
type Shape2 = Fix (Square .+ Circle)
type Shape3 = Fix (Square .+ Circle .+ Translated)

shapes1 :: [Shape1]
shapes1 = [square 1.0, square 2.0, square 3.0]

shapes2 :: [Shape2]
shapes2 = [square 1.0, circle 3.0]

shapes3 :: [Shape3]
shapes3 = [square 1.0, circle 3.0, translated (1.0, 1.0) (circle 2.0)]

-- * SHAPE OPERATIONS

-- ** Contains

contains :: Contains f => Fix f -> Point -> Bool
contains =  contains' contains . out

class Contains f where
  contains' :: (r -> Point -> Bool) -> (f r -> Point -> Bool)

instance Contains Square where
  contains' rec (Square s) (x,y)  =  0 <= x && x <= s && 0 <= y && y <= s

instance Contains Circle where
  contains' rec (Circle r) (x,y)  =  (x - r)^2 + (y - r)^2 <= r^2

instance Contains Translated where
  contains' rec (Translated (vx,vy) s) (x,y)  =  rec s (x - vx, y - vy)

instance (Contains f, Contains g) => Contains (f .+ g) where
  contains' rec (InlF fr)  =  contains' rec fr
  contains' rec (InrF gr)  =  contains' rec gr

-- * EXERCISES

-- Ex1: Create a case that represents the union of two shapes.

-- Ex2: Create an operation that shrinks a shape by a given factor.

-- Ex3: Create a modular version for
--
--        data Exp = Lit Int | Add Exp Exp
--
--        eval :: Exp -> Int
--        eval (Lit n)      =  n
--        eval (Add e1 e2)  =  eval e1 + eval e2
--
--        display :: Exp -> String
--        display (Lit n)      =  show n
--        display (Add e1 e2)  =  display e1 ++ " + " ++ display e2

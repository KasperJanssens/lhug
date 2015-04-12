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

shrink :: Shrinkable f => Fix f -> Float -> Fix f
shrink fixPoint factor =  let s = shrink' shrink (out fixPoint) factor in In s

class Contains f where
  contains' :: (r -> Point -> Bool) -> f r -> Point -> Bool

class Shrinkable f where
  shrink' :: (r -> Float -> r) -> f r -> Float -> f r

instance Contains Square where
  contains' rec (Square s) (x,y)  =  0 <= x && x <= s && 0 <= y && y <= s

instance Shrinkable Square where
  shrink' rec (Square r) factor = Square (r / factor)

instance Contains Circle where
  contains' rec (Circle r) (x,y)  =  (x - r)^2 + (y - r)^2 <= r^2

instance Shrinkable Circle where
  shrink' rec (Circle s) factor = Circle (s / factor)

instance Contains Translated where
  contains' rec (Translated (vx,vy) s) (x,y)  =  rec s (x - vx, y - vy)

instance Shrinkable Translated where
  shrink' rec (Translated (vs, vy) s) factor = Translated (vs, vy) (rec s factor)

instance (Contains f, Contains g) => Contains (f .+ g) where
  contains' rec (InlF fr)  =  contains' rec fr
  contains' rec (InrF gr)  =  contains' rec gr

instance (Shrinkable f, Shrinkable g) => Shrinkable (f .+ g) where
  shrink' rec (InlF fr) factor = InlF (shrink' rec fr factor)
  shrink' rec (InrF gr) factor = InrF (shrink' rec gr factor)

data Union r = Union r r

union :: (Union <: f) => Fix f -> Fix f -> Fix f
union s1 s2 = In (inj (Union s1 s2))

instance Contains Union where
  contains' rec (Union s1 s2) (x,y) = rec s1 (x,y) || rec s2 (x,y)

instance Shrinkable Union where
  shrink' rec (Union s1 s2) factor = Union (rec s1 factor) (rec s2 factor)

type Shape4 = Fix (Square .+ Circle .+ Translated .+ Union)

shapes4 ::[Shape4]
shapes4 = [translated (1,2) (circle 7), union (circle 7) (square 5)]

res :: [Bool]
res = fmap (flip contains $ (0,1)) shapes4

res2 :: [Shape4]
res2 = fmap (flip shrink $ 7) shapes4

-- * EXERCISES

-- Ex1: Create a case that represents the union of two shapes.

-- Ex2: Create an operation that shrinks a shape by a given factor.

data Literal r = Lit Int

data Add r = Add r r

type Exp = (Literal .+ Add)

eval ::(Evaluable f) => Fix f -> Int
eval = eval' eval . out

class Evaluable f where
   eval' :: (r -> Int) -> f r -> Int

instance Evaluable Literal where
  eval' rec (Lit x) = x

instance Evaluable Add where
  eval' rec (Add l f) = rec l + rec f

instance (Evaluable f, Evaluable g) => Evaluable (f .+ g) where
  eval' rec (InlF fLeft) = eval' rec fLeft
  eval' rec (InrF fRight) = eval' rec fRight

display :: (Displayable f) => Fix f -> String
display = display' display . out

class Displayable f where
  display' :: (r -> String) -> f r -> String

instance Displayable Literal where
  display' rec (Lit x) = show x

instance Displayable Add where
  display' rec (Add l r) = show (rec l) ++ " + " ++ show (rec r)

instance (Displayable f, Displayable g) => Displayable (f .+ g) where
  display' rec (InlF fLeft) = display' rec fLeft
  display' rec (InrF fRight) = display' rec fRight

data Mul r = Mul r r

type ExpExt = (Literal .+ Add .+ Mul)

instance Evaluable Mul where
  eval' rec (Mul l r) = rec l * rec r

instance Displayable Mul where
  display' rec (Mul left right) = show (rec left) ++ " * " ++ show (rec right)

lit :: (Literal <: f) => Int -> Fix f
lit x = In (inj (Lit x))

add :: (Add <: f) => Fix f -> Fix f -> Fix f
add left right = In (inj (Add left right))

mul :: (Mul <: f) => Fix f -> Fix f -> Fix f
mul left right = In (inj (Mul left right))

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

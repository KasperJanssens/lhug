{-# LANGUAGE TypeOperators #-}

module PartThree.PartThree where


{-
 - Code based on the Leuven Haskell User Group talk "Bringing Functions into the Fold".
 -
 - Tom Schrijvers
 -}

import Prelude hiding (sum, length)
import PartOne.PartOne

fac :: Int -> Int
fac n  =  if n == 0 then 1 else n * fac (n - 1)

-- * EXPRESSION-LEVEL FIXPOINT OPERATOR

fix :: (a -> a) -> a
fix f = f (fix f)

fac' :: (Int -> Int) -> (Int -> Int)
fac' f n  =  if n == 0 then 1 else n * f (n - 1)

fac2 :: Int -> Int
fac2 = fix fac'

-- * BASIC LIST TYPE

data List = Nil | Cons Int List deriving (Show, Eq)

sum :: List -> Int
sum Nil          =  0
sum (Cons x xs)  =  x + sum xs

prod :: List -> Int
prod Nil          =  1
prod (Cons x xs)  =  x * prod xs

length :: List -> Int
length Nil          =  0
length (Cons x xs)  =  1 + length xs

-- Ex0: Write the non-recursive sum'

sum2 :: List -> Int
sum2 = fix sum' where
  sum' _go Nil = 0
  sum' go (Cons x xs) = x + go xs


-- * FOLD RECURSION SCHEME

fold :: a -> (Int -> a -> a) -> List -> a
fold n c Nil          =  n
fold n c (Cons x xs)  =  c x (fold n c xs)

sumf :: List -> Int
sumf = fold 0 (+)

prodf :: List-> Int
prodf = fold 1 (*)

lengthf :: List -> Int
lengthf = fold 0 (\_ n -> n + 1)

-- Ex1: Write the following functions in terms of fold

appendf :: List -> List -> List
appendf leftList rightList = fold rightList Cons leftList

reversef :: List -> List
reversef = fold Nil (\x lijst -> appendf lijst (Cons x Nil))

idList :: List -> List
idList = fold Nil Cons


-- * PATTERN FUNCTOR OF LIST

data List' r = Nil' | Cons' Int r

instance Functor List' where
  fmap f Nil'         =  Nil'
  fmap f (Cons' x r)  =  Cons' x (f r)

-- * TYPE-LEVEL FIXPOINT OPERATOR

newtype Fix f  =  In (f (Fix f))

listIso :: List <~> Fix List'
listIso = Iso t f where
  t Nil          =  In Nil'
  t (Cons x xs)  =  In (Cons' x (t xs))

  f (In Nil')          =  Nil
  f (In (Cons' x xs))  =  Cons x (f xs)

-- Ex2: Establish the following isomorphisms
listIso2 :: Fix List' <~> List' (Fix List')
listIso2 = Iso t f where
  t (In Nil') =  Nil'
  t (In (Cons' x r)) = Cons' x (In (t r))
  f Nil' = In Nil'
  f (Cons' x (In list)) = In (Cons' x (f list))

listIso3 :: List' List <~> List
listIso3 = Iso t f where
  t Nil' = Nil
  t (Cons' x r) = Cons x r
  f Nil = Nil'
  f (Cons x r) = Cons' x r


-- * GENERIC FOLD

gfold :: Functor f => (f a -> a) -> Fix f -> a
gfold alg (In s)  =  alg (fmap (gfold alg) s)

sum3 :: Fix List' -> Int
sum3 = gfold alg where
  alg Nil'         =  0
  alg (Cons' x s)  =  x + s

-- Ex3: Write length in terms of gfold

length3 :: Fix List' -> Int
length3 = gfold alg where
  alg  Nil' =  0
  alg (Cons' _ s) = 1 + s

-- * EXPRESSION EXAMPLE

data Exp = Lit Int | Add Exp Exp

data Exp' r = Lit' Int | Add' r r

instance Functor Exp' where
  fmap f (Lit' n)    =  Lit' n
  fmap f (Add' x y)  =  Add' (f x) (f y)

eval :: Exp -> Int
eval (Lit n)      =  n
eval (Add e1 e2)  =  eval e1 + eval e2

eval2 :: Fix Exp' -> Int
eval2 = gfold alg where
  alg (Lit' n)    =  n
  alg (Add' x y)  =  x + y

-- Ex4: Write the following function in terms of gfold

showExp :: Exp -> String
showExp (Lit n)      =  show n
showExp (Add e1 e2)  =  parens (showExp e1) ++ "+" ++ parens (showExp e2)

parens :: String -> String
parens s  = "(" ++ s ++ ")"

showExp2 :: Fix Exp' -> String
showExp2  =  gfold alg where
  alg = undefined

-- Ex5: Write a specialised version of gfold for Exp,
--      just like fold is a specialised version of gfold for List,
--      and write eval in terms of it.

-- Ex6: Write an identity function that is generic in the functor f as a fold.

gId :: Functor f => Fix f -> Fix f
gId = gfold undefined

-- Ex7: Create a list type that is polymorphic in its type of elements
--      and write its pattern functor.

-- Ex8: (a) Write the pattern functor for the rose trees from Meeting1
--            data RoseTree a = Node a [RoseTree a]
--      (b) Use gfold to write a toList function that collects the elements
--          of the tree in a list.

-- Ex9: (Hard) Write left fold in terms of a right fold

foldL :: a -> (a -> Int -> a) -> List -> a
foldL acc update  Nil         =  acc
foldL acc update (Cons x xs)  =  foldL (update acc x) update xs

foldL2 :: a -> (a -> Int -> a) -> List -> a
foldL2 = undefined

-- Ex10: Discover the folds in code you've written previously.
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Applicative

data Prop
  = Var Int
  | And Prop Prop
  | Or Prop Prop
  | Not Prop
  | Val Bool
  deriving (Read, Show, Eq)

findVar :: Prop -> Maybe Int
findVar (Var a) = Just a
findVar (And p1 p2) = findVar p1 <|> findVar p2
findVar (Or p1 p2) = findVar p1 <|> findVar p2
findVar (Not p) = findVar p
findVar (Val _) = Nothing

replace :: Int -> Bool -> Prop -> Prop
replace var val expr =
  case expr of
    Var a -> if a == var then Val val else Var a
    And p1 p2 -> And (replace var val p1) (replace var val p2)
    Or p1 p2 -> Or (replace var val p1) (replace var val p2)
    Not p -> Not (replace var val p)
    Val b -> Val b

shorthand :: Prop -> Prop
shorthand (And p1 p2)
  | p1 == Val False = Val False
  | p1 == Val True = if p2 == Val True then Val True else p2
  | p2 == Val False = Val False
  | p2 == Val True = p1
  | otherwise = And p1 p2
shorthand (Or p1 p2)
  | p1 == Val True = Val True
  | p1 == Val False = if p2 == Val False then Val False else p2
  | p2 == Val False = Val False
  | p2 == Val True = p1
  | otherwise = And p1 p2
shorthand _ = error "Not exausted"

simplify :: Prop -> Prop
simplify (Var a) = Var a
simplify (And p1 p2) = shorthand $ And (simplify p1) (simplify p2)
simplify (Or p1 p2) = shorthand $ Or (simplify p1) (simplify p2)
simplify (Not p) = case p of
  Val a -> Val (not a)
  _ -> Not p
simplify (Val b) = Val b

getBool :: Prop -> Bool
getBool (Val b) = b
getBool _ = error "Not a value"

sat :: Prop -> Bool
sat expr = case findVar expr of
  Nothing -> getBool expr
  Just var -> sat (letTrue var) || sat (letFalse var)
    where
      letTrue v = simplify (replace v True expr)
      letFalse v = simplify (replace v False expr)

parseDimacs :: String -> Prop
parseDimacs content = parseProblem $ filter notHeader (lines content)
  where
    notHeader a = a /= [] && (head a /= 'p') && (head a /= 'c')

parseProblem :: [String] -> Prop
parseProblem problem = foldl1 And (map toProp problem)
  where
    parseNum :: Int -> Prop
    parseNum n = if n < 0 then Not (Var (- n)) else Var n

    toProp :: String -> Prop
    toProp line = foldl1 Or (map parseNum (filter (/= 0) nums)) where nums :: [Int] = map read (words line)

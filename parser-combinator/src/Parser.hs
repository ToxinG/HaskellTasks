{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

first :: (a -> b) -> (a, c) -> (b, c)
first f (p1, p2) = (f p1, p2)

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure a = Parser $ \s -> Just (a, s)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser pf <*> Parser pa = Parser pb where
        pb = pf >=> \(f', s) -> pa s >>= \(x, s') -> Just (f' x, s')

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser parser >>= f = Parser mb where
        mb = parser >=> \(ta, s) -> runParser (f ta) s

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ const Nothing

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser a <|> Parser b = Parser $ \s -> a s <|> b s

ok :: Parser s ()
ok = Parser $ \s -> Just((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just((), [])
    _ -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq a => a -> Parser a a
element e = satisfy (== e)

stream :: Eq a => [a] -> Parser a [a]
stream p = Parser $ \s -> matchEach p s where
    matchEach [] s = Just(p, s)
    matchEach (x:xs) [] = Nothing
    matchEach (x:xs) (y:ys) = if x == y then matchEach xs ys else Nothing


balancedBrackets :: Parser Char String
balancedBrackets = ((element '(' *> balance 1) <|> (const "ok") <$> eof)

balance :: Int -> Parser Char String
balance 0 = balancedBrackets
balance n = (element ')' *> balance (n - 1) <|> element '(' *> balance (n + 1))

unsignedInt :: Parser Char Int
unsignedInt = ((\x -> (read x :: Int)) <$> (some $ satisfy isDigit))

signedInt :: Parser Char Int
signedInt = ((\x y -> y * case x of {'-' -> -1; _ -> 1}) <$> (element '+' <|> element '-' <|> (fmap (const ' ') ok)) <*> unsignedInt)

listListParser :: Parser Char [[Int]]
listListParser = parseList []

parseList :: [[Int]] -> Parser Char [[Int]]
parseList ll = ((const ll) <$> eof) <|> ((unsignedInt >>= (\x -> parseInts x [])) >>= (\l -> parseList (ll ++ [l])))

parseInts 0 l = (const l) <$> (eof <|> (many $ element ' ') *> element ',' *> (many $ element ' ') *> ok)
parseInts n l = (many $ element ' ') *> element ',' *> (many $ element ' ') *> signedInt >>= (\x -> parseInts (n - 1) (l ++ [x]))

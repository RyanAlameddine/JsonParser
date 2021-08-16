module ParserLib where
    
--MAKE SURE TO ADD IMPORTS
import Control.Applicative
import Data.Char

--conceptually, the intro is the most difficult part
--ima skim over some of the topics covered today to get to the actualy parsing
--the theory behind it (category theory) is rly interesting but I wont focus too much on it

--when you get to the actual parsers, the difficulty is building up the intuition
-- for what functions do what and what operators mean what
--you might find yourself staring at a function trying to write the parser for like 5 minutes
--  only to realize you can do it as the composition of just one or two function calls

--parser :: String -> a
--parser :: String -> (String, a)
--parser :: String -> Maybe (String, a)
--parser :: String -> Either String (String, a)
--parser :: String -> Either (Int, Int, String) (String, a)
--type Parser a = String -> Maybe (String, a)
--data Parser a = Parser (String -> Maybe (String, a))
newtype Parser a = Parser (String -> Maybe (String, a))

charY :: Parser Char
charY = Parser parse
    where
        parse (x:xs)
            | x == 'Y'  = Just (xs, 'Y')
            -- otherwise = Nothing
        parse _ = Nothing

runParser :: Parser a -> String -> Maybe (String, a)
runParser (Parser f) = f

        --skip this?
        -- data Person = Person String Int Float Int
        -- data Person = Person { name :: String, age :: Int, height :: Float, idNum :: Int}

        -- getName :: Person -> String
        -- getName (Person n _ _ _) = n
        -- getName Person{name = n} = n

        -- newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser parse
    where
        parse (x:xs)
            | p x       = Just (xs, x)
            | otherwise = Nothing

char :: Char -> Parser Char
-- char c = Parser parse
--     where
--         parse (x:xs)
--             | x == c = Just (xs, c)
--         parse _ = Nothing
char c = satisfy (==c)

digit :: Parser Char
--digit = satisfy (`elem` ['0'..'9'])
digit = satisfy isDigit

string :: String -> Parser String
--string :: Char -> Parser String
-- ['h', 'e', 'l', 'l', 'o']
-- [char 'h', char 'e', char 'l', char 'l', char 'o']
-- pSequenceA :: [Parser a] -> Parser [a]
-- pChain (char 'h') (char 'e') -> Parser that parses ('h', 'e') (dont have them implement)

timesFiveMaybe :: Maybe Int -> Maybe Int
timesFiveMaybe Nothing = Nothing
timesFiveMaybe (Just x) = Just (x * 5)

timesFiveList :: [Int] -> [Int]
timesFiveList [] = []
timesFiveList (x:xs) = (x * 5) : timesFiveList xs

--timesFive = goBackInContext (valueIgnoringContext * 5)

--change those to fmap

-- maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
-- -- maybeApply Nothing _ = Nothing
-- -- maybeApply _ Nothing = Nothing
-- -- maybeApply (Just f) (Just a) = Just (f a)

maybeApply2 :: Maybe (a -> b) -> Maybe (b -> c) -> Maybe a -> Maybe c
maybeApply2 Nothing _ _ = Nothing
maybeApply2 _ Nothing _ = Nothing
maybeApply2 _ _ Nothing = Nothing
maybeApply2 (Just f) (Just g) (Just a) = Just (g (f a))
--maybeApply2 _ _ _ _ = Nothing

pleaseStop :: (a -> Maybe b) -> (b -> Maybe c) -> Maybe a -> Maybe c
pleaseStop _ _ Nothing  = Nothing
pleaseStop f g (Just a) = applyG (f a)
    where
        applyG Nothing = Nothing
        applyG (Just b) = g b

cry :: Maybe (a -> Maybe b) -> Maybe (b -> Maybe c) -> Maybe a -> Maybe c

--rewrite one in do notation
-- maybeApply maybeF maybeVal = do
--     f   <- maybeF
--     val <- maybeVal
--     Just (f val)


cry maybeF maybeG maybeA = do
    f <- maybeF
    a <- maybeA
    b <- f a
    g <- maybeG
    c <- g b
    Just c




instance Functor Parser where
    fmap f (Parser p) = Parser parser
        where
            -- parser xs = fmap mapMaybe (p xs)
            -- mapMaybe (str, a) = (str, f a) --fmap on tuple

            parser xs = do
                (str, val) <- p xs
                pure (str, f val)
            -- parser xs = do
            --     val <- p xs
            --     Just $ fmap f val

--runParser charY "yellow" ("Y")
--runParser (fmap succ charY) "Yellow"

instance Applicative Parser where
    pure v = Parser parser
        where parser xs = Just (xs, v)
    liftA2 f (Parser x) (Parser y) = Parser parser
        where
            parser input = do
                (input' , v1) <- x input
                (input'', v2) <- y input'
                pure (input'', f v1 v2)

    --Parser (a -> b) <*> Parser a -> Parser b


pSequenceA :: [Parser a] -> Parser [a]
-- pSequenceA []     = pure []
-- pSequenceA (x:xs) = liftA2 (:) x (pSequenceA xs)
pSequenceA = foldr (liftA2 (:)) (pure [])

--sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
--sequenceA :: (Applicative f) => [f a] -> f [a]

string = sequenceA . map char--traverse

--runParser (sequenceA [string "hello", string " ", string "the"]) "hello there"
--nameInGreeting :: String -> Parser String

--nameInGreeting str = sequenceA [string "hello, ", string str, string ", how are you?"]

--runParser (nameInGreeting "Stan") "hello, Stan, how are you?"
nameInGreeting str = string "hello, " *> string str <* string ", how are you?"


-- Just 5 <|> Nothing, Nothing <|> Just 5, Nothing <|> Nothing 
instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser a) <|> (Parser b) = Parser parser
        where
            parser input = a input <|> b input

greeting = string "hi" <|> string "hello" <|> string "greetings"

choice :: [Parser a] -> Parser a
choice = foldl1 (<|>)

oneOrMore :: Parser a -> Parser [a]
--oneOrMore parser = some parser
oneOrMore = some

zeroOrMore :: Parser a -> Parser [a]
--zeroOrMore parser = many parser
zeroOrMore = many

--int, a parser of Integer which reads some number of digits
--I would suggest checking the `read` function

--space, a parser which parses a whitespace character
--manySpace, a parser which parses zero or more whitespace characters
--someSpace, a parser which parses one or more whitespace characters

--separateBy, takes a Parser for a separator and a Parser for a value
--returns a parser which parses a list of all values (not including the separators)

int :: Parser Integer
int = fmap read (some digit)

space :: Parser Char
--space = char ' ' <|> char '\t' <|> '\n'
space = satisfy isSpace

zeroOrMoreSpace = zeroOrMore space

oneOrMoreSpace = oneOrMore space

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy sep val = liftA2 (:) val pairs <|> pure []
    where pairs = zeroOrMore (sep *> val)
--separateBy separator value = (:) <$> value <*> many (separator *> value) <|> pure []

--runParser (separateBy (char ',') (int)) "123,1234,12,5"
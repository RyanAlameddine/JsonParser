module JsonParser where

import ParserLib
import Control.Applicative

parseFile :: FilePath -> IO (Maybe (String, JsonValue))
parseFile path = do
    text <- readFile path 
    return $ runParser jsonObject text 

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer 
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving Show

--jsonValue = jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
jsonValue = zeroOrMoreSpace *> value <* zeroOrMoreSpace
    where value = choice [jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject]

-- jsonNull = fmap (\_ -> JsonNull) (string "null")
jsonNull = JsonNull <$ string "null"

-- jsonBool   "true" -> JsonBool True
-- jsonNumber "123"  -> JsonNumber 123
-- jsonString ""hi"" -> JsonString "hi" 

jsonBool = (JsonBool True <$ string "true") <|> (JsonBool False <$ string "false")

jsonNumber = JsonNumber <$> int

jsonString = JsonString <$> stringLiteral

--betweenManyWhitespace parser = between manyWhitespace parser manyWhitespace

jsonArray = JsonArray <$> (char '[' *> values <* char ']')
    where 
        values = separateBy comma jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (char '{' *> object <* char '}')
    where
        object = separateBy comma pair
        pair = combine (name <* char ':') jsonValue
        combine = liftA2 (,)
        name = zeroOrMoreSpace *> stringLiteral <* zeroOrMoreSpace 

stringLiteral = doubleQuote *> many stringChar <* doubleQuote
stringChar = satisfy (/='"')

doubleQuote = char '"'
comma       = char ','
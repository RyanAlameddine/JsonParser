module JsonParser where

import ParserLib

data JsonValue = JsonNull
                | JsonBool Bool
                | JsonNumber Integer 
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject (Map.Map String JsonValue) 

-- jsonNull = fmap (\_ -> JsonNull) (string "null")
jsonNull = JsonNull <$ string "null"

-- jsonBool   "true" -> JsonBool True
-- jsonNumber "123"  -> JsonNumber 123
-- jsonString ""hi"" -> JsonString "hi" 

jsonBool = (JsonBool True <$ string "true") <|> (JsonBool False <$ string "false")

jsonNumber = JsonNumber <$> int

jsonString = JsonString <$> stringLiteral
    where
        stringLiteral = doubleQuote *> many stringChar <* doubleQuote
        stringChar = satisfy (/='"')


--SHOW THEM THIS
--jsonValue = jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
jsonValue = zeroOrMoreSpace *> value <* zeroOrMoreSpace
    where value = choice [jsonBool, jsonNumber, jsonString, jsonArray]

--betweenManyWhitespace parser = between manyWhitespace parser manyWhitespace

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> separateBy comma jsonValue



doubleQuote = char '"'
comma       = char ','
module Parser
    ( parseWfg
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Data.Char (chr)
import Numeric (readHex)
import WfgLang

-- COMMENTS --

comment :: GenParser Char st Bool
comment = do
        try (do
                string "#"
                manyTill anyChar endOfLine
                return True
            )
    <|> try ( do
                string "#~"
                manyTill anyChar (string "~#")
                return True
            )
    <|> return False


spacesOrComments :: GenParser Char st ()
spacesOrComments = do
    spaces
    r <- option False comment
    if r then spacesOrComments else return ()


-- identifier --
lname :: GenParser Char st Identifier
lname = do
    first <- choice [letter, char '_']
    name <- many $ choice [letter, digit, char '_']
    return (first:name)

-- VALUES --

-- numeric literal --
lnum :: GenParser Char st Value
lnum = do
    isNegative <- optionalMinus
    wholePart <- wholePartRule
    hasFractional <- option False (char '.' >> return True)
    hasExp <- option False (choice [char 'e', char 'E'] >> return True)

    fractionalPart <- if hasFractional then fracPartRule else return 0.0
    exponentialPart <- if hasExp then expPartRule else return 0.0

    return $ constructNumber isNegative (hasFractional || hasExp) wholePart fractionalPart exponentialPart
    where
        constructNumber :: Bool -> Bool -> Integer -> Double -> Double -> Value
        constructNumber isNegative True wholePart fractionalPart exponentialPart = ValDouble n where
            n = if isNegative then (-n1) else n1
            n1 = n2 * (10.0 ** exponentialPart)
            n2 :: Double
            n2 = fractionalPart + fromInteger wholePart

        constructNumber isNegative False wholePart _ _ = ValInt n where
            n = if isNegative then (-wholePart) else wholePart

        optionalMinus = do
            r <- optionMaybe $ char '-'
            case r of Nothing -> return False
                      Just '-' -> return True

        wholePartRule = do
            first <- choice [char '0', oneOf ['1'..'9']]
            rest <- many digit
            return $ read (first:rest)

        fracPartRule = do
            digits <- many digit
            return $ read $ "0." ++ digits

        expPartRule = do
            signMb <- optionMaybe $ choice [char '-', char '+']
            digits <- many digit
            return $ ct signMb digits where
                ct :: Maybe Char -> String -> Double
                ct Nothing ds = read ds
                ct (Just '-') ds = -read ds
                ct (Just '+') ds = read ds

-- string literal --
lstr :: GenParser Char st Value
lstr = do
    c <- oneOf "'\""
    strC <- strContent c
    char c
    return $ ValString strC
    where
        unescapedChar :: Char -> GenParser Char st Char
        unescapedChar c = noneOf [c, '\\']

        escapedChar :: Char -> GenParser Char st Char
        escapedChar ch = do
            char '\\'
            c <- oneOf [ch, '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']
            case c of 'u' -> do
                                code <- count 4 $ oneOf $ ['0'..'9'] ++ ['A'..'F']
                                return $ (chr . fst . head . readHex) code
                      _ -> return $ cvt c
                           where
                               cvt :: Char -> Char
                               cvt 'b' = '\b'
                               cvt 'f' = '\f'
                               cvt 'n' = '\n'
                               cvt 'r' = '\r'
                               cvt 't' = '\t'
                               cvt c = c

        strContent :: Char -> GenParser Char st String
        strContent c = many $ choice [escapedChar c, unescapedChar c]

-- bool literal --
lbool :: GenParser Char st Value
lbool = try (do
        v <- lname
        case v of "true" -> return $ ValBool True
                  "false" -> return $ ValBool False
                  _ -> fail "This is not a boolean value!"
    )

wfgValue :: GenParser Char st Value
wfgValue = choice [lbool, lstr, lnum]

-- OPERATORS --

keywords :: [String] -> GenParser Char st String
keywords kwds = try (do
        found <- lname
        if elem found kwds then
            return found
        else
            fail "Not a keyword"
    )

binaryOperator :: GenParser Char st BinaryOperator
binaryOperator = do
    s <- choice
        [ string "+"
        , string "-"
        , string "*"
        , string "/"
        , string "%"
        , keywords ["and", "or", "xor"]
        ]
    return $ binaryOperatorFromString s

unaryOperator :: GenParser Char st UnaryOperator
unaryOperator = do
    s <- choice [ string "-"
                , keywords ["not"]
                ]
    return $ unaryOperatorFromString s

-- EXPRESSIONS --

identifierExpression :: GenParser Char st Expression
identifierExpression = do
    id <- lname
    return $ ExprIdentifier id

atomicExpression :: GenParser Char st Expression
atomicExpression = do
    -- TODO: also (wfgExpression)
    mbval <- optionMaybe wfgValue
    case mbval of Nothing -> identifierExpression
                  Just val -> return $ ExprValue val

binaryOperatorExpression :: GenParser Char st Expression
binaryOperatorExpression = do
    -- TODO: use common expression rule
    spacesOrComments
    left <- atomicExpression
    spacesOrComments
    op <- binaryOperator
    spacesOrComments
    right <- atomicExpression
    spacesOrComments
    return $ ExprBinaryOp op left right

unaryOperatorExpression :: GenParser Char st Expression
unaryOperatorExpression = do
    -- TODO: use common expression rule
    spacesOrComments
    op <- unaryOperator
    spacesOrComments
    operand <- atomicExpression
    spacesOrComments
    return $ ExprUnaryOp op operand

readOperatorExpression :: GenParser Char st Expression
readOperatorExpression = do
    spacesOrComments
    keywords ["read"]
    spacesOrComments
    return ExprRead

wfgExpression :: GenParser Char st Expression
wfgExpression = choice [ unaryOperatorExpression
                       , binaryOperatorExpression
                       , readOperatorExpression
                       , atomicExpression
                       ]

parseWfg :: String -> String -> Either ParseError Expression
parseWfg fileName = parse wfgExpression fileName

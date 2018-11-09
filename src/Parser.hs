module Parser
    ( parseWfg
    , parseInput
    , parseExprOrCommand
    ) where

import WfgLang

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

def = emptyDef{ commentStart = "#~"
              , commentEnd = "~#"
              , commentLine = "#"
              , identStart = letter <|> char '_' <|> char '$'
              , identLetter = alphaNum
              , opStart = oneOf "+-*/%=!naox\\."
              , opLetter = oneOf "=otndr"
              , reservedOpNames = ["+", "-", "*", "/", "%", "=", "==", "!="
                                  , "not", "and", "or", "xor", "\\", "."]
              , reservedNames = ["true", "false", "end",
                                 "if", "then", "else",
                                 "while", "do", "output", "read"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , naturalOrFloat = m_naturalOrFloat
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , semi = m_semi
           , stringLiteral = m_stringLiteral
           , whiteSpace = m_whiteSpace } = makeTokenParser def

numberVal :: Either Integer Double -> Value
numberVal (Left i) = ValInt i
numberVal (Right d) = ValDouble d

exprParser :: Parser Expression
exprParser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "-"   >> return (ExprUnaryOp UnMinus     ))]
        , [Prefix (m_reservedOp "not" >> return (ExprUnaryOp UnNot       ))]
        , [Infix (m_reservedOp "*"   >> return (ExprBinaryOp BinMult     )) AssocLeft]
        , [Infix (m_reservedOp "/"   >> return (ExprBinaryOp BinDiv      )) AssocLeft]
        , [Infix (m_reservedOp "%"   >> return (ExprBinaryOp BinRemainder)) AssocLeft]
        , [Infix (m_reservedOp "+"   >> return (ExprBinaryOp BinPlus     )) AssocLeft]
        , [Infix (m_reservedOp "-"   >> return (ExprBinaryOp BinMinus    )) AssocLeft]
        , [Infix (m_reservedOp "=="  >> return (ExprBinaryOp BinEq       )) AssocLeft]
        , [Infix (m_reservedOp "!="  >> return (ExprBinaryOp BinNEq      )) AssocLeft]
        , [Infix (m_reservedOp "and" >> return (ExprBinaryOp BinAnd      )) AssocLeft]
        , [Infix (m_reservedOp "or"  >> return (ExprBinaryOp BinOr       )) AssocLeft]
        , [Infix (m_reservedOp "xor" >> return (ExprBinaryOp BinXor      )) AssocLeft]
        ]
term = m_parens exprParser
       <|> fmap ExprIdentifier m_identifier
       <|> fmap (\x -> ExprValue (numberVal x)) m_naturalOrFloat
       <|> (m_reserved "true" >> return (ExprValue $ ValBool True))
       <|> (m_reserved "false" >> return (ExprValue $ ValBool False))
       <|> (m_reserved "read" >> return ExprRead)

atLeastOneSemi = skipMany1 (m_semi >> m_whiteSpace)

wfgParser :: Parser Command
wfgParser = do
    m_whiteSpace
    result <- cmdParser
    optional m_semi
    eof
    return result
    where
        cmdParser :: Parser Command
        cmdParser = do
            cmds <- many singleCmd
            return $ CmdSequence cmds
        singleCmd = do { v <- m_identifier
                    ; m_reservedOp "="
                    ; e <- exprParser
                    ; atLeastOneSemi
                    ; return (CmdAssign v e)
                    }
                <|> do { m_semi
                    ; return CmdNoop
                    }
                <|> do { m_reserved "output"
                    ; e <- exprParser
                    ; atLeastOneSemi
                    ; return (CmdOutput e)
                    }
                <|> do { m_reserved "if"
                    ; condition <- exprParser
                    ; m_reserved "then"
                    ; thenBranch <- cmdParser
                    ; m_reserved "else"
                    ; elseBranch <- cmdParser
                    ; m_reserved "end"
                    ; return (CmdCondition condition thenBranch elseBranch)
                    }
                <|> do { m_reserved "while"
                    ; condition <- exprParser
                    ; m_reserved "do"
                    ; body <- cmdParser
                    ; m_reserved "end"
                    ; return (CmdWhileLoop condition body)
                    }

parseWfg :: String -> String -> Either ParseError Command
parseWfg filename = parse wfgParser filename


literal = fmap numberVal m_naturalOrFloat
          <|> (m_reservedOp "-" >> (fmap numberValNeg m_naturalOrFloat))
          <|> (m_reserved "true" >> return (ValBool True))
          <|> (m_reserved "false" >> return (ValBool False))
                where
                    numberValNeg :: Either Integer Double -> Value
                    numberValNeg (Left i) = numberVal (Left (-i))
                    numberValNeg (Right d) = numberVal (Right (-d))

parseInput :: String -> Either ParseError Value
parseInput = parse literal "(input)"


exprOrCommand :: Parser (Either Command Expression)
exprOrCommand = fmap Left wfgParser <|> fmap Right (do
    m_whiteSpace
    e <- exprParser
    eof
    return e)

parseExprOrCommand :: String -> String -> Either ParseError (Either Command Expression)
parseExprOrCommand filename = parse exprOrCommand filename

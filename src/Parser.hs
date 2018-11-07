module Parser
    ( parseWfg
    ) where

import WfgLang

import Control.Applicative((<*))
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
           , semiSep = m_semiSep
           , semi = m_semi
           , whiteSpace = m_whiteSpace } = makeTokenParser def

numberExpr :: Either Integer Double -> Expression
numberExpr (Left i) = ExprValue $ ValInt i
numberExpr (Right d) = ExprValue $ ValDouble d

exprParser :: Parser Expression
exprParser = buildExpressionParser table term <?> "expression"
table = [ [Infix (m_reservedOp "+"   >> return (ExprBinaryOp BinPlus     )) AssocLeft]
        , [Infix (m_reservedOp "-"   >> return (ExprBinaryOp BinMinus    )) AssocLeft]
        , [Infix (m_reservedOp "*"   >> return (ExprBinaryOp BinMult     )) AssocLeft]
        , [Infix (m_reservedOp "/"   >> return (ExprBinaryOp BinDiv      )) AssocLeft]
        , [Infix (m_reservedOp "%"   >> return (ExprBinaryOp BinRemainder)) AssocLeft]
        , [Infix (m_reservedOp "=="  >> return (ExprBinaryOp BinEq       )) AssocLeft]
        , [Infix (m_reservedOp "!="  >> return (ExprBinaryOp BinNEq      )) AssocLeft]
        , [Infix (m_reservedOp "and" >> return (ExprBinaryOp BinAnd      )) AssocLeft]
        , [Infix (m_reservedOp "or"  >> return (ExprBinaryOp BinOr       )) AssocLeft]
        , [Infix (m_reservedOp "xor" >> return (ExprBinaryOp BinXor      )) AssocLeft]
        , [Prefix (m_reservedOp "not" >> return (ExprUnaryOp UnNot  ))]
        , [Prefix (m_reservedOp "-"   >> return (ExprUnaryOp UnMinus))]
        ]
term = m_parens exprParser
       <|> fmap ExprIdentifier m_identifier
       <|> fmap numberExpr m_naturalOrFloat
       <|> (m_reserved "true" >> return (ExprValue $ ValBool True))
       <|> (m_reserved "false" >> return (ExprValue $ ValBool False))
       <|> (m_reserved "read" >> return ExprRead)

wfgParser :: Parser Command
wfgParser = m_whiteSpace >> cmdParser where
    cmdParser :: Parser Command
    cmdParser = do
        cmds <- m_semiSep singleCmd
        -- optional m_semi
        return $ CmdSequence cmds
    singleCmd = (eof >> return CmdNoop)
            <|> do { v <- m_identifier
                   ; m_reservedOp "="
                   ; e <- exprParser
                   ; return (CmdAssign v e)
                   }
            <|> do { m_reserved "output"
                   ; e <- exprParser
                   ; return (CmdOutput e)
                   }
            <|> do { m_reserved "if"
                   ; condition <- exprParser
                   ; m_reserved "then"
                   ; thenBranch <- cmdParser
                   ; optional m_semi
                   ; m_reserved "else"
                   ; elseBranch <- cmdParser
                   ; optional m_semi
                   ; m_reserved "end"
                   ; return (CmdCondition condition thenBranch elseBranch)
                   }
            <|> do { m_reserved "while"
                   ; condition <- exprParser
                   ; m_reserved "do"
                   ; body <- cmdParser
                   ; optional m_semi
                   ; m_reserved "end"
                   ; return (CmdWhileLoop condition body)
                   }

parseWfg :: String -> String -> Either ParseError Command
parseWfg filename = parse wfgParser filename

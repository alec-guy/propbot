{-# LANGUAGE OverloadedStrings #-} 

module Parser where 

import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators as CM
import Control.Monad.Combinators.Expr
-- import Control.Lens 
import Data.Text 
import Data.Void 
import Control.Monad (void) 
import Text.Megaparsec.Error 

import System.IO 
import Data.Text.IO as TIO

import LogicTypes 
import System.Exit 

type Parser = Parsec Void Text 

spaceC :: Parser () 
spaceC = L.space hspace1 CM.empty CM.empty 

lexer :: Parser a -> Parser a 
lexer = L.lexeme spaceC 

parens :: Parser a -> Parser a
parens = CM.between (string "(") (string ")") 

variable :: Parser Proposition
variable =  lexer $ Var <$> upperChar 

expr :: Parser Proposition
expr = lexer $ makeExprParser term table 

term :: Parser Proposition
term = lexer (parens expr <|> variable)

table :: [[Operator Parser Proposition]]
table = [[Prefix (Not <$ (CM.choice  $ (lexer . string) <$> ["~", "¬", "!", "′"]))]
        ,[InfixR (And <$ (CM.choice $ (lexer . string) <$> ["&", "∧", "·"]))
         ,InfixR (Or <$ (CM.choice $ (lexer . string) <$> ["||", "∨" ,"+", "∥"]))
         ,InfixR (If <$ (CM.choice $ (lexer . string) <$> ["->", "→", "⇒", "⊃"]))
         ,InfixR (Xor <$ (CM.choice $ (lexer . string) <$> ["xor", "XOR" , "⊻", "⊕", "↮", "≢"]))
         ,InfixR (Nand <$ (CM.choice $ (lexer . string) <$> ["nand", "NAND", "⊼"]))
         ,InfixR (Nor <$ (CM.choice $ (lexer . string) <$> ["nor", "NOR", "⊽"]))
         ,InfixR (Iff <$ (CM.choice $ (lexer . string) <$> ["<->", "⇔", "↔" , "≡"]))
         ]
        ]

parseTherefore :: Parser Text
parseTherefore = lexer $ CM.choice [string "%", string "∴", "therefore", "Therefore" , "THEREFORE"] 

parseConclusion = do
    void parseTherefore
    lexer expr 
parseArgument :: Parser Argument
parseArgument = do 
   (prems,conc) <- manyTill_ (expr <* (void $ lexer $ string ",") ) parseConclusion
   (eof <|> (void newline))
   return $ Argument {premises = prems, conclusion = conc}

getArgument :: IO Argument
getArgument = do
   arg <- TIO.readFile "myarg.txt"
   e   <- return (parse parseArgument "" arg)
   case e of 
    Left e    -> do 
                  Prelude.putStrLn $ errorBundlePretty e 
                  exitSuccess
    Right arg -> return arg 
  
   

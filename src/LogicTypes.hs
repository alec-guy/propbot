{-# LANGUAGE OverloadedStrings #-} 

module Parser where 

import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators as CM
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Debug as D
-- import Control.Lens 
import Data.Text as T
import Data.Void 
import Control.Monad (void) 
import Text.Megaparsec.Error 

import System.IO 
import Data.Text.IO as TIO

import LogicTypes 
import System.Exit 

type Parser = Parsec Void Text 

parseEquiv :: Parser [Proposition]
parseEquiv = do 
   exp1 <- expr 
   void $ lexer $ string ","
   exp2 <- expr 
   return $ [exp1, exp2]
   
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
table = [[Prefix (Not <$ (CM.choice  $ (lexer . string) <$> ["~", "¬", "!", "′" , "not"]))]
        ,[InfixR (And <$ (CM.choice $ (lexer . string) <$> ["&", "∧", "·", "and"]))
         ,InfixR (Or <$ (CM.choice $ (lexer . string) <$> ["||", "∨" ,"+", "∥", "or"]))
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

parsePropositions :: Parser [Proposition]
parsePropositions = do
   void $ spaceC
   props <- (sepEndBy1 (lexer expr) (lexer $ string ","))
   eof
   return $ props 

parseArgument :: Parser Argument
parseArgument = do 
   (prems,conc) <- manyTill_ ((lexer expr) <* (void $ lexer $ string ",") ) parseConclusion
   (eof <|> (void newline))
   return $ Argument {premises = prems, conclusion = conc}

parseErrorMsg :: Parser String 
parseErrorMsg = do 
   input <- do
      void $ skipManyTill (anySingle) newline 
      void $ skipManyTill (anySingle) newline 
      void $ lexer $ many alphaNumChar 
      void $ lexer $ (string "|")
      manyTill anySingle newline 
   unexpectedThing <- manyTill anySingle (string "expecting")
   case input of 
      "<empty line>" -> return $ "Argument must have at least one premise and a conclusion\n"  <>
                                  "Unexpected: " <> unexpectedThing <>
                                  "Your input: " <> input 
      _              ->
         let wordsS = Prelude.words input 
         in case (Prelude.all (== True)) $ (\t -> not $ t `Prelude.elem` wordsS) <$> ["therefore" , "Therefore","%","∴"] of 
             True  -> return $ "Argument is missing a therefore symbol and\n" <> "unexpected " <> unexpectedThing <> "\nYour input: " <> input
             False -> return $ "Unexpected " <> unexpectedThing <> "\nYour input: " <> input
   

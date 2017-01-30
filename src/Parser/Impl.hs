{-# LANGUAGE DeriveGeneric #-}

module Parser.Impl where

import Ast
import Text.ParserCombinators.ReadP
import Text.PrettyPrint.GenericPretty

data ParseErrorImpl a
  = NoParse
  | AmbiguousGrammar [a]
  | NotImplemented
  deriving (Eq, Show, Generic)

instance (Out a) => Out (ParseErrorImpl a)

type ParseError = ParseErrorImpl Prog

token :: ReadP a -> ReadP a
token p = skipSpaces *> p

binOp :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
binOp op f = string op *> pure f

parseLitCon :: ReadP Lit
parseLitCon = fmap LitCon $ munch1 (`elem` ['0'..'9'])

parseLitVar :: ReadP Lit
parseLitVar = fmap LitVar $ munch1 (`elem` ['a'..'z'])

parseLit :: ReadP Lit
parseLit = token $ choice
  [ parseLitCon
  , parseLitVar
  ]

parseExp0 :: ReadP LitExp
parseExp0 = fmap ExpLit parseLit

parseExp1Op :: ReadP (LitExp -> LitExp -> LitExp)
parseExp1Op = token $ binOp "*" ExpMul

parseExp1 :: ReadP LitExp
parseExp1 = chainl1 parseExp0 parseExp1Op

parseExp2Op :: ReadP (LitExp -> LitExp -> LitExp)
parseExp2Op = token $ choice
  [ binOp "+" ExpAdd
  , binOp "-" ExpSub
  ]

parseExp2 :: ReadP (LitExp)
parseExp2 = chainl1 parseExp1 parseExp2Op

parseProg :: ReadP Prog
parseProg = fmap Prog parseExp2

parse :: ReadP a -> String -> [(a, String)]
parse = readP_to_S

fullParse :: ReadP a -> String -> [a]
fullParse p s = fmap fst $ parse (p <* (skipSpaces >> eof)) s

parseString' :: ReadP a -> String -> Either (ParseErrorImpl a) a
parseString' p s =
  case fullParse p s of
    [] -> Left NoParse
    [a] -> Right a
    as -> Left $ AmbiguousGrammar as

parseString :: String -> Either ParseError Prog
parseString = parseString' parseProg

parseFile' :: ReadP a -> FilePath -> IO (Either (ParseErrorImpl a) a)
parseFile' p path = fmap (parseString' p) $ readFile path

parseFile :: FilePath -> IO (Either ParseError Prog)
parseFile = parseFile' parseProg

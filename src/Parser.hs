{-
 - Copyright (C) 2018 Matvey Soloviev
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Parser (
    do_parse
)
where

import qualified Data.List as List
import qualified Data.Char as Char

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

import Types

{- ### PARSER ### -}
langDef = javaStyle { P.reservedNames = ["@include",
                                         "@define", "@undef", 
                                         "@var", "@global", "@set", "@push_back",
                                         "@for", "@match", "@calc",
                                         "@eval", "@quote", "@unquote",
                                         "@dumpstack"],
                      P.reservedOpNames = ["=>","@!","@*","@+","@^","@#"],
                      P.opLetter       = oneOf ":!#%&*+./<=>?\\^|-~",
                      P.nestedComments = False }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (langDef)

symbol = P.symbol lexer
reserved = P.reserved lexer
ident = P.identifier lexer
parens = P.parens lexer
braces = P.braces lexer
brackets = P.brackets lexer
whiteSpace = P.whiteSpace lexer
operator = P.operator lexer
reservedOp = P.reservedOp lexer
lexeme = P.lexeme lexer

do_parse :: String -> String -> Either ParseError [Code]
do_parse f s = parse ( do whiteSpace
                          c <- parser
                          eof
                          return c
                     ) f s 

-- a block framed in (), [] or {}
parse_block =     (strict_parens parser >>= return.Parentheses)
              <|> (strict_brackets parser >>= return.SquareBrackets)
              <|> (strict_braces parser >>= return.CurlyBraces)

-- code = sequence of basic units
parser :: Parser [Code]
parser = many parse_one 

-- one basic unit
parse_one :: Parser Code
parse_one =     parse_comment
            <|> parse_cpp
            <|> parse_include
            <|> parse_define
            <|> parse_undef
            <|> parse_match
            <|> parse_direct
            <|> parse_eval
            <|> parse_concat
            <|> parse_calc
            <|> parse_quote
            <|> parse_unquote
            <|> parse_localvar
            <|> parse_globalvar
            <|> parse_setvar
            <|> parse_pushback
            <|> parse_debug
            <|> parse_parser
            <|> parse_native
            <|> parse_block

parse_debug = do reserved "@dumpstack"
                 return $ DumpStack

-- C preprocessor directive; needs separate treatment because it has to go on its own line
parse_cpp = do symbol "#"
               s <- many (satisfy (/= '\n'))
               return $ CPPDirective s

-- our own @include
parse_include = do reserved "@include"
                   s <- P.stringLiteral lexer 
                   return $ Include s

-- one substitution rule for @define macro
parse_defrule = many $ do lhs <- parens parser
                          reservedOp "=>"
                          rhs <- parens parser
                          return (lhs,rhs)

-- @define a macro
parse_define = do reserved "@define"
                  name <- ident
                  rules <- semistrict_braces parse_defrule
                  return (Define name rules)

-- @undefine a macro
parse_undef = do reserved "@undef"
                 name <- ident
                 return (Undef name)

-- @match a piece of code with substitution rules
parse_match = do reserved "@match"
                 code <- parens parser
                 rules <- semistrict_braces parse_defrule
                 return (Match code rules)

-- @var, @global, @set
parse_vartree =     (parens parser >>= return.Leaf)
                <|> (between (symbol "@[") (symbol "]")
                             (parse_vartree `sepBy` (symbol ",")) >>= return.Star)

parse_localvar = do reserved "@var"
                    symbol "$"
                    name <- ident
                    value <- parse_vartree
                    return (DefLocal name value)

parse_globalvar = do reserved "@global"
                     symbol "$"
                     name <- ident
                     value <- parse_vartree
                     return (DefGlobal name value)

parse_setvar = do reserved "@set"
                  symbol "$"
                  name <- ident
                  value <- parse_vartree
                  return (SetVar name value)

parse_pushback = do reserved "@push_back"
                    name <- parse_varname
                    value <- parse_vartree
                    return (PushBack name value)

-- @! a piece of code that is quoted verbatim without macro expansion
parse_direct = do reservedOp "@!"
                  c <- ( parens parser <|> brackets parser <|> braces parser <|> (parse_one >>= return.(:[])) )
                  return (Direct c)
-- @eval a piece of code that is evaluated twice (so e.g. the contents of variables are macro-expanded)
parse_eval = do reservedOp "@eval"
                c <- ( parens parser <|> brackets parser <|> braces parser <|> (parse_one >>= return.(:[])) )
                return (Eval c)
-- concatenate two tokens, cf. ##
parse_concat = do reservedOp "@@"
                  return Concat
-- arithmetic
parse_calc = do reserved "@calc"
                c <- parens parser
                return (Calc c)

-- strings <-> code
parse_quote = do reserved "@quote"
                 c <- parens parser
                 return (Quote c)

parse_unquote = do reserved "@unquote"
                   s <- P.stringLiteral lexer -- because we want escape sequences etc. to work
                   return (Unquote s)

-- parse capture or loop construction: $varname, @for(bind)[optional separator](body), @*[optional separator](body), @+[optional separator](body),
-- @^[optional][terminator][tokens]$varname, @#$varname
-- todo: support absence of separator
parse_parser =     parse_var
               <|> parse_for
               <|> parse_star
               <|> parse_plus
               <|> parse_capture_until
               <|> parse_capture_name
 
parse_varname = do symbol "$"
                   ident
parse_var = do symbol "$"
               i <- strict_ident 
               return $ Var i

parse_for = do reserved "@for"
               binders <- parens $ do lhs <- parse_varname `sepBy` (symbol ",")
                                      symbol ":"
                                      rhs <- parse_varname `sepBy` (symbol ",")
                                      return (lhs,rhs)
               separator <- (brackets parser <|> return [])
               body <- parens parser
               return $ LoopOver binders body separator
parse_star = do reservedOp "@*"
                separator <- (brackets parser <|> return [])
                body <- parens parser
                return $ ZeroOrMore body separator
parse_plus = do reservedOp "@+"
                separator <- (brackets parser <|> return [])
                body <- parens parser
                return $ OneOrMore body separator
parse_capture_until = do reservedOp "@^"
                         stops <- many $ brackets parser
                         var <- parse_varname
                         return $ CaptureUntil var stops
parse_capture_name = do reservedOp "@#"
                        var <- parse_varname
                        return $ CaptureName var

-- a C++ "word" of arbitrary composition
strict_word =     many1 (P.identLetter langDef)
              <|> many1 (P.opLetter langDef)
              <|> do c <- oneOf ";,"
                     return [c]

-- any native C++ token
parse_native =     (strict_ident >>= return.Name)
               <|> (strict_operator >>= return.Operator)
               <|> (framedBy '\"' >>= return.StringLiteral)
               <|> (framedBy '\'' >>= return.CharLiteral)
               <|> (strict_word >>= return.Other)
               <|> parse_whitespace

parse_whitespace =     (nnlspaces >>= return.WhiteSpace)
                   <|> parse_newline

strict_ident = do c <- P.identStart langDef
                  cs <- many (P.identLetter langDef)
                  return (c:cs)
               <?> "identifier"

strict_operator = do c <- P.opStart langDef
                     cs <- many (P.opLetter langDef)
                     return (c:cs)
                  <?> "operator"

strict_parens p   = between (string "(") (string ")") p
strict_braces p   = between (string "{") (string "}") p
strict_brackets p = between (string "[") (string "]") p

semistrict_braces p   = between (symbol "{") (string "}") p

isNewline c = c=='\n' || c=='\r'

nnlspaces = many1 (satisfy (\c -> Char.isSpace c && not (isNewline c)))

parse_newline = do s <- many1 (satisfy isNewline)
                   p <- getPosition
                   return $ NewLine s (sourceName p) (sourceLine p)

-- start with f, don't end until we encounter f again, unless it's preceded by (an odd number of) \
framedBy f = fmap concat $ between (char f) (char f <?> "end of literal")
                                   (many (    ((satisfy (\c -> (c /= f) && (c /= '\\'))) >>= return.(:[]))
                                          <|> do c <- char '\\'
                                                 d <- anyChar
                                                 return [c,d] ) )

-- need to redefine this because that part of parsec is opaque...
parse_comment =     do s <- try (string (P.commentLine langDef))
                       ss <- many (satisfy (/= '\n'))
                       return $ WhiteSpace (s++ss)
                <|> do s <- try (string (P.commentStart langDef))
                       ss <- inComment
                       return $ WhiteSpace (s++ss)
inComment =     do{ s <- try (string (P.commentEnd langDef)); return s }
            <|> do{ s <- many1 (noneOf startEnd); ss <- inComment; return (s++ss) }
            <|> do{ s <- oneOf startEnd; ss <- inComment; return (s:ss) }
            <?> "end of comment"
            where startEnd = List.nub (P.commentEnd langDef ++ P.commentStart langDef)
 

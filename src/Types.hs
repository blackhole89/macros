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

module Types ( 
    SubstRule, Code(..), VarTree(..),
    to_string, to_string_all
)
where

type SubstRule = ([Code],[Code])

data Code =   CPPDirective String
            | Parentheses [Code]
            | SquareBrackets [Code]
            | CurlyBraces [Code]
            | Name String       -- candidate for substitution
            | Operator String
            | StringLiteral String
            | CharLiteral String
            | Other String
            | WhiteSpace String
            | NewLine String String Int -- content file line
            -- our features
            | Include String
            | Define String [SubstRule]   -- name [(LHS,RHS)]
            | Undef String
            | DefLocal String VarTree
            | DefGlobal String VarTree
            | SetVar String VarTree
            | PushBack String VarTree
            | Match [Code] [SubstRule]    -- match processed code
            | Direct [Code]               -- force no substitution
            | Var String
            | CaptureName String
            | CaptureUntil String [[Code]] 
            | ZeroOrMore [Code] [Code]    -- pattern separator
            | OneOrMore [Code] [Code]     -- pattern separator
            | LoopOver ([String],[String]) [Code] [Code] -- binder body separator
            | Quote [Code]   -- convert code to string literal
            | Unquote String -- insert string literally
            | Concat
            | Calc [Code]
            -- debug
            | DumpStack
    deriving (Show, Eq, Ord)

data VarTree =   Leaf [Code]
               | Star [VarTree]
     deriving (Show,Eq,Ord)

{- ## stringify programs ## -}

to_string_all (WhiteSpace _:Concat:WhiteSpace _:cs) = to_string_all cs
to_string_all (WhiteSpace _:Concat:cs) = to_string_all cs
to_string_all (Concat:WhiteSpace _:cs) = to_string_all cs
to_string_all (c:Concat:cs) = (to_string c)++(to_string_all cs)
to_string_all (c:cs) = (to_string c)++(to_string_all cs)
to_string_all [] = ""
to_string (CPPDirective s) = "#"++s++""
to_string (Parentheses c) = "("++(to_string_all c)++")"
to_string (SquareBrackets c) = "["++(to_string_all c)++"]"
to_string (CurlyBraces c) = "{"++(to_string_all c)++"}"
to_string (Name s) = s
to_string (Operator s) = s
to_string (StringLiteral s) = "\""++s++"\""
to_string (CharLiteral c) = "\'"++c++"\'"
to_string (Other s) = s
to_string (WhiteSpace s) = s
to_string (NewLine s f l) = s++"#line "++(show l)++" "++(show f)++"\n"
to_string (Direct c) = "/* @! */"++(to_string_all c)++"/* !@ */"
to_string (Concat) = ""
to_string (Unquote s) = s
to_string (Quote c) = "\""++(to_string_all c)++"\""
-- none of these should actually ever be emitted
to_string (Include s) = "/* @include "++s++" */"
to_string (Define s ls) = "/* @define "++s++" */"
to_string (Undef s) = "/* @undef "++s++" */" 
to_string (Match c ls) = "/* @match */"
to_string (Var s) = "/* $"++s++" */"
to_string (CaptureName s) = "/* @#$"++s++" */"
to_string (CaptureUntil s b) = "/* @^...$"++s++" */"
to_string (ZeroOrMore c s) = "/* @* */"
to_string (OneOrMore c s) = "/* @+ */"
to_string (LoopOver b c s) = "/* @for */"
to_string (DefLocal s v) = "/* @var $"++s++" */"
to_string (DefGlobal s v) = "/* @global $"++s++" */"
to_string (SetVar s v) = "/* @set $"++s++" */"
to_string (PushBack s v) = "/* @push_back $"++s++" */"



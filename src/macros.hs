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

import qualified Data.Set as Set
import Data.Set (Set,(\\))
import qualified Data.Map as Map
import Data.Map (Map,(!))
import qualified Data.List as List
import Data.Maybe
import System.Environment
import System.IO (hPutStrLn,stderr)

import Types
import Parser
import MacroProcessor
                  
{- ### @include processing in IO monad ### -}
process_include ((Include s):cs) = do c <- process_file s
                                      cs' <- process_include cs
                                      return $ c++cs'
{- -- for now, include can only exist at top level
process_include ((Parentheses c):cs) = do c' <- Parentheses <$> process_include c
                                  cs' <- process_include cs
                                  return $ c':cs'
process_include ((CurlyBraces c):cs) = do c' <- CurlyBraces <$> process_include c
                                  cs' <- process_include cs
                                  return $ c':cs'
process_include ((SquareBrackets c):cs) = do c' <- SquareBrackets <$> process_include c
                                     cs' <- process_include cs
                                     return $ c':cs'
-}
process_include (c:cs) = do cs' <- process_include cs
                            return $ c:cs'
process_include [] = return []

process_file fn = do input <- readFile fn
                     case do_parse fn input of
                         Left e -> do hPutStrLn stderr $ "Parse error: "++show e
                                      return []
                         Right c -> do c' <- process_include c
                                       return c'

{- ### CLI ### -}

main = do args <- getArgs
          c <- process_file (head args)
          f <- return $ case (tail args) of
            ["-n"] -> strip_newlines 
            _ -> \x -> x
          case do_process c of
            Left e -> hPutStrLn stderr $ show e
            Right c' -> do putStrLn $  to_string_all $ f c'
       
 

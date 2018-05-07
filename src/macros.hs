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
       
 

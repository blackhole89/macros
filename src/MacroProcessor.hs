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

module MacroProcessor (
    MacroState,
    MacroError(..),
    do_process,
    strip_newlines
)
where

import qualified Data.Set as Set
import Data.Set (Set,(\\))
import qualified Data.Map as Map
import Data.Map (Map,(!))
import qualified Data.List as List
import Data.Maybe

import Text.Parsec hiding (State)
import Control.Monad.State.Lazy
import Control.Monad.Except

import Text.Parsec.Expr
import Text.Read (readMaybe)
import qualified Text.Parsec.Pos as Pos

import Types

{- ### MACRO PROCESSOR ### -}
type VarFrame = Map String VarTree
data Macro = Macro { patterns :: [SubstRule],
                     closure :: VarFrame }

data MacroState = MacroState { macros :: Map String Macro,
                               vars :: VarFrame,
                               stack :: [VarFrame] }

data MacroError =   AllRulesFailed [SubstRule]
                  | CantUnify ([Code],[Code])
                  | UnknownVar String
                  | IllegalInstance String
                  | InconsistentLengths [String]
                  | NotAList String [Code]
                  | ArithError ParseError
                  | InternalError String
                  | ErrorInCall [Code] MacroError

-- ## pretty-print error ## --
instance Show MacroError where
    show (AllRulesFailed rs) =   "No patterns matched in macro application. List of failure points:\n"
                               ++(List.intercalate "\n" $ map (\(x,y) -> "  PATTERN: "++(take 300 $ show x)++"...\n"++
                                                                         "  INPUT:   "++(take 300 $ show y)++"...") rs)
    show (UnknownVar s) = "Unknown variable $"++s++"."
    show (IllegalInstance s) = "Can't directly instantiate list $"++s++"."
    show (InconsistentLengths vs) = "Inconsistent list lengths in lockstep iteration: "++(List.intercalate "," $ map ('$':) vs)++"."
    show (NotAList s v) = "Can't iterate over non-list: $"++s++"=("++(show v)++")."
    show (InternalError s) = "Internal error: "++s++"."
    show (ArithError e) = "Arithmetic error: "++(show e)++"."
    show (ErrorInCall c e) = "In macro invocation "++(take 50 $ to_string_all c)++"...:\n"++(show e)

type MacroProcess a = ExceptT MacroError a

-- add/del macro @define (always global)
addMacro n c p = do st <- get
                    put $ st { macros = Map.insert n (Macro { closure = c, patterns = p}) (macros st) }
delMacro n = do st <- get
                put $ st { macros = Map.delete n (macros st) }
-- add variable in current stack frame
addLocal n v = do st <- get
                  case stack st of
                    (hd:tl) -> put $ st { stack = (Map.insert n v hd):tl }
                    [] -> addGlobal n v
-- add global variable
addGlobal n v = do st <- get
                   put $ st { vars = Map.insert n v (vars st) }
-- get/set variable (stack from top to bottom, then global)
setVar n v = do st <- get
                let walkStack (r:rs) seen = if Map.member n r
                                            then put $ st { stack = (List.reverse seen) ++ (Map.insert n v r):rs }
                                            else walkStack rs (r:seen)
                    walkStack [] _ = if Map.member n (vars st)
                                     then put $ st { vars = Map.insert n v (vars st) }
                                     else throwError $ UnknownVar n
                    in walkStack (stack st) []
getVar n = do st <- get
              let walkStack n (r:rs) = if Map.member n r
                                       then return $ r!n
                                       else walkStack n rs
                  walkStack n [] = if Map.member n (vars st)
                                   then return $ (vars st)!n
                                   else throwError $ UnknownVar n
                  in walkStack n (stack st)

-- push stack frame
pushFrame :: VarFrame -> ExceptT MacroError (State MacroState) ()
pushFrame f = do st <- get
                 put $ st { stack = f:(stack st) }

-- pop stack frame
popFrame :: ExceptT MacroError (State MacroState) VarFrame
popFrame = do st <- get
              case stack st of 
                (hd:tl) -> do put $ st { stack = tl }
                              return hd
                [] -> throwError $ InternalError "Popping from empty frame stack."

-- convert list of frames to frame of lists
-- require: all frames are over the same variables
commuteFrames :: [VarFrame] -> VarFrame
commuteFrames [] = Map.empty
commuteFrames [f] = Map.map (Star.(:[])) f
commuteFrames (f:fs) = Map.mapWithKey (\n (Star vs) -> Star ((f!n):vs)) (commuteFrames fs)

show_vartree :: VarTree -> [Code]
show_vartree (Leaf c) = c
show_vartree (Star l) = (List.intersperse (Other ",") $ map (Parentheses. show_vartree) l)

-- ## replace all newlines with s ## --
strip_newlines ((NewLine s _ _):cs) = (WhiteSpace s):(strip_newlines cs)
strip_newlines ((Parentheses c):cs) = (Parentheses $ strip_newlines c):(strip_newlines cs)
strip_newlines ((SquareBrackets c):cs) = (SquareBrackets $ strip_newlines c):(strip_newlines cs)
strip_newlines ((CurlyBraces c):cs) = (CurlyBraces $ strip_newlines c):(strip_newlines cs)
strip_newlines (c:cs) = c:(strip_newlines cs)
strip_newlines [] = []

-- trim whitespace at beginning and end of code
trim ((WhiteSpace _):cs) = trim cs
trim ((NewLine _ _ _):cs) = List.reverse $ trim' $ List.reverse cs
trim [] = []
trim cs = trim ((NewLine "" "" 0):cs) 
trim' ((WhiteSpace _):cs) = trim' cs
trim' ((NewLine _ _ _):cs) = cs
trim' [] = []
trim' cs = cs

{- ## process a code block ## -}
do_process l = fst $ runState (runExceptT $ process l) $ MacroState { macros = Map.empty, vars = Map.empty, stack = [Map.empty] }
-- name we may be interested in
process ((Name s):cs) = do st <- get
                           if Map.notMember s (macros st)
                           then do cs' <- process cs
                                   return $ (Name s):cs'
                           else let b = ((macros st)!s)
                                    in do pushFrame (closure b)
                                          (r',cs') <- try_call (patterns b) cs `catchError` \e -> throwError $ ErrorInCall ((Name s):cs) e
                                          cs'' <- process cs'
                                          return (r'++cs'')
                                   -- popFrame
                                   -- process cs' -- we substituted and bound vars successfully
-- explicit match
process ((Match c rs):cs) = do c' <- process c
                               pushFrame Map.empty
                               (c'',_) <- try_call rs c'    -- ignore unmatched tail
                               cs' <- process cs
                               return $ c''++cs'
-- arithemtic
process ((Calc c):cs) = do c' <- process c
                           c'' <- calc c'
                           cs' <- process cs
                           return $ c''++cs'
-- variable read
process ((Var s):cs) = do v <- getVar s
                          process (show_vartree v ++ cs)
-- variable write
-- todo?: split DefLocal etc. into (DefLocal String):(VarTree): so we can @set $var1 $var2 
-- (other solution?)
process ((DefLocal s v):cs) = do v' <- process_vartree v
                                 addLocal s v'
                                 process cs
process ((DefGlobal s v):cs) = do v' <- process_vartree v
                                  addGlobal s v'
                                  process cs
process ((SetVar s v):cs) = do v' <- process_vartree v
                               setVar s v'
                               process cs
process ((PushBack s v):cs) = do v' <- process_vartree v
                                 w <- getVar s
                                 case w of (Leaf _) -> throwError $ NotAList s ((PushBack s v):cs)
                                           (Star l) -> do setVar s (Star (l++[v']))
                                                          process cs
-- loop
process ((LoopOver (bto,bfrom) c s):cs) = do -- bfrom is [ source1, ..., sourcen ]; vfrom is transpose of [ [ source1val1, ..., source1valm ], ..., [ ... sourcenvalm] ]
                                             -- hence [ [ source1val1, ..., sourcenval1], ..., [ ... sourcenvalm] ]
                                             vfrom <- liftM List.transpose $ mapM
                                                        (\n -> do vs <- getVar n
                                                                  case vs of
                                                                     (Leaf v) -> throwError $ NotAList n v
                                                                     (Star l) -> return l)
                                                        bfrom
                                             if List.any (\x -> (List.length bto) /= (List.length x)) vfrom
                                             then throwError $ InconsistentLengths bfrom
                                             else return []
                                             -- workloads of shape ( stack frame, code  )
                                             pieces <- mapM ( \(f,c) -> do pushFrame f
                                                                           c' <- process c
                                                                           popFrame
                                                                           return c' ) $
                                                            List.intersperse ( Map.empty, s ) $ map (\x -> ( Map.fromList $ List.zip bto x, c)) vfrom
                                             cs' <- process cs
                                             return $ (concat pieces)++cs'
-- define, undef
process ((Define s p):cs) = do st <- get
                               addMacro s (Map.unions (stack st)) p
                               process cs
process ((Undef s):cs) = do delMacro s
                            process cs
-- ignore for preprocessing
process ((Direct c):cs) = do cs' <- process cs
                             return $ c++cs'
-- passthrough
process ((Parentheses c):cs) = do c' <- Parentheses <$> process c
                                  cs' <- process cs
                                  return $ c':cs'
process ((CurlyBraces c):cs) = do c' <- CurlyBraces <$> process c
                                  cs' <- process cs
                                  return $ c':cs'
process ((SquareBrackets c):cs) = do c' <- SquareBrackets <$> process c
                                     cs' <- process cs
                                     return $ c':cs'
-- debug
process (DumpStack:cs) = do st <- get
                            cs' <- process cs
                            return $ (Other $ "/* "++ (show $ stack st) ++" */"):cs'
process (other:cs) = do cs' <- process cs
                        return $ other:cs'
process [] = return []

-- ## process variable assignment ## --
process_vartree (Leaf cs) = process cs >>= return.Leaf
process_vartree (Star sub) = mapM process_vartree sub >>= return.Star

-- shallowCodePrefixOf a b | trace ("scpf " ++ show a ++ " " ++ show b) False = undefined
shallowCodePrefixOf ((WhiteSpace _):xs) ys = shallowCodePrefixOf xs ys
shallowCodePrefixOf xs ((WhiteSpace _):ys) = shallowCodePrefixOf xs ys
shallowCodePrefixOf ((NewLine _ _ _):xs) ys = shallowCodePrefixOf xs ys
shallowCodePrefixOf xs ((NewLine _ _ _):ys) = shallowCodePrefixOf xs ys
shallowCodePrefixOf [] _ = True
shallowCodePrefixOf _ [] = False
shallowCodePrefixOf ((Parentheses _):xs) ((Parentheses _):ys) = shallowCodePrefixOf xs ys
shallowCodePrefixOf ((SquareBrackets _):xs) ((SquareBrackets _):ys) = shallowCodePrefixOf xs ys
shallowCodePrefixOf ((CurlyBraces _):xs) ((CurlyBraces _):ys) = shallowCodePrefixOf xs ys
shallowCodePrefixOf (x:xs) (y:ys) = x==y && shallowCodePrefixOf xs ys

-- ## clean up stack frame and rethrow error ## --
with_frame f = catchError f (\e -> do popFrame; throwError e)

-- ## collect all (unquantified) variables in piece of code ## --
collect_vars ((Var s):ls) = (s,Star []):(collect_vars ls)
collect_vars ((CaptureUntil s _):ls) = (s,Star []):(collect_vars ls)
collect_vars ((Parentheses c):ls) = (collect_vars c)++(collect_vars ls)
collect_vars ((SquareBrackets c):ls) = (collect_vars c)++(collect_vars ls)
collect_vars ((CurlyBraces c):ls) = (collect_vars c)++(collect_vars ls)
collect_vars (_:ls) = collect_vars ls
collect_vars [] = []

{- ## try unifying with LHS of substitution rule ## -}
try_unif :: [Code] -> [Code] -> ExceptT MacroError (State MacroState) [Code]
-- done; return tail, don't eat excess whitespace
try_unif [] rs = return rs
-- whitespace has no impact on unification. These rules need to be here to prevent whitespace capture by Var
try_unif ((WhiteSpace _):ls) rs = try_unif ls rs
try_unif ls ((WhiteSpace _):rs) = try_unif ls rs
try_unif ((NewLine _ _ _):ls) rs = try_unif ls rs
try_unif ls ((NewLine _ _ _):rs) = try_unif ls rs
-- expand a var that is about to be captured! Don't eval calls though, because we might get weird scenarios of the form macrocall macrocall macrocall...
try_unif (l:ls) ((Var s):rs) = do v <- getVar s
                                  try_unif (l:ls) (show_vartree v ++ rs)
-- capture single var 
try_unif ((Var s):ls) (r:rs) = do addLocal s (Leaf [r])
                                  try_unif ls rs
try_unif ((CaptureName s):ls) ((Name n):rs) = do addLocal s (Leaf [Name n])
                                                 try_unif ls rs
try_unif ((CaptureUntil s b):ls) rs = let capture (r:rs)
                                            | List.any (`shallowCodePrefixOf` (r:rs)) b = ([],r:rs)
                                            | otherwise = let (cs,rs') = capture rs in (r:cs,rs')
                                          capture [] = ([],[])
                                          (cs,rs') = capture rs
                                      in do addLocal s (Leaf cs)
                                            try_unif ls rs'
-- repetitions --
try_unif ((ZeroOrMore c s):ls) rs = do st <- get
                                       (try_unif ((OneOrMore c s):ls) rs
                                          `catchError` (\e -> do put $ st { stack = ((Map.fromList $ collect_vars c) `Map.union` (head $ stack st)):(tail $ stack st) }
                                                                 try_unif ls rs))
try_unif ((OneOrMore c s):ls) rs = let mklist rs = do pushFrame Map.empty
                                                      rs' <- with_frame $ try_unif c rs
                                                      frame <- popFrame
                                                      catchError (do rs'' <- try_unif s rs'
                                                                     (rs''',frames) <- mklist rs''
                                                                     return (rs''',frame:frames))
                                                                 (\_ -> return (rs',[frame]))
                                       in do (rs',fs) <- mklist rs
                                             st <- get
                                             put $ st { stack = ((commuteFrames fs) `Map.union` (head $ stack st)):(tail $ stack st) }
                                             try_unif ls rs'
-- substructure 
-- process contents fully before unifying!
try_unif ((Parentheses l):ls) ((Parentheses r):rs) = do r' <- process r
                                                        tail <- try_unif l r
                                                        if (trim tail) == [] then try_unif ls rs
                                                        else throwError $ CantUnify (l,r)
try_unif ((CurlyBraces l):ls) ((CurlyBraces r):rs) = try_unif ((Parentheses l):ls) ((Parentheses r):rs)
try_unif ((SquareBrackets l):ls) ((SquareBrackets r):rs) = try_unif ((Parentheses l):ls) ((Parentheses r):rs)

try_unif (l:ls) (r:rs) = if l == r then try_unif ls rs else throwError $ CantUnify (l:ls, r:rs)
-- out of tokens to match!
try_unif x y = throwError $ CantUnify (x,y)

-- ## try list of substitution rules in turn; error if all fail
try_call2 :: [SubstRule] -> [Code] -> [([Code],[Code])] -> ExceptT MacroError (State MacroState) ([Code],[Code])
try_call2 ((l,r):rs) cs past_errors = do st <- get
                                         case runState (runExceptT $ try_unif l cs) st of
                                            (Left (CantUnify e),st') -> try_call2 rs cs (e:past_errors)
                                            (Left e',_) -> throwError e'
                                            (Right cs',st') -> do put st'
                                                                  r' <- liftM trim $ process r
                                                                  popFrame
                                                                  -- cs'' <- process cs'
                                                                  return (r', cs')
try_call2 [] cs past_errors = throwError $ AllRulesFailed past_errors
try_call rs cs = try_call2 rs cs []


{- ## macro-time arithmetic computations with @calc ## -}
atable = [ [unary (Operator "-") negate],
           [binary (Operator "*") (*) AssocLeft, binary (Operator "/") div AssocLeft],
           [binary (Operator "+") (+) AssocLeft, binary (Operator "-") (-) AssocLeft] ]
binary n f a = Infix (satisfy' (==n) >> return f) a
unary n f = Prefix (satisfy' (==n) >> return f)

satisfy' f = tokenPrim (\c -> show c)
                       (\p _ _ -> Pos.updatePosChar p 'x')
                       (\c -> if f c then Just c else Nothing)

is_parens (Parentheses _) = True
is_parens _               = False
arith_parens = do (Parentheses c) <- satisfy' is_parens
                  case sub_calc c of
                    (Left e) -> mkPT (\_ -> return (Empty (return (Error e))))
                    (Right n) -> return n

is_number (Other n) = ((readMaybe n :: Maybe Int) /= Nothing)
is_number _         = False
arith_number = do (Other n) <- satisfy' is_number
                  return $ (read n :: Int)

arith_term = arith_parens <|> arith_number

-- arith_process :: Parsec [Code] () Int
arith_process = buildExpressionParser atable arith_term

sub_calc c = parse ( do i <- arith_process
                        eof
                        return i ) "@calc" c 

--  
calc :: [Code] -> ExceptT MacroError (State MacroState) [Code]
calc c = case sub_calc c of
            Left e -> throwError $ ArithError e 
            Right n -> return [Other $ show n]




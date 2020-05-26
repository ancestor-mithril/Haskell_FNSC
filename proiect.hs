import Data.Char
import System.Environment

-- And (Var "x") (Var "y")
-- Or (Var "x") (Var "y")
-- Or (Var "x") (Var "y")
-- Iff (Var "x") (Var "y")
-- Exists "x" (Var "y")
-- Forall "x" (Var "y")
-- Not (Var "x")

data Formula = Var [String]
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Iff Formula Formula
  | Exists String Formula
  | Forall String Formula
  | Not Formula
  deriving Eq


instance Show Formula where
  show (Var x) = case (length x) == 1 of
                  True -> "Var" ++ (show x)
                  False -> "Var" ++ "{" ++ "h_" ++ (show ((length x) - 1)) ++ show (tail x) ++ "}"
  show (Not f) = "!" ++ (show f)
  show (And f1 f2) = "(" ++ (show f1) ++ " & " ++ (show f2) ++ ")"
  show (Or f1 f2) = "(" ++ (show f1) ++ " | " ++ (show f2) ++ ")"
  show (Implies f1 f2) = "(" ++ (show f1) ++ " -> " ++ (show f2) ++ ")"
  show (Iff f1 f2) = "(" ++ (show f1) ++ " <=> " ++ (show f2) ++ ")"
  show (Exists x f) = "(" ++ "Exists" ++ "(" ++ x ++ ")" ++ "." ++ "(" ++ (show f) ++ ")" ++ ")"
  show (Forall x f) = "(" ++ "Forall" ++ "(" ++ x ++ ")" ++ "." ++ "(" ++ (show f) ++ ")" ++ ")"





data Token = TVar [String]
  | TLParen 
  | TRParen 
  | TAnd 
  | TOr 
  | TNot 
  | TImplies 
  | TIff
  | TExists
  | TForall
  | TDot
  deriving (Eq, Show)

-- tokenize "(a&b)"
-- tokenize "a&(c|d)->(a<=>d)"
-- tokenize "Exists yx . (a&(c|d)->(a<=>d))"

first :: String -> String
first [] = []
first (c : tl) | isAlpha c = c : (first tl)
first _ = []

after_first :: String -> String
after_first [] = []
after_first (c : tl) | isAlpha c = (after_first tl)
after_first (c : tl) = c : tl

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(' : tl) = TLParen : (tokenize tl)
tokenize (')' : tl) = TRParen : (tokenize tl)
tokenize ('&' : tl) = TAnd : (tokenize tl)
tokenize ('|' : tl) = TOr : (tokenize tl)
tokenize ('!' : tl) = TNot : (tokenize tl)
tokenize ('-' : '>' : tl) = TImplies : (tokenize tl)
tokenize ('<' : '=' : '>' : tl) = TIff : (tokenize tl)
tokenize ('E' : 'x' : 'i' : 's' : 't' : 's' : tl) = TExists : (tokenize tl)
tokenize ('F' : 'o' : 'r' : 'a' : 'l' : 'l' : tl) = TForall : (tokenize tl)
tokenize ('.' : tl) = TDot : (tokenize tl)
tokenize (c : tl) | isAlpha c = (TVar [(first(c:tl))] : (tokenize (after_first(c:tl))))
tokenize (c : tl) | isSpace c = tokenize tl
tokenize _ = error "No tokenization."


parse_form = parse_exists
-- tokenize "(!!!a)"
-- parse_form(tokenize "!a")
-- parse_form(tokenize "!a&(bd&c&!(D&!a))")
-- parse_form [TNot,TNot,TNot,TVar "x", TAnd, TVar "y", TAnd, TVar "z"]
-- parse_form [TNot,TNot,TNot,TVar "x", TAnd, TVar "y", TAnd, TVar "z", TOr, TVar "x", TOr, TVar "y"]
-- parse_form [TNot,TNot,TNot,TVar "x", TAnd, TVar "y", TImplies, TVar "z", TOr, TVar "x", TOr, TVar "y"]
-- parse_form (tokenize "(!a&(bd&c&!(D&!a)))<=>(a|b->c)")
-- parse_form (tokenize "Exists z. Exists x.(!a&(bd&c&!(D&!a)))<=>(Exists x. (x -> y))")
-- parse_form (tokenize "Forall y.Exists x.x -> y")
-- parse_form (tokenize "Exists z. Forall x.(!a&(bd&(Forall c.c)&!(D&!(Exists a.a|d))))<=>(Exists x. (x -> y))")
-- parse_form (tokenize "Exists z. Forall x.(!a&(bd&Forall c.c&!(D&!(Exists a.a|d))))<=>(Exists x. (x -> y))")


get_formula :: Maybe (Formula, [Token]) -> Formula
get_formula (Just (f, tokens)) = f           
                   
                   
parse_exists :: [Token] -> Maybe (Formula, [Token])
parse_exists (TExists : TVar var : TDot : tokens) = case parse_exists tokens of
  Nothing -> Nothing
  Just (f, tokens') -> Just (Exists (head var) f, tokens')
parse_exists (TForall : TVar var : TDot : tokens) = case parse_exists tokens of
  Nothing -> Nothing
  Just (f, tokens') -> Just (Forall (head var) f, tokens')
parse_exists tokens = parse_eq tokens
                   
           
parse_eq :: [Token] -> Maybe (Formula, [Token])
parse_eq tokens =
  case parse_impls tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TIff : tokens') ->
      case parse_impls tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (Iff f1 f2, tokens'')
    r -> r


parse_impls :: [Token] -> Maybe (Formula, [Token])
parse_impls tokens =
  case parse_disjs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TImplies : tokens') ->
      case parse_impls tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (Implies f1 f2, tokens'')
    r -> r
                   
                   
parse_disjs :: [Token] -> Maybe (Formula, [Token])
parse_disjs tokens =
  case parse_conjs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TOr : tokens') ->
      case parse_disjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (Or f1 f2, tokens'')
    r -> r
                   
                   
parse_conjs :: [Token] -> Maybe (Formula, [Token])
parse_conjs tokens =
  case parse_negs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TAnd : tokens') ->
      case parse_conjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (And f1 f2, tokens'')
    r -> r
                   
                   
                   
                   
                   
parse_negs :: [Token] -> Maybe (Formula, [Token])
parse_negs (TVar var : tokens) = Just (Var var, tokens)
parse_negs (TNot : tokens) = case parse_negs tokens of
  Nothing -> Nothing
  Just (f, tokens') -> Just (Not f, tokens')
parse_negs (TExists : tokens) = parse_form (TExists : tokens)
parse_negs (TForall : tokens) = parse_form (TForall : tokens)
parse_negs (TLParen : tokens) = case parse_form tokens of
  Nothing -> Nothing
  Just (f, TRParen : tokens') -> Just (f, tokens')
  _ -> Nothing
parse_                   negs _ = Nothing
                   
                   
plus :: Int -> Int -> Int
plus a b = a+b
                   
                   
                   
                   
                   
iff :: Formula -> Formula
iff (Iff f1 f2) = And (Implies f1 f2) (Implies f2 f1)
iff f = f
                   
implies :: Formula -> Formula
implies (Implies f1 f2) = (Or (Not f1) f2)
implies f = f
                   
not_and :: Formula -> Formula
not_and (Not (And f1 f2)) = (Or (Not f1) (Not f2))
not_and f = f
                   
not_or :: Formula -> Formula
not_or (Not (Or f1 f2)) = (And (Not f1) (Not f2))
not_or f = f
                   
                   
or_and_right :: Formula -> Formula
or_and_right (Or f1 (And f2 f3)) = (And (Or f1 f2) (Or f1 f3))
or_and_right f = f
                   
                   
or_and_left :: Formula -> Formula
or_and_left (Or (And f1 f2) f3) = (And (Or f1 f3) (Or f2 f3))
or_and_left f = f
                   
                   
and_assoc :: Formula -> Formula
and_assoc (And (And f1 f2) f3) = (And f1 (And f2 f3))
and_assoc f = f
                   
                   
or_assoc :: Formula -> Formula
or_assoc (Or (Or f1 f2) f3) = (Or f1 (Or f2 f3))
or_assoc f = f
                   
not_not :: Formula -> Formula
not_not (Not (Not f)) = f
not_not f = f
                   
-- o functie care sa aplice orice transformare
chain :: (Formula -> Formula) -> (Formula -> Formula) -> Formula -> Formula
chain t1 t2 f = let f' = t1 f in
                  if f == f' then t2 f
                  else f'
                   
chain_list :: [Formula -> Formula] -> (Formula -> Formula)
chain_list [] = \x -> x
chain_list (hd : tl) = chain hd (chain_list tl)
                   
                   
-- aplica exact una dintre transformarile atomice in radacina
topmost :: Formula -> Formula
topmost = chain_list [ not_not, or_assoc, and_assoc, or_and_right, or_and_left, not_or, not_and, implies, iff ]
                   
-- applica exact o transformare dintre cele 8
once :: Formula -> Formula
once f = apply_once f topmost
                   
-- punctul fix al unei transformari de la Formula la Formula
fixpoint :: (Formula -> Formula) -> (Formula -> Formula)
fixpoint t f = let f' = t f in
                 if f == f' then f
                 else fixpoint t f'
                   
-- punctul fix al functiei once
cnf :: Formula -> Formula
cnf = fixpoint once
                   
-- aplica exact una dintre transformarile atomice oriunde in formula (la radacina sau in subformule)
apply_once :: Formula -> (Formula -> Formula) -> Formula
apply_once f t =
  let f' = t f in
    if f == f' then
      apply_deep f t
    else
      f'
                   
                   
-- aplica exact una dintre transformarile atomice in subformulele stricte (nu la radacina)
apply_deep :: Formula -> (Formula -> Formula) -> Formula
apply_deep (Not f) t = (Not (apply_once f t))
apply_deep (And f1 f2) t = let f1' = apply_once f1 t in
                             if f1 == f1' then
                               (And f1 (apply_once f2 t))
                             else
                               (And f1' f2)
apply_deep (Or f1 f2) t = let f1' = apply_once f1 t in
                             if f1 == f1' then
                               (Or f1 (apply_once f2 t))
                             else
                               (Or f1' f2)
apply_deep (Implies f1 f2) t = let f1' = apply_once f1 t in
                             if f1 == f1' then
                               (Implies f1 (apply_once f2 t))
                             else
                               (Implies f1' f2)
apply_deep (Iff f1 f2) t = let f1' = apply_once f1 t in
                          if f1 == f1' then
                            (Iff f1 (apply_once f2 t))
                          else
                            (Iff f1' f2)
apply_deep (Exists var f) t = (Exists var (apply_once f t))
apply_deep (Forall var f) t = (Forall var (apply_once f t))
apply_deep (Var x) _ = (Var x)
                   


f1=get_formula(parse_form (tokenize "Forall y.Exists x.x -> y"))
cnf_f1=cnf f1
-- cnf (formula) -> formula in cnf


-- replace (formula, str1, str2) -> formula cu toate ocurentele lui str1 transformate in str2
replace :: Formula -> [String] -> [String] -> Formula
replace (Var var) str1 str2 = if (var == str1) then (Var str2) else (Var var)
replace (Not f) str1 str2 = (Not (replace f str1 str2))
replace (Forall var f) str1 str2 = (Forall var (replace f str1 str2))
replace (Exists var f) str1 str2 = (Exists var (replace f str1 str2))
replace (Or f1 f2) str1 str2 = (Or (replace f1 str1 str2) (replace f2 str1 str2))
replace (And f1 f2) str1 str2 = (And (replace f1 str1 str2) (replace f2 str1 str2))
replace (Implies f1 f2) str1 str2 = (Implies (replace f1 str1 str2) (replace f2 str1 str2))
replace (Iff f1 f2) str1 str2 = (Iff (replace f1 str1 str2) (replace f2 str1 str2))

variable_1 = "substitute_"

--substitute (numar) -> "substitute_numar"
substitute :: Int -> String
substitute nr = variable_1 ++ show(nr)

-- primul pas fnp -> redenumirea pentru a putea aplica regulile 
-- folosim al doilea argument pentru a redenumi in mod unic toate variabilele legate 
rename :: Formula -> Int -> Formula
rename (Var var) n = (Var var)
rename (Not f) n = Not (rename f (n*2) )
rename (And f1 f2) n = And (rename f1 (n*2)) (rename f2 (n*2+1))
rename (Or f1 f2) n = Or (rename f1 (n*2)) (rename f2 (n*2+1))
rename (Implies f1 f2) n = Implies (rename f1 (n*2)) (rename f2 (n*2+1))
rename (Iff f1 f2) n = Iff (rename f1 (n*2)) (rename f2 (n*2+1))
rename (Exists var f) n = Exists (substitute n) (rename (replace f [var] [(substitute n)]) (n*2)) 
rename (Forall var f) n = Forall (substitute n) (rename (replace f [var] [(substitute n)]) (n*2)) 
-- Inlocuim toate aparitiile variabilei legate cu Forall cu stringul (substitute n)


f2=get_formula(parse_form (tokenize "Exists z. Forall x.(!a&(bd&(Forall c.c)&!(D&!(Exists a.a|d))))<=>(Exists x. (x -> y))"))
f2_renamed=rename f2 1
cnf_f2_renamed=cnf f2_renamed



-- al doilea pas fnp : aplicarea echivalentelor pentru a scoate cuantificatorii in fata
forall_and :: Formula -> Formula
forall_and (And (Forall var f1) f2) = Forall var (And f1 f2)
forall_and f = f

and_forall :: Formula -> Formula
and_forall (And f1 (Forall var f2)) = Forall var (And f1 f2)
and_forall f = f


forall_or :: Formula -> Formula
forall_or (Or (Forall var f1) f2) = Forall var (Or f1 f2)
forall_or f = f

or_forall :: Formula -> Formula
or_forall (Or f1 (Forall var f2)) = Forall var (Or f1 f2)
or_forall f = f


exists_or :: Formula -> Formula
exists_or (Or (Exists var f1) f2) = Exists var (Or f1 f2)
exists_or f = f

or_exists :: Formula -> Formula
or_exists (Or f1 (Exists var f2)) = Exists var (Or f1 f2)
or_exists f = f

exists_and :: Formula -> Formula
exists_and (And (Exists var f1) f2) = Exists var (And f1 f2)
exists_and f = f

and_exists :: Formula -> Formula
and_exists (And f1 (Exists var f2)) = Exists var (And f1 f2)
and_exists f = f

not_forall :: Formula -> Formula
not_forall (Not (Forall var f)) = Exists var (Not f)
not_forall f = f

not_exists :: Formula -> Formula
not_exists (Not (Exists var f)) = Forall var (Not f)
not_exists f = f


-- aplica exact una dintre transformarile atomice in radacina
topmost_fnp :: Formula -> Formula
topmost_fnp = chain_list [iff, implies, not_not, forall_and, and_forall, forall_or, or_forall, exists_and, and_exists, exists_or, or_exists, not_forall, not_exists]


-- applica exact o transformare dintre cele 13
once_fnp :: Formula -> Formula
once_fnp f = apply_once f topmost_fnp

-- fnp (formula) -> formula in fnp
fnp :: Formula -> Formula
fnp = fixpoint once_fnp

-- dupa obtinerea formulei in fnp, se face inchiderea acesteia

variable_2 = "x_"


-- vars_name (fnp f2_renamed) [] []
vars_name :: Formula -> [Formula] -> [[String]] -> [[String]]
vars_name (Var var) formulas vars = case (elem var vars) of
                                True -> case null formulas of
                                          True -> vars
                                          False -> vars_name (head formulas) (tail formulas) (vars)
                                False -> case null formulas of
                                          True -> (var:vars)
                                          False -> vars_name (head formulas) (tail formulas) (var:vars)
vars_name (Not f) fs vars = vars_name f fs vars
vars_name (And f1 f2) fs vars = vars_name f1 (f2:fs) vars
vars_name (Or f1 f2) fs vars = vars_name f1 (f2:fs) vars
vars_name (Exists var f) fs vars = vars_name f fs vars
vars_name (Forall var f) fs vars = vars_name f fs vars


-- binded_vars (fnp f2_renamed)
binded_vars :: Formula -> [[String]]
binded_vars (Exists var f) = [var] : (binded_vars f)
binded_vars (Forall var f) = [var] : (binded_vars f)
binded_vars _ = []

-- free (all variables) (binded variables) -> (all -- binded) variables
-- free (vars_name (fnp f2_renamed) [] []) (binded_vars (fnp f2_renamed)) []
free :: [[String]] -> [[String]] -> [[String]] -> [[String]]
free [] binded free_vars = free_vars
free (v:vars) binded free_vars = case elem v binded of
                                  True -> (free vars binded free_vars)
                                  False -> (free vars binded (v:free_vars))



-- close (fnp f2_renamed) (free (vars_name (fnp f2_renamed) [] []) (binded_vars (fnp f2_renamed)) [])
-- close (formula, string) | formula e in fnp -> formula in fnp inchisa existential
close :: Formula -> [[String]] -> Formula
close f [] = f
close f (v:vars) = Exists (head v) $ close f vars

close_f2=close (fnp f2_renamed) (free (vars_name (fnp f2_renamed) [] []) (binded_vars (fnp f2_renamed)) [])

f3=rename(get_formula(parse_form(tokenize "Forall x. Forall y. Exists z. z"))) 1
close_f3 = close (fnp f3) (free (vars_name (fnp f3) [] []) (binded_vars (fnp f3)) [])


-- fns close_f2 ["f"]
-- fns close_f3 ["f"]
fns :: Formula -> [String] -> Formula
fns (Forall var f) vars = Forall var (fns f (var : vars))
fns (Exists var f) vars = case null vars of
                            True -> fns f vars
                            False -> fns (replace f [var] (reverse vars)) vars
fns f vars = f

fnsc2 = fnsc (fns close_f2 ["f"])
fnsc3 = fnsc (fns close_f3 ["f"])

fnsc :: Formula -> Formula
fnsc (Forall var f) = Forall var $ fnsc f
fnsc f = cnf f


-- Pasi: phi -> FNP -> Inchiderea ->  FNS  -> FNSC


main :: IO()
main = do
  args <- getArgs
  file_content <- readFile $ head args
  let tokens = tokenize file_content
  putStrLn "Tokenization: "
  putStrLn $ show tokens
  putStrLn ""
  let parsed_formula = get_formula $ parse_form tokens
  putStrLn "Parsed Formula: "
  putStrLn $ show parsed_formula
  putStrLn ""
  let renamed_formula = rename parsed_formula 1
  putStrLn "Renamed Formula: "
  putStrLn $ show renamed_formula
  putStrLn ""
  let fnp_formula = fnp renamed_formula
  putStrLn "Formula in fnp"
  putStrLn $ show fnp_formula
  putStrLn ""
  let all_variables = vars_name fnp_formula [] []
  putStrLn "All variable in formula"
  putStrLn $ show all_variables
  putStrLn ""
  let binded_variables = binded_vars fnp_formula
  putStrLn "All binded variables in formula"
  putStrLn $ show binded_variables
  putStrLn ""
  let free_variables = free all_variables binded_variables []
  putStrLn "All free variables in formula"
  putStrLn $ show free_variables
  putStrLn ""
  let closed_formula = close fnp_formula free_variables
  putStrLn "Closed fnp formula"
  putStrLn $ show closed_formula
  putStrLn ""
  let fns_formula = fns closed_formula ["f"]
  putStrLn "fns formula"
  putStrLn $ show fns_formula
  putStrLn ""
  let fnsc_formula = fnsc fns_formula
  putStrLn "fnsc formula"
  putStrLn $ show fnsc_formula
  putStrLn ""
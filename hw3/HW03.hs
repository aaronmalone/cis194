module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend originalState name value = newState
  where newState str | str == name = value
                     | otherwise   = originalState str

empty :: State
empty _ = 0

newState :: String -> Int -> State
newState str num = extend empty str num

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state expr = case expr of
  Var name           -> state name
  Val num            -> num
  Op expr1 bop expr2 ->
      let result1 = evalE state expr1
          result2 = evalE state expr2
            in case bop of
              Plus   -> result1 + result2
              Minus  -> result1 - result2
              Times  -> result1 * result2
              Divide -> result1 `div` result2
              Gt     -> boolToInt (result1 > result2)
              Ge     -> boolToInt (result1 == result2 || result1 > result2)
              Lt     -> boolToInt (result1 < result2)
              Le     -> boolToInt (result1 == result2 || result1 < result2)
              Eql    -> boolToInt (result1 == result2)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var expr)           = DAssign var expr
desugar (If expr thenStmt elseStmt) = DIf expr (ds thenStmt) (ds elseStmt)
desugar (While expr stmt)           = DWhile expr (ds stmt)
desugar (Sequence stmt1 stmt2)      = DSequence (ds stmt1) (ds stmt2)
desugar (Skip)                      = DSkip
desugar (Incr var)                  = DAssign var (Op (Var var) Plus (Val 1))
desugar (For init cond upd body)    = DSequence (ds init) (DWhile cond (DSequence (ds body) (ds upd)))

ds = desugar


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state dietStatement = case dietStatement of
  DAssign var expr           -> extend state var (evalE state expr)
  DIf expr thenStmt elseStmt -> if evalE state expr == 1 then es state thenStmt else es state elseStmt
  DWhile expr stmt           -> if evalE state expr == 1 then es state (DSequence stmt (DWhile expr stmt)) else state
  DSequence stmt1 stmt2      -> es (es state stmt1) stmt2
  DSkip                      -> state


es = evalSimple;

run :: State -> Statement -> State
run state stmt = evalSimple state $ desugar stmt

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))

runFactorial :: Int -> Int
runFactorial = runStatement "In" "Out" factorial


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

runSquareRoot :: Int -> Int
runSquareRoot = runStatement "A" "B" squareRoot

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

runStatement :: String -> String -> Statement -> Int -> Int
runStatement inputVariable outputVariable statement input = (run (newState inputVariable input) statement) outputVariable

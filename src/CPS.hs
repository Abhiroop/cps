module CPS where

type Variable = String

data Value
  = Var    Variable
  | Label  Variable
  | Int    Int
  | Real   String
  | String String
  deriving (Show, Eq, Ord)

data Accesspath
  = OFFP Int
  | SELP Int Accesspath
  deriving (Show, Eq, Ord)

data PrimOp
  = MUL | ADD | MINUS | DIV
  | Twiddle
  | IEQL | INEQL | LT | LTE | GT | GTE
  | RangeChk
  | Not
  | Subscript
  | OrdOf
  | Assign | UnboxedAssign | Update | UnboxedUpdate
  | Store  | MakeRef       | MakeRefUnboxed
  | ALength | SLength
  | GetHdlr | SetHdlr
  | Boxed
  | FADD | FSUB | FDIV   | FMUL | FEQL | FNEQ | FGE | FGT
  | FLE  | FLT  | RSHIFT | LSHIFT
  | ORB  | ANDB | XORB   | NOTB
  deriving (Show, Eq, Ord)

data CExp =
    Record [(Value, Accesspath)] Variable CExp
  | Select Int Value Variable CExp
  | Offset Int Value Variable CExp
  | App    Value [Value]
  | Fix    [(Variable, [Variable], CExp)] CExp
  | Switch Value [CExp]
  | PrimOp PrimOp [Value] [Variable] [CExp]
  deriving (Show, Eq, Ord)


{-
c = a + b
PrimOp Plus [Var "a", Var "b"] [Var "c"] [Var "e"]


c = (a + 1) * (3 + c)
PrimOp Plus [Var "a", Int 1] ["u"]
 [PrimOp Plus [Int 3, Var "c"] ["v"]
   [PrimOp Mul [Var "u", Var "v"] ["e"] ["M"]]]


if a > b then F else G
PrimOp GT [Var "a", Var "b"] [] ["F", "G"]

w = (a, 2, c)  -- continuation is E
RECORD [(Var "a", OFFP 0), (Int 2, OFFP 0), (Var "c", OFFP 0)] w E

-}

foo a b c d =
  let f x = 2 * x + 1
  in f (a + b) * f (c + d)

foo1 a b c d r =
  let f x k = k (2 * x + 1)
      k1 i = let k2 j = r (i * j)
              in f (c + d) k2
   in f (a + b) k1


































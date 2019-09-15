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

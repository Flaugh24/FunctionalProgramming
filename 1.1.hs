data Operator = Plus | Minus | Mult deriving (Show, Eq)

data UnaryOperator = UnMinus deriving (Show, Eq)

data Term = IntConstant { intValue::Int }
          | Variable { varName::String }
          | BinaryTerm { lhv::Term, rhv::Term, op::Operator } 
          | UnaryTerm {unaryOp::UnaryOperator, value::Term} deriving (Show, Eq)

a <+> b = BinaryTerm a Plus b 

a <-> b = BinaryTerm a Minus b 

a <*> b = BinaryTerm a Mult b 

(<-->) a = UnaryTerm UnMinus a

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant intValue) _ _ = IntConstant intValue
replaceVar (Variable varName) var term = if varName == var then term else Variable varName
replaceVar (BinaryTerm lhv rhv op) var term = BinaryTerm (replaceVar lhv var term) (replaceVar rhv var term) op
replaceVar (UnaryTerm op trm) var term = UnaryTerm op (replaceVar trm var term)
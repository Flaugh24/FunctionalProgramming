import Prelude hiding (head, last, init, tail, (<*>))
import Control.Lens hiding (op)

import Term
import ReverseList


-- ������� � ������ ��������
operation :: Lens' Term Operation
operation = lens op (\term newOp -> term { op = newOp })

operationTestTerm = BinaryTerm (IntConstant 5) Plus (Variable "var")
operationGet = operationTestTerm^.operation
operationSet = operationTestTerm & operation .~ Minus

-- ������� � ������ ����������
var :: Lens' Term String
var = lens varName (\term newVar -> term {varName = newVar})

varTestTerm = Variable "Testing"
varGet = varTestTerm^.var
varSet = varTestTerm & var .~ "Tested"

-- BinaryTerm � ��������� �����������
leftMostMajorOp :: Lens' Term Term
leftMostMajorOp = lens leftMostMajorOpGetter leftMostMajorOpSetter

leftMostMajorOpTest = ((IntConstant 10) <-> (IntConstant 5)) <*> ((IntConstant 6) <*> (IntConstant 4))
leftMostMajorOpGet = leftMostMajorOpTest^.leftMostMajorOp
leftMostMajorOpSet = leftMostMajorOpTest & leftMostMajorOp .~ (Variable "Test")

-- ����� ��������� ��������� ���� (IntConstant ��� Variable)
rightMostTerm :: Lens' Term Term
rightMostTerm = lens rightMostTermGetter rightMostTermSetter

rightMostTermTest = ((IntConstant 10) <-> (IntConstant 5)) <*> ((IntConstant 6) <*> (IntConstant 4))
rightMostTermGet = rightMostTermTest^.rightMostTerm
rightMostTermSet = rightMostTermTest & rightMostTerm .~ (Variable "Test")


-- ������ ������� RList
head :: Lens' (ReverseList a) a
head = lens getter setter where
  getter (RCons _ x) = x
  setter (RCons xs x) newHead = RCons xs newHead

headTestRList = RCons (RCons RNil 5) 3
headGet = headTestRList^.head
headSet = headTestRList & head .~ 5

-- ��������� ������� RList
last :: Lens' (ReverseList a) a
last = lens getter setter where
  getter (RCons RNil x) = x
  getter (RCons xs x) = getter xs

  setter (RCons RNil x) newLast = RCons RNil newLast
  setter (RCons xs x) newLast = RCons (setter xs newLast) x

lastTestRList = RCons (RCons RNil 5) 3
lastGet = lastTestRList^.last
lastSet = lastTestRList & last .~ 3

-- ��� �������� RList ����� �������
tail :: Lens' (ReverseList a) (ReverseList a)
tail = lens getter setter where
  getter (RCons xs x) = xs
  setter (RCons xs x) newTail = RCons newTail x

tailTestRList = RCons (RCons (RCons RNil 10) 5) 3
tailGet = tailTestRList^.tail
tailSet = tailTestRList & tail .~ RNil
data Deque a = Deque [a] [a] deriving (Show)

-- ������ ������� ������ - ����� ����� �������.
-- ����� ������� ������ ~ �������� ������� �����.
-- ������ ������� ������ - ������ ����� �������.
-- ����� ������� ������ ~ �������� ������� ������.

-- ����� ������� append � appendLeft ������ O(1), �.�. (:) - O(1).
-- pop � popLeft ����� ������ O(1) �� ��� �� ��������, ����� ������,
-- ����� ���� �� ������� ��������, � � ������ ����� ������ ������ ��������.

append :: Deque a -> a -> Deque a
append (Deque in_ out) v = Deque in_ (v:out)


appendLeft :: Deque a -> a -> Deque a
appendLeft (Deque in_ out) v = Deque (v:in_) out


halve :: [a] -> ([a], [a])
halve lst = splitAt (length lst `div` 2) lst

equalize :: Deque a -> Deque a
equalize (Deque in_ []) = Deque (fst half) (reverse $ snd half) where
  half = halve in_
equalize (Deque [] out) = Deque (reverse $ snd half) (fst half) where
  half = halve out


pop :: Deque a -> (Deque a, a)
pop (Deque [] []) = error "Deque is empty"
pop (Deque in_ (h:t)) = (Deque in_ t, h)
-- ������, ����� ������ ������ ��������
pop (Deque (h:[]) []) = (Deque [] [], h)
pop deque@(Deque in_ []) = pop $ equalize deque

popLeft :: Deque a -> (Deque a, a)
popLeft (Deque [] []) = error "Deque is empty"
popLeft (Deque (h:t) out) = (Deque t out, h)
-- ������, ����� ����� ������ ��������
popLeft (Deque [] (h:[])) = (Deque [] [], h)
popLeft deque@(Deque [] out) = popLeft $ equalize deque

-- ����� �������:
--   ������������ 1$ �� ������ ����������.
--   ��� �������� �� ���� ����� ���������� ������, ����� ������ ������ �������.
--   ����� ����� ����������� "�������������".
--   ������������ � ���� ������ N/2, ��� N �� ��� ���������� �� �����������.
--   ��������������, ���������������� ��������� ���� �������� ��� ����� - ���������.
--
-- ����� ������:
--   ��������� - ����� ���� �������.
--   ���������� ����������� ��������� �� 1.
--   �������� ��� "�������������" �� ������ ���������.
--   �������� � "��������������" ��������� ��������� �� N � �������� O(N),
--   �� ��������� �� ���� ��� ������ N/2 ��������.
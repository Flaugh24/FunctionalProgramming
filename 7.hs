data Deque a = Deque [a] [a] deriving (Show)

-- Ќачало первого списка - левый конец очереди.
--  онец первого списка ~ середина очереди слева.
-- Ќачало второго списка - правый конец очереди.
--  онец второго списка ~ середина очереди справа.

-- “аким образом append и appendLeft всегда O(1), т.к. (:) - O(1).
-- pop и popLeft почти всегда O(1) по тем же причинам, кроме случа€,
-- когда один из списков опустеет, а в другом будет больше одного элемента.

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
-- —лучаи, когда правый список опустеет
pop (Deque (h:[]) []) = (Deque [] [], h)
pop deque@(Deque in_ []) = pop $ equalize deque

popLeft :: Deque a -> (Deque a, a)
popLeft (Deque [] []) = error "Deque is empty"
popLeft (Deque (h:t) out) = (Deque t out, h)
-- —лучаи, когда левый список опустеет
popLeft (Deque [] (h:[])) = (Deque [] [], h)
popLeft deque@(Deque [] out) = popLeft $ equalize deque

-- ћетод банкира:
--   «арабатываем 1$ за каждое добавление.
--   ѕри удалении из деки может возникнуть случай, когда нужный список опустел.
--   “огда нужно производить "перекидывание".
--   ѕерекидываем в этом случае N/2, где N мы уже заработали на добавлени€х.
--   —оответственно, амортизированна€ сложность всех операций над декой - константа.
--
-- ћетод физика:
--   ѕотенциал - сумма длин списков.
--   ƒобавление увеличивает потенциал на 1.
--   ”даление без "перекидывани€" не мен€ет потенциал.
--   ”даление с "перекидыванием" уменьшает потенциал на N и занимает O(N),
--   но случаетс€ не чаще чем каждую N/2 операцию.
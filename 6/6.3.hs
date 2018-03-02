import Data.Stream as S
import Data.List as L

factorials = 1 : L.zipWith (*) factorials [1..]


ePrecisions :: Stream Rational
ePrecisions = S.zipWith (\ a b -> foldr (+) 0 $ L.take a b) (S.iterate (1+) 1) (S.repeat $ L.map (1/) factorials) 
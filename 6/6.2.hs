import Data.Stream as S
import Data.List as L

--sinPrecisions :: Double -> Stream Double
factorials = 1 : L.zipWith (*) factorials (L.map toEnum $ L.filter odd [1..])
taylor x = L.zipWith3 (\ a b c -> a * b / c)
         (L.zipWith (^) (L.repeat x) (L.filter odd [1..]))
         (L.zipWith (^) (L.repeat (-1)) [0..])
         factorials

sinPrecisions :: Double -> Stream Double
sinPrecisions x = S.zipWith (\ a b -> foldr (+) 0 $ L.take a b) (S.iterate (1+) 1) (S.repeat $ taylor x) 
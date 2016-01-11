module Homework1.Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 _ _ _ = []
hanoi n a b c =
  hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- Frame-Stewart algorithm
hanoiR :: Integer -> [Peg] -> [Move]
hanoiR 0 _ = []
hanoiR 1 (p1 : p2 : _) = [(p1, p2)]
hanoiR n [p1, p2, p3] =
    hanoiR (n - 1) [p1, p3, p2] ++
    [(p1, p2)] ++
    hanoiR (n - 1) [p3, p2, p1]

hanoiR n (p1 : p2 : p3 : rest) =
  hanoiR k (p1 : p3 : p2 : rest) ++
  hanoiR (n - k) (p1 : p2 : rest) ++
  hanoiR k (p3 : p2 : p1 : rest)
  where k = n `quot` 2

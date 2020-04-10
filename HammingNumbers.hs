merge :: [Integer] -> [Integer] -> [Integer]
merge xxs@(x:xs) yys@(y:ys) | x ==y =x: merge xs ys
    | x < y  =x: merge xs yys
    | x > y  =y: merge xxs ys

hamming :: [Integer]
hamming =1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming) )


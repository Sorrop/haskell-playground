solveQdr:: (Float, Float, Float) -> (Float, Float)
solveQdr = \ (a,b,c) -> if a == 0 then error "not quadratic"
                        else let d = b^2 - 4*a*c in
                        if d < 0 then error "no real solutions"
                        else ((-b + sqrt d) / 2*a, (-b - sqrt d) / 2*a)

-- obtain all the even integers up to a specified one
evens:: Int -> [Int]          
evens = \ x -> [y | y <- [1..x], mod y 2 == 0]


-- Check if an integer is even
is_even:: Int -> Bool
is_even = \ x -> if mod x 2 == 0 then True
                 else False

-- Count occurrences of True in a list of booleans
count_trues:: [Bool] -> Int
count_trues [] = 0
count_trues (x:xs) = if x == True then 1 + count_trues(xs)
                     else count_trues(xs) 

-- check if a property holds for exactly one element of a list
unique:: (a -> Bool) -> [a] -> Bool
unique p xs = let l = count_trues (map p xs) in
              if (l == 1) then True
              else False

-- check if list of booleans contain an even number of Trues
parity:: [Bool] -> Bool
parity xs = let n = (count_trues xs) in
            if (mod n 2 == 0) then True
            else False

-- check if a property holds for an even number of list's elements
evenNR:: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
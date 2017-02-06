
divides:: Integer -> (Integer -> Bool) --  divides takes an argument of type Integer 
                                       --  and produces a result of type (Integer -> Bool)
                                       --  which is procedure that takes as argument an integer
                                       --  and produces a result of type Bool
divides d n = rem n d == 0  
                            

ldf:: Integer -> (Integer -> Integer)

ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ld:: Integer -> Integer
ld n = ldf 2 n

primes0:: Integer -> Bool
primes0 n | n < 1     = error "must be n > 1"
          | n == 1    = False
          | otherwise = ld n == n


prime_factorization:: Integer -> [Integer]
prime_factorization n | n == 1    = []
                      | otherwise = [x] ++ prime_factorization (div n x)
                                            where x = ld n


prime_factorization':: Integer -> [Integer]
prime_factorization' n | n == 1    = []
                      | otherwise = x : prime_factorization (div n x)
                                            where x = ld n


lengths:: [[a]] -> [Int]     -- Gives a list of lengths of each list of a list of lists (mouahaha)
lengths (x:xs) = map length (x:xs)


ldpf:: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

ldp:: Integer -> Integer
ldp n = ldpf primes1 n


primes1:: [Integer]
primes1 = 2 : filter prime [3..]

prime:: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n
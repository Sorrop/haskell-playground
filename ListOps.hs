
mnmInt:: [Int] -> Int             -- Get min element of list
mnmInt [] = error "Empty list."
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)




mxmInt:: [Int] -> Int             -- Get max element of list
mxmInt [] = error "Empty List."
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)




rmvFst:: [Int] -> Int -> [Int]    -- Remove first occurence of element d from list
rmvFst [] d = error "Empty List."

rmvFst [x] d | x == d    = []
             | otherwise = [x]

rmvFst (x:xs) d | x == d     = xs
                | otherwise  = (x:rmvFst xs d)




sortInt:: [Int] -> [Int]          -- sorts a list  
sortInt [] = []
sortInt xs = m : sortInt (rmvFst xs m) where m = mnmInt xs




average:: [Int] -> Float          -- Get average of a list
average [] = error "Empty List."
average xs = fromIntegral (sum xs) / fromIntegral (length xs)




char_occ:: String -> Char -> Int  -- Count occurences of char c in a string 
char_occ [] c = 0
char_occ (c:cs) x | x == c     = 1 + (char_occ cs x)
                  | otherwise  = char_occ cs x




copy:: Int -> Char -> String      -- Copy a character n times to produce a string
copy 0 c = []
copy n c = c: (copy (n-1) c) 

blowup_aux:: String -> Int -> String   
blowup_aux [] n = []
blowup_aux (x:xs) n = copy n x ++ ( blowup_aux xs (n+1) )

blowup:: String -> String         -- From abcd produce abbcccdddd
blowup xs = blowup_aux xs 1




get_index_aux:: String -> Char -> Int
get_index_aux [] x = 2
get_index_aux (c:cs) x | c == x     = 1
                       | otherwise  = 1 + get_index_aux cs x

get_index:: String -> Char -> Int -- Return the index of a character in string
get_index s c | (get_index_aux s c) > length s  = -1
              | otherwise                       = get_index_aux s c


prefix:: String -> String -> Bool -- Check if str1 is prefix of str2
prefix [] xs = True
prefix xs [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys


substring:: String -> String -> Bool
substring [] xs = True
substring xs [] = False
substring (x:xs) (y:ys) | prefix (x:xs) (y:ys) = True
                        | substring (x:xs) ys  = True
                        | otherwise            = False


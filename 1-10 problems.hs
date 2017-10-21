{-
	Volodymyr Hryhoriev
	MF-42
	The problems are taken from https://wiki.haskell.org/99_questions/1_to_10
-}

-- 1
myLast :: [a] -> a
myLast [] = error "The list is empty"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast xs 
  | length xs < 2 = error "The list is too small" -- Can use 'length' function, because I've written the same function below - 'myLength'.
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "The list is empty"
elementAt _ x 
  | x <= 0 = error "Index should be greater than 0"
elementAt (x:xs) 1 = x
elementAt (_:xs) n = elementAt xs (n - 1) 

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse(xs) ++ [x]

--6
isPalindrome ::  (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == reverse xs -- can use 'reverse' function, because I've written the same function above - 'myReverse'.

--7
data NestedList a = Elem a | List [NestedList a] deriving(Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs) 

--8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (f:s:xs)
  | f == s = compressed_tail
  | otherwise = f:compressed_tail
  where
     compressed_tail = compress (s:xs)

--9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (f:s:xs)
  | f == s = (f:pf):pt -- add head to the 1st element (the 1st list) of the 'packed_tail' list
  | otherwise = [f]:packed_tail 
  where
     (pf:pt) = pack (s:xs) -- pattern matching 'packed_tail'
     packed_tail = (pf:pt)

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode list = 
  let packed_list = pack list
  in  calculate packed_list
     where
        calculate :: (Eq a) => [[a]] -> [(Int, a)] -- transform 'packed_list' into form [(a, b)]
        calculate [[]] = []
        calculate [f:xs] = [(length xs + 1, f)]
        calculate ((f:x):xs) = (length x + 1, f):calculate xs
  

{-
	Volodymyr Hryhoriev
	MF-42
	The problems are taken from https://wiki.haskell.org/99_questions/1_to_10
-}

-- 1
myLast :: [a] -> a
myLast [] = error "The list is empty" -- Если список пуст - выкидывает ошибку. 
myLast ([x]) = x -- Если список содержит только один элемент - возвращаем этот элемент. (УЗР- условие завершения рекурсии).
myLast (_:xs) = myLast xs -- Иначе применяем ту же функцию к хвосту списка.

-- 2
myButLast :: [a] -> a
myButLast xs  -- Если в списке меньше двух элементов - выкидываем ошибку
  | length xs < 2 = error "The list is too small" -- Can use 'length' function, because I've written the same function below - 'myLength'.
myButLast [x,_] = x -- Если список состоит из двух элементов - возвращаем первый (УЗР).
myButLast (_:xs) = myButLast xs -- Иначе применяем ту же функцию к хвосту списка

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "The list is empty" -- Если список пуст - выкидываем ошибку
elementAt _ x 
  | x <= 0 = error "Index should be greater than 0" -- Если индекс меньше или равен 0 - выкидываем ошибку
elementAt (x:xs) 1 = x -- Если индекс равен 1 - возвращаем голову списка (УЗР).
elementAt (_:xs) n = elementAt xs (n - 1) -- Иначе применяем ту же функцию к хвосту списка, дикрементим индекс.

-- 4
myLength :: [a] -> Int
myLength [] = 0 -- Если список пуст - возвращем 0 (УЗР)
myLength (_:xs) = 1 + myLength xs -- Иначе применяем ту же функцию к хвосту и прибавляем 1

-- 5
myReverse :: [a] -> [a]
myReverse [] = [] -- Если список пуст - возращаем пустой список (УЗР).
myReverse [x] = [x] -- Если список состоит из 1 элемента - возвращаем его (УЗР).
myReverse (x:xs) = myReverse(xs) ++ [x] -- Иначе вызываем ту же функции к хвосту, результат вызова конкатанируем с головой.

--6
isPalindrome ::  (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs -- can use 'reverse' function, because I've written the same function above - 'myReverse'.

--7
data NestedList a = Elem a | List [NestedList a] deriving(Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x] -- Если список состоит из 1 элемента - возращаем список со значеним этого элемента (УЗР).
flatten (List []) = [] -- Если список состоит из пустого листа - возвращаем пустой спсиок (УЗР).
flatten (List (x:xs)) = flatten x ++ flatten (List xs) -- Иначе применяем отдельно ту же функцию для головы списка и
                                                       -- для хвоста, результаты конкатанируем.

--8
compress :: (Eq a) => [a] -> [a]
compress [] = [] -- Если список пуст - возвращем его (УЗР).
compress [x] = [x] -- Если список состоит из 1 элемента - возвращаем его (УЗР).
compress (f:s:xs) -- Разбиваем список на голову, второй элемент и хвост
  | f == s = compressed_tail -- Если голова и второй элемент равны - "сжатый хвост"
  | otherwise = f:compressed_tail -- Иначе прибавляем к "сжатому хвосту" голову и возвращаем.
                                  -- NOTE: под прибавление подразумевается вставка в вначало списка.
  where
     compressed_tail = compress (s:xs) -- "Сжатый хвост" - это результата применения фунеции ко второму элементу + хвосту.

--9
pack :: (Eq a) => [a] -> [[a]]
pack [] = [] -- Если список пуст - возвращаем его (УЗР).
pack [x] = [[x]] -- Если список состоит из 1 элемент, возвращаем список, который содержит этот список (УЗР).
pack (f:s:xs) -- Разбиваем список на голову, второй элемент и хвост.
  | f == s = (f:pf):pt -- Прибавляем первый элемент к первому списку "запаковаго хвоста".
  | otherwise = [f]:packed_tail -- Иначе вставляем в начало список состоящий из первого элемента к "запакованому хвосту""
  where
     (pf:pt) = pack (s:xs) -- пакуем список состоящий из вторго элемента и хвоста
     packed_tail = (pf:pt) -- "запакованый хвост"

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = [] -- Если список пуст - возвращаем его (УЗР).
encode list = 
  let packed_list = pack list -- применяем функции упаковки к списку, сохраняем  переменную
  in  calculate packed_list -- применяем к упакованому списку фукнцию подсчета повторений.
     where -- Определяем функцию, которая подсчитывает кол-во повторений в каждом списке
        calculate :: (Eq a) => [[a]] -> [(Int, a)] -- transform 'packed_list' into form [(a, b)]
        calculate [] = [] -- Если список содержит пустой список - возвращаем пустой список (УЗР).
        calculate ((f:x):xs) = (length x + 1, f):calculate xs -- *** ((f:x):xs) - расшифровуется как:
                                                -- list :: [[a]] = (x:xs) - список списков
                                                -- nested_list :: [a] = (f:x) - первый элемент list
                                                -- element :: a = f - первый элемент nested_list.
                                                -- *** Иначе подсчитывает кол-во совпадений в первом списке, пакуем это в кортеж.
                                                -- Прибавляем это к результату приме той же функцию к хвосту.
  

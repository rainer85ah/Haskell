
module Lista where

data Lista a = L [a]
data List a = LV | LL [a] deriving (Show)

isEmpty :: [a] -> Bool
isEmpty lista = (length lista) == 0

delete :: (Eq a) => a -> [a] -> [a]
delete e [] = []
delete e lista = [x | x <- lista, x/=e]

elemIndex :: (Eq a) => a -> [a] -> Maybe Integer
elemIndex = elemIndexAux 0

elemIndexAux :: (Eq a) => Integer -> a -> [a] -> Maybe Integer
elemIndexAux _ e [] = Nothing
elemIndexAux i e (x:xs) = if (e==x) then Just i else elemIndexAux (i+1) e xs

elemIndices :: (Eq a) => a -> [a] -> [Integer]
elemIndices = elemIndicesAux [] 0

elemIndicesAux :: (Eq a) => [Integer] -> Integer -> a -> [a] -> [Integer]
elemIndicesAux sol i e [] = sol
elemIndicesAux sol i e (x:xs) = if (e==x) then elemIndicesAux (sol++[i]) (i+1) e xs else elemIndicesAux sol (i+1) e xs

insert :: (Ord a) => a -> [a] -> [a]
insert e [] = [e]
insert e (x:[]) = if (e>=x) then (x:e:[]) else (e:x:[])
insert e (x:y:xs) = if (e<=x) then (e:x:y:xs) else 
													if (e>x && e<=y) then (x:e:y:xs) else (x:y:e:xs)   

union :: [a] -> [a] -> [a]
union l1 l2 = l1++l2




flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

map2 :: (a -> b) -> [a] -> [b]
map2 f lista = [f x | x <- lista]



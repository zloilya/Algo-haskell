import Data.Text (pack, unpack, splitOn)


data Tree a = Leaf a | Node (Tree a) a (Tree a) | Nil deriving (Show)

fromBooltoString :: Bool -> String
fromBooltoString True = "true"
fromBooltoString False = "false"

split :: String -> String -> [String]
split what xs = Prelude.map unpack (splitOn (pack what) (pack xs))

insert :: Int -> Tree Int -> Tree Int
insert x Nil = Leaf x
insert x t@(Leaf y)
  | y == x = t
  | x < y = Node (Leaf x) y Nil
  | otherwise = Node (Leaf y) x Nil
insert val t@(Node x y z)
  | y == val = t
  | y < val = Node x y (insert val z)
  | otherwise = Node (insert val x) y z
delete x Nil = Nil
delete x t@(Leaf y)
  |  y == x = Nil
  | otherwise = t
delete x (Node l y r)
    | x < y     = Node (delete x l) y r
    | x > y     = Node l y (delete x r)
    | otherwise = keep l r
    where  keep :: Tree Int -> Tree Int -> Tree Int
           keep Nil r = r
           keep l Nil = l
           keep l r = Node l' y' r
               where y' = find l
                     l' = delete y' l
                     find :: Tree Int -> Int
                     find (Node _ rm Nil) = rm
                     find (Node _  _ r)   = find r
exists x Nil = False
exists x (Leaf y)
   | x == y = True
   | otherwise = False
exists x (Node l y r)
   | x < y = exists x l
   | x > y = exists x r
   | otherwise = True

isNothing :: Maybe Int -> Bool
isNothing Nothing = True
isNothing _ = False

prev :: Int -> Tree Int -> Maybe Int
prev x Nil = Nothing
prev x (Leaf y) = if x > y then Just y else Nothing
prev x (Node l y r) =
  if x > y
    then if isNothing (prev x r)
           then Just y
           else prev x r
    else prev x l
next :: Int -> Tree Int -> Maybe Int
next x Nil = Nothing
next x (Leaf y) = if x < y then Just y else Nothing
next x (Node l y r) =
  if x < y
    then if isNothing (next x l)
           then Just y
           else next x l
    else next x r

fromMaybeInttoString :: Maybe Int -> String
fromMaybeInttoString (Just x) = show x
fromMaybeInttoString Nothing  = "none"

isName :: String -> (String, Int)
isName xs = (head $ split " " xs, read $ head $ tail $ split " " xs)

helper :: Tree Int -> [String] -> [String]
helper tree [] = []
helper tree (x:xs) =
  case isName x of
    ("insert", val) -> helper (insert val tree) xs
    ("delete", val) -> helper (delete val tree) xs
    ("exists", val) -> fromBooltoString   (exists val tree) : helper tree xs
    ("next",   val) -> fromMaybeInttoString (next val tree) : helper tree xs
    ("prev",   val) -> fromMaybeInttoString (prev val tree) : helper tree xs
    _               -> helper tree xs

glue :: [String] -> String
glue = foldr (\x -> (++) (x ++ "\n")) []
main = do
  s <- getContents
  putStr $ glue $ helper Nil (split "\n" s)











  
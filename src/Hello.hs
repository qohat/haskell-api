module Hello where

type Name = String
newtype City = CityName String deriving Show

data Person = Person Name City
instance Show Person where
  show (Person n c) = "Person: {name: " ++ n ++ ", city: " ++ show c ++ "}" -- adHoc Polimorphism

data Country = Country { name :: String, code :: Int }
data Json = JsonNull | JsonInt { valueInt :: Int } | JsonString { valueStr :: String }
makeJson :: Int -> Json
makeJson = JsonInt -- makeJson i = JsonInt i

makePerson :: Name -> City -> Person
makePerson = Person

makeName :: String -> Name
makeName n = n

compose :: City -> Person
compose = makePerson . makeName $ "Name" -- it could be (makePerson . makeName) "Name"

newPerson :: Person
newPerson = compose $ CityName "City"

getString :: Json -> String
getString (JsonString v) = v
getString (JsonInt _) = "Int"
getString _ = "Null"

getStringTwo :: Json -> String
getStringTwo json =
  case json of
    JsonString v -> v
    JsonInt _ -> "Int"
    _ -> "Null"

getList :: [Int]
getList = [1, 2, 3, 4, 5]

mapList :: [Int] -> [Int]
mapList = map (* 2)

flatMapList :: [Int] -> [Int]
flatMapList list = list >>= (\v -> [v * 4])

foldList :: [Int] -> Int
foldList = foldl (\a b -> a * 3 + b) 0 -- (\a b -> a + b) -> (+) 

finalFunction :: [Int] -> Int
finalFunction = foldList . flatMapList . mapList









     
    


import Data.List
main :: IO()
main = do
    print (filterTypical  ["Mallard", "Hook Bill", "African", "Crested","Pilgrim", "Toulouse", "Blue Swedish"])
    print (filterTypical ["Mallard", "Barbary", "Hook Bill", "Blue Swedish","Crested"])
    print (filterTypical ["African", "Roman Tufted", "Toulouse", "Pilgrim","Steinbacher"])

typical :: [String]
typical = ["African", "Roman Tufted", "Toulouse", "Pilgrim","Steinbacher"]

filterTypical :: [String] -> [String]
filterTypical = filter (\x -> not (x`elem`typical))


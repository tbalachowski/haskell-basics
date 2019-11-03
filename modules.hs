import qualified Data.Map as Map

phoneBook = 
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "49*3-2928")
    ,("lucile", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]
    
findByKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findByKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty
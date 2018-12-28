slice :: Int -> Int -> [list] -> [list]
slice i j lt = take ((j-i)+1) $ drop (i-1) lt


delete :: Int -> [a] -> [a] 
delete k lt
	| length lt < k = lt
	| otherwise     = take  (k-1) lt ++ delete k (drop k lt) 


sortlist :: [[Int]] -> [[Int]]
sortlist [] = []
sortlist (x:xs)= sortlist [y| y<-xs, length y<=length x ] ++ [x] ++ sortlist [y| y<- xs, length y >length x ]


flatten :: (Ord a) => [[a]] -> [a]
flatten [] = []
flatten (x:lt) = x ++ flatten lt



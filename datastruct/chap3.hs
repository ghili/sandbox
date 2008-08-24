

data Heap a = E | H Int a (Heap a) (Heap a)
              deriving Show

merge h E = h
merge E h = h
merge h1@(H _ x a1 b1) h2@(H _ y a2 b2) = if x < y then makeT x a1 (merge b1 h2) 
                                          else makeT y a2 (merge h1 b2)

rank E =0 
rank (H x _ _ _) = x

makeT x a b = if rank a >= rank b then H (rank b + 1) x a b
              else H (rank a+1) x b a

insert x h = merge h (H 1 x E E)

--exercice 3.2
insert2 x E = H 1 x E E
insert2 x h@(H _ y a2 b2) = if x < y then makeT x E h
                                          else makeT y a2 (insert2 x b2)

testinsert = insert 1 (insert 4 (insert 3 (insert 2 E)))

testinsert2 = insert2 1 (insert2 4 (insert2 3 (insert2 2 E)))

--exercice 3.3
fromList [] (h1:(h2:xs))= fromList [] ((merge h1 h2):(fromList [] xs))
fromList [] (h:[])= [h]
fromList [] []= []
fromList (x:xs) s = fromList xs ((insert x E):s)


testFromList = fromList [1..5] []

--exercice 3.4
data WHeap a = WE | WH Int a (WHeap a) (WHeap a)
               deriving Show

mergeW h WE = h
mergeW WE h = h
mergeW h1@(WH _ x a1 b1) h2@(WH _ y a2 b2) = if x < y then makeWT x a1 (mergeW b1 h2) 
                                          else makeWT y a2 (mergeW h1 b2)

rankW WE =0 
rankW (WH x _ _ _) = x

makeWT x a b = if rankW a >= rankW b then WH (rankW b + rankW a + 1) x a b
              else WH (rankW a + rankW b +1) x b a


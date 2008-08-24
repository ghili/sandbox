--exercice 2.1
suffixes s@(x:xs)= s:(suffixes xs)
suffixes []=[]

data Tree a = E | T (Tree a) a (Tree a)

--exercice 2.2
membera x z E = if x==z then True else False
membera x z (T a y b)= if x<= y then membera x y a else membera x z b

member x E = False
member x (T a y b)= membera x y (T a y b)

testmember = member 2 (T (T E 1 E) 3 E)


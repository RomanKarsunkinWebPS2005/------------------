import Modulemy

ex_1  = do
   print(ex1 [1,3,5,4,5,7,6,8,10,1])
   print(ex1 [1,5,3,7])
   print(ex1 [5,4,3,2,1,0])
   print(ex1 [])
ex_2  = do 
    print(ex2 700)

ex_3  = do 
    print(ex3 [10,15,3,8,21] [2,4,3,5,7])
    print(ex3 [2,4,6] [8,4,12])
    print(ex3 [2,4,6] [4,3,12])
    print(ex3 [1,2,3,4,5,6,7,8] [])
ex_33  = do 
    print(ex3 [10,15,3,8,21] [2,4,0,5,7])
ex_4  = do     
      ex4 "input.txt" "out.txt"
      contents <- readFile "out.txt"
      putStr contents

    
main = do

    startIO <- getLine
    case startIO of 
        "ex1" -> ex_1 
        "ex2" -> ex_2
        "ex3" -> ex_3
        "ex33" -> ex_33
        "ex4" -> ex_4
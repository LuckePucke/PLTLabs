double x = x + x ;
twice f x = f (f x) ;
quadruple = twice double ;
mainz = twice quadruple 2 ;
maint = twice (\x -> x + double x) 6 ;

grow x = 1 + grow x ;

first x y = x + 1 ;

main = print (first 4 (grow 4)) ; -- result 5 with -n, loop otherwise

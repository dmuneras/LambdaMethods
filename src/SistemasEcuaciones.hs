module SistemasEcuaciones where
import GramaticaAbstracta
import Data.Array

{-FUNCIONES PARA LECTURA DE MATRICES-}

leerMatriz :: Integer -> [Double] -> Matriz
leerMatriz n elem = listArray ((1,1),(n,n)) elem

leerMatrizAu :: Integer -> [Double] -> Matriz
leerMatrizAu n elem = listArray ((1,1),(n+1,n)) elem


{-METODOS AUXILIARES -}

{-tomado de http://ronnyml.wordpress.com/category/haskell/-}

quickSort::Ord a=>[a]->[a]
quickSort [] = []
quickSort (x:xs) = quickSort(menores) ++ [x] ++ quickSort(mayores)
   where
      menores = [y | y <-xs, y < x]
      mayores = [z | z <-xs, z >= x] 


{- Funcion que retorna el valor del mayor -}

mayor ::Matriz -> Double
mayor m = last(quickSort (elems m))

{-Funcion que retorna una columna de la c matriz a, necesita que ingresen la matriz, la columna deseada y el numero de filas total de la fila-}
darColumna :: Matriz -> Integer -> Matriz
darColumna m c = listArray ((1,c),((fst(last(indices m)) ),c))(map (\y -> snd y)(filter (\x -> (snd (fst x)) == c ) (assocs m)))

{-Funcion que busca el valor mayor de una Matriz A-}

buscarMayorTotal :: Matriz -> ((Integer,Integer), Double)
buscarMayorTotal m = head(filter (\x -> (snd x == (mayor m))) (assocs m))

buscarMayorParcial :: Matriz -> Integer -> ((Integer,Integer),Double)
buscarMayorParcial m  c = head (filter (\x -> (snd x == (mayor (darColumna m c )))) (assocs (darColumna m c )))

{-Tomado de la pagina => http://www.lcc.uma.es/~blas/pfHaskell/gentle/arrays.html, utiliza constructores de listas-}

{-La funcion cambio de fila fue tomada del manual "una introduccion agradable a haskell", su funcionamiento se basa en el uso de la funcion //, la cual es una funcion de actualizacion para array, aclaramos que al ser una funcion que no utiliza monadas oviamente no modifica la matriz original, simplemente construye una matriz diferente-}

cambioFilas :: (Ix a, Ix b, Enum b) => Array (a,b) c -> a -> a -> Array (a,b) c
cambioFilas a i i'=  a // [assoc | j <- [jLo..jHi], assoc <- [((i ,j), a!(i',j)), ((i',j), a!(i, j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a

{-La funcion cambio de columna es casi identica a la anterior su unico cambio es que actualizamos los indices de la columna-}
cambioColumnas :: (Ix a, Ix b, Enum b) =>  Array (b,a) c -> a -> a -> Array (b,a) c
cambioColumnas a j j'  = a // [assoc | i <- [iLo..iHi], assoc <- [((i,j), a!(i,j')) , ((i,j'), a!(i,j))] ]
                   where ((iLo,jLo),(iHi,jHi)) = bounds a

{-PARA PRUEBAS-}
matriz1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]


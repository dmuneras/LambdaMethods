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
darColumn :: Matriz -> Integer -> Matriz
darColumn m c = listArray ((1,c),((fst(last(indices m)) ),c))(map (\y -> snd y)(filter (\x -> (snd (fst x)) == c ) (assocs m)))

{-Funcion que busca el valor mayor de una Matriz A-}

buscarMayorTotal :: Matriz -> ((Integer,Integer), Double)
buscarMayorTotal m = head(filter (\x -> (snd x == (mayor m))) (assocs m))

buscarMayorParcial :: Matriz -> Integer -> ((Integer,Integer),Double)
buscarMayorParcial m  c = head (filter (\x -> (snd x == (mayor (darColumn m c )))) (assocs (darColumn m c )))

{-PARA PRUEBAS-}
matriz1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]


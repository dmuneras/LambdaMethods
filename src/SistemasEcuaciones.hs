module SistemasEcuaciones where
import GramaticaAbstracta
import Data.Array
import FuncionesAuxiliaresSE


{- ELIMINACION GAUSIANA-}

eliminacionGaussiana :: Matriz -> Integer ->Matriz 
eliminacionGaussiana a n = etapak a n (n-1)

etapak :: Matriz -> Integer -> Integer -> Matriz
etapak a n 1 = actualizar a (indOper a (etapaj (operm a 1) (n-1))1)
etapak a n k = actualizar (etapak a n (k-1)) (indOper (etapak a n (k-1)) (etapaj (operm (etapak a n (k-1)) k) (n-k) )k )

{-Funcion que se encarga de las transformaciones de fila en cada etapa k, utiliza la funcion etapaj' que a su vez utiliza la funcion etapaj''-}

etapaj :: Matriz -> Integer -> Matriz 
etapaj a j = listArray ((1,1),(j,(j+2))) (map (\x -> snd x) (etapaj' a j))

etapaj' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj' a 1  = etapaj'' a 1 
etapaj' a j = (etapaj' a (j-1)) ++ (etapaj'' a j)

etapaj'' :: Matriz -> Integer -> [((Integer,Integer),Double)]
etapaj'' a j = (map (\x -> (((fst x), ((snd x) - (f x * mk))))) (darFila a (j+1)))
    where mk = multFila a 1 (j+1)
          f x = ( snd (head (filter (\y -> snd (fst(y)) == snd (fst x)) (operFila a 1))))

{-FUNCIONES DE ACTUALIZACION DE MATRIZ-}

{-Funcion que devuelve la submatriz que sera procesada dependiendo de la etapa k en la que va el proceso-}
operm :: Matriz -> Integer -> Matriz
operm a k 
      | (snd(snd(bounds a)) == fst (snd(bounds a))) = leerMatriz (n-(k-1)) ar
      | otherwise = leerMatrizAu (n-k) ar
            where n = (snd(snd(bounds a)))
                  ar =  (map (\x -> snd x)(filter (\x -> (snd(fst(x)) >= k && (fst(fst(x))) >= k)) (assocs a)))

{-Funcion que da a cada nuevo valor su respectivo indice para ser ubicado en la matriz -}
indOper :: Matriz -> Matriz -> Integer -> Matriz
indOper a om k 
        |(snd(snd(bounds a)) == fst (snd(bounds a))) =  listArray (((k+1),k), (n,n)) (elems om)
        | otherwise =  listArray (((k+1),k), ((n-1),n)) (elems om)
                 where n = snd(snd(bounds a))

{-Funcion que actualiza la matriz  con sus nuevos valores -}
actualizar :: Matriz -> Matriz -> Matriz
actualizar a e 
           | (snd(snd(bounds a)) == fst (snd(bounds a))) = leerMatriz n ar
           | otherwise =  leerMatrizAu (n-1) ar
                where n = snd(snd(bounds a))
                      ar =  (map (\x -> snd x)(map (\x -> (act' x (assocs e)) ) (assocs a)))

{-Funcion auxiliar que se encarga de sustituir un valor viejo por uno nuevo en la matriz  -}
act' :: ((Integer,Integer),Double) -> [((Integer,Integer),Double)] -> ((Integer,Integer),Double) 
act' b e 
     | (filter (\x -> (fst x) == (fst b))e) /= [] = head(filter (\x -> (fst x) == (fst b))e)
     | otherwise = b


{-SUSTITUCIONES-}

xn :: Matriz -> Integer -> Integer ->  Double
xn a n k  = a!(n,(n+1)) /a!(n,n)
    where m = eliminacionGaussiana a n
 

{-PARA PRUEBAS-}
m1 = leerMatriz 3 [2,1,4,3,2,3,6,1,4]
m2 = leerMatriz 4 [20,1,3,2,4,60,-3,-7,1,2,50,7,-2,-7,4,18]
m3 = leerMatriz 3 [10,2,5,3,12,2,-4,-5,15]
m4 = leerMatriz 4 [2,-3,10,-7,3,12,-16,16,14,-18,40,-7,16,-8,-50,6]
m4au =  leerMatrizAu 4 [2,-3,10,-7,20,   3,12,-16,16,30  ,14,-18,40,-7, 25 ,16,-8,-50,6,-18]
m5 = leerMatriz 4 [-7,2,-3,4,5,-1,14,-1,1,9,-7,5,-12,13,-8,-4]
m2au =  leerMatrizAu 4 [20,1,3,2,1,  4,60,-3,-7,1,  1,2,50,7,1, -2,-7,4,18,1]




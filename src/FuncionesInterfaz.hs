module FuncionesInterfaz where

import Semantica
import GramaticaConcreta
import GramaticaAbstracta
import EcuacionesNoLineales
import SistemasEcuaciones
import FuncionesAuxiliaresSE
import UU.Parsing
import Graphics.UI.Gtk
import Prelude
import Control.Exception
import Foreign
import Data.Array
import Data.List

process ::(EntryClass e) => String -> String -> String -> String -> String -> String -> e -> IO ()
process f a b tol i p e  
              | (p == "ver ecuacion parser ") = do a <- parseIO pFunc (funScanTxt f)
                                                   let st = show a
                                                   set e [ entryText := show st ]
              | (p == "Busqueda incremental ") = do f <- parseIO pFunc (funScanTxt f)
                                                    b <- parseIO pFunc (funScanTxt b)
                                                    a <-parseIO pFunc (funScanTxt a)
                                                    putStrLn ((show a)++ " incrementando " ++ (show b) ++ " iter " ++ (show i))
                                                    let st = busqdIncremental f a b (read i)
                                                    set e [entryText := show st]
              | (p == "Biseccion ") = do f <- parseIO pFunc (funScanTxt f)
                                         b <- parseIO pFunc (funScanTxt b)
                                         a <-parseIO pFunc (funScanTxt a)
                                         tol <- parseIO pFunc (funScanTxt tol)
                                         putStrLn ("Biseccion "++"Intervalo "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                         let st = biseccion f a b tol (read i) "abs"
                                         set e [entryText := st]
             | (p == "Regla falsa ") = do f <- parseIO pFunc (funScanTxt f)
                                          b <- parseIO pFunc (funScanTxt b)
                                          a <-parseIO pFunc (funScanTxt a)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("Regla falsa "++"Intervalo "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                          let st = reglaFalsa f a b tol (read i) "abs"
                                          set e [entryText := st]
              | (p == "Punto Fijo ") = do f <- parseIO pFunc (funScanTxt f)
                                          g <- parseIO pFunc (funScanTxt a)
                                          a <-parseIO pFunc (funScanTxt b)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("puntof"++"Inicial "++(show a) ++ " g" ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                          let st = puntoFijo f a g tol (read i) "abs"
                                          set e [entryText := st]
              | (p == "Newton ") =     do f <- parseIO pFunc (funScanTxt f)
                                          g <- parseIO pFunc (funScanTxt a)
                                          a <-parseIO pFunc (funScanTxt b)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("Newton"++"Inicial "++(show a) ++ " f'" ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i))
                                          let st = newton f a g tol (read i) "abs"
                                          set e [entryText := st]
              | (p == "Secante ") =    do f <- parseIO pFunc (funScanTxt f)
                                          b <- parseIO pFunc (funScanTxt b)
                                          a <-parseIO pFunc (funScanTxt a)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("Secante "++"Iniciales "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i)) 
                                          let st = secante f a b tol (read i) "abs"
                                          set e [entryText := st]
              | (p == "raices ") =     do f <- parseIO pFunc (funScanTxt f)
                                          a <-parseIO pFunc (funScanTxt a)
                                          tol <- parseIO pFunc (funScanTxt tol)
                                          putStrLn ("RaicesMult"++"Inicial "++ (show a) ++ "," ++ (show b) ++ " " ++ (show tol) ++ " "++ (show i)) 
                                          let st = raicesMult f a tol (read i) "abs"
                                          set e [entryText := st] 
              | otherwise = set e [entryText := "todavia no"]


{- Funcion que me retorna el texto utilizado como control en el process-}
control :: RadioButton -> String
control b =  unsafePerformIO(get b buttonLabel)

{-Funcion que me retorna el radiobutton activo-}
controlRadio :: [RadioButton] -> RadioButton
controlRadio ar = head (filter (\x -> unsafePerformIO(toggleButtonGetActive x)) ar)   
  

{-Funcion que va indicando en la consola el cambio de estado de los radiobutton -}
setRadioState :: RadioButton -> IO ()
setRadioState b = do
  state <- toggleButtonGetActive b
  label <- get b buttonLabel
  putStrLn ("State " ++ label ++ " now is " ++ (show state)) 

ayudaGen :: IO () 
ayudaGen = do
  initGUI
  windowAyuda <- windowNew
  set windowAyuda [windowTitle := "Ayuda General", windowDefaultWidth := 600,
                   windowDefaultHeight := 600 ]
  windowSetIconFromFile windowAyuda "lambda"
  contentPrincipal <- vBoxNew True 10
  containerAdd windowAyuda contentPrincipal
      
  contentAyuda <- vBoxNew False 0
  boxPackStart contentPrincipal contentAyuda PackNatural 0     
  containerSetBorderWidth contentAyuda 10
     
  ayuda <- labelNew (Just "Lo sentimos, en el momento no tenemos idea de como abrir PDF desde haskell.") 
  containerAdd contentAyuda ayuda
     
  widgetShowAll windowAyuda
  onDestroy windowAyuda mainQuit
  mainGUI
  

ayudaENL :: IO()
ayudaENL =  do
     initGUI
     windowAyuda <- windowNew
     set windowAyuda [windowTitle := "Ayudas Ecuaciones no lineales", windowDefaultWidth := 400,
                 windowDefaultHeight := 400 ]
     windowSetIconFromFile windowAyuda "lambda"
     contentPrincipal <- vBoxNew True 10
     containerAdd windowAyuda contentPrincipal
      
     contentAyuda <- vBoxNew False 0
     boxPackStart contentPrincipal contentAyuda PackNatural 0     
     containerSetBorderWidth contentAyuda 10
     
     text <- labelNew (Just "Seleccione el tema sobre el que necesita ayuda.")
     boxPackStart contentAyuda text PackNatural 0
     
    
  
     radios <- vBoxNew False 0 
     optionabi <- vBoxNew False 0
     optionab <- vBoxNew  False 0
     optionrf <- vBoxNew  False 0
     optionpf <- vBoxNew  False 0
     optionn <- vBoxNew False 0
     options <- vBoxNew False 0
     optionrm <- vBoxNew False 0
     boxPackStart contentAyuda radios PackNatural 0
     radioabi <- radioButtonNewWithLabel "Busqueda incremental "
     radioab <- radioButtonNewWithLabelFromWidget radioabi "Biseccion "
     radiorf <- radioButtonNewWithLabelFromWidget radioab "Regla falsa "
     radiopf <- radioButtonNewWithLabelFromWidget radiorf "Punto fijo "
     radion <- radioButtonNewWithLabelFromWidget radiopf "Newton "
     radioss <- radioButtonNewWithLabelFromWidget radion "Secante "
     radiorm <- radioButtonNewWithLabelFromWidget radioss "Raices Multiples"
     boxPackStart optionabi radioabi PackNatural 5
     boxPackStart optionab radioab PackNatural 5
     boxPackStart optionrf radiorf PackNatural 5
     boxPackStart optionpf radiopf PackNatural 5
     boxPackStart optionab radion PackNatural 5
     boxPackStart optionrf radioss PackNatural 5
     boxPackStart optionpf radiorm PackNatural 5
     boxPackStart radios optionabi PackNatural 0
     boxPackStart radios optionab PackNatural 0
     boxPackStart radios optionrf PackNatural 0
     boxPackStart radios optionpf PackNatural 0
     boxPackStart radios optionn PackNatural 0
     boxPackStart radios options PackNatural 0
     boxPackStart radios optionrm PackNatural 0
     
     textAyuda <- labelNew Nothing
     boxPackStart contentPrincipal textAyuda PackNatural 0
    

     onToggled radioabi $ do
               labelSetText textAyuda textbi
              
     onToggled radioab $ do
              labelSetText textAyuda textbiseccion
           
     onToggled radiorf $ do
              labelSetText textAyuda  textrfalsa
             
     onToggled radiopf $ do
              labelSetText textAyuda textpunto
             
     onToggled radion $ do
              labelSetText textAyuda textnewton
             
     onToggled radioss $ do
              labelSetText textAyuda textsecante
             
     onToggled radiorm $ do
              labelSetText textAyuda textraices
            

    
     widgetShowAll windowAyuda
     onDestroy windowAyuda mainQuit
     mainGUI
      

ayudaSE :: IO()
ayudaSE =  do
     initGUI
     windowAyuda <- windowNew
     set windowAyuda [windowTitle := "Ayudas Sistemas de Ecuaciones", windowDefaultWidth := 400,
                 windowDefaultHeight := 400 ]
     windowSetIconFromFile windowAyuda "lambda"
     contentPrincipal <- vBoxNew True 10
     containerAdd windowAyuda contentPrincipal
      
     contentAyuda <- vBoxNew False 0
     boxPackStart contentPrincipal contentAyuda PackNatural 0     
     containerSetBorderWidth contentAyuda 10
     
     text <- labelNew (Just "Seleccione el tema sobre el que necesita ayuda.")
     boxPackStart contentAyuda text PackNatural 0
     
    
  
     radios <- vBoxNew False 0 
     optionabi <- vBoxNew False 0
     optionab <- vBoxNew  False 0
     optionrf <- vBoxNew  False 0
     optionpf <- vBoxNew  False 0
     optionn <- vBoxNew False 0
     options <- vBoxNew False 0
     optionrm <- vBoxNew False 0
     boxPackStart contentAyuda radios PackNatural 0
     radioabi <- radioButtonNewWithLabel "Eliminacion Gaussiana "
     radioab <- radioButtonNewWithLabelFromWidget radioabi "Eliminación con Pivoteo Parcial "
     radiorf <- radioButtonNewWithLabelFromWidget radioab "ELiminación con Pivoteo Total "
     radiopf <- radioButtonNewWithLabelFromWidget radiorf "Jacobi "
     radion <- radioButtonNewWithLabelFromWidget radiopf "Gauss-Seidel"
     boxPackStart optionabi radioabi PackNatural 5
     boxPackStart optionab radioab PackNatural 5
     boxPackStart optionrf radiorf PackNatural 5
     boxPackStart optionpf radiopf PackNatural 5
     boxPackStart optionab radion PackNatural 5
     boxPackStart radios optionabi PackNatural 0
     boxPackStart radios optionab PackNatural 0
     boxPackStart radios optionrf PackNatural 0
     boxPackStart radios optionpf PackNatural 0
     boxPackStart radios optionn PackNatural 0
 
                 
     textAyuda <- labelNew Nothing
     boxPackStart contentPrincipal textAyuda PackNatural 0
    

     onToggled radioabi $ do
               labelSetText textAyuda textgs
              
     onToggled radioab $ do
              labelSetText textAyuda textpparcial
           
     onToggled radiorf $ do
              labelSetText textAyuda textpt
             
     onToggled radiopf $ do
              labelSetText textAyuda textjacobi
             
     onToggled radion $ do
              labelSetText textAyuda textseidel

    
     widgetShowAll windowAyuda
     onDestroy windowAyuda mainQuit
     mainGUI
      

ayudaInter :: IO()
ayudaInter =  do
     initGUI
     windowAyuda <- windowNew
     set windowAyuda [windowTitle := "Ayudas para Interpolación", windowDefaultWidth := 400,
                 windowDefaultHeight := 400 ]
     windowSetIconFromFile windowAyuda "lambda"
     contentPrincipal <- vBoxNew True 10
     containerAdd windowAyuda contentPrincipal
      
     contentAyuda <- vBoxNew False 0
     boxPackStart contentPrincipal contentAyuda PackNatural 0     
     containerSetBorderWidth contentAyuda 10
     
     text <- labelNew (Just "Seleccione el tema sobre el que necesita ayuda.")
     boxPackStart contentAyuda text PackNatural 0
     
    
  
     radios <- vBoxNew False 0 
     optionabi <- vBoxNew False 0
     optionab <- vBoxNew  False 0
     optionrf <- vBoxNew  False 0
     optionpf <- vBoxNew  False 0
     optionn <- vBoxNew False 0    
     boxPackStart contentAyuda radios PackNatural 0
     radioabi <- radioButtonNewWithLabel "Newton "
     radioab <- radioButtonNewWithLabelFromWidget radioabi "Lagrange "
     radiorf <- radioButtonNewWithLabelFromWidget radioab "Trazadores Lineales "
     radiopf <- radioButtonNewWithLabelFromWidget radiorf "Trazadores Cuadráticos "
     radion <- radioButtonNewWithLabelFromWidget radiopf "Trazadores Cúbicos"
   
     boxPackStart optionabi radioabi PackNatural 5
     boxPackStart optionab radioab PackNatural 5
     boxPackStart optionrf radiorf PackNatural 5
     boxPackStart optionpf radiopf PackNatural 5
     boxPackStart optionn radion PackNatural 5
     boxPackStart radios optionabi PackNatural 0
     boxPackStart radios optionab PackNatural 0
     boxPackStart radios optionrf PackNatural 0
     boxPackStart radios optionpf PackNatural 0
     boxPackStart radios optionn PackNatural 0
   
     
     textAyuda <- labelNew Nothing
     boxPackStart contentPrincipal textAyuda PackNatural 10
    

     onToggled radioabi $ do
               labelSetText textAyuda textdivididas
              
     onToggled radioab $ do
              labelSetText textAyuda textlagrange
           
     onToggled radiorf $ do
              labelSetText textAyuda textlineales
             
     onToggled radiopf $ do
              labelSetText textAyuda textcuadraticos
     onToggled radion $ do
              labelSetText textAyuda textcubicos
   

    
     widgetShowAll windowAyuda
     onDestroy windowAyuda mainQuit
     mainGUI
      
ayudaint :: IO()
ayudaint =  do
     initGUI
     windowAyuda <- windowNew
     set windowAyuda [windowTitle := "Ayudas Integración", windowDefaultWidth := 400,
                 windowDefaultHeight := 400 ]
     windowSetIconFromFile windowAyuda "lambda"
     contentPrincipal <- vBoxNew True 10
     containerAdd windowAyuda contentPrincipal
      
     contentAyuda <- vBoxNew False 0
     boxPackStart contentPrincipal contentAyuda PackNatural 0     
     containerSetBorderWidth contentAyuda 10
     
     text <- labelNew (Just "Seleccione el tema sobre el que necesita ayuda.")
     boxPackStart contentAyuda text PackNatural 0
     
    
  
     radios <- vBoxNew False 0 
     optionabi <- vBoxNew False 0
     optionab <- vBoxNew  False 0
     optionrf <- vBoxNew  False 0
     optionpf <- vBoxNew  False 0
     optionn <- vBoxNew False 0
     options <- vBoxNew False 0
     optionrm <- vBoxNew False 0
     boxPackStart contentAyuda radios PackNatural 0
     radioab <- radioButtonNewWithLabel "Trapecios"
     radiopf <- radioButtonNewWithLabelFromWidget radioab "Simpson 1/3 generalizado"
     radion <- radioButtonNewWithLabelFromWidget radiopf "Simpson 3/8 sencillo"
     radioss <- radioButtonNewWithLabelFromWidget radion "Iterativos "
     boxPackStart optionab radioab PackNatural 5
     boxPackStart optionpf radiopf PackNatural 5
     boxPackStart optionn radion PackNatural 5
     boxPackStart options radioss PackNatural 5
     boxPackStart radios optionab PackNatural 0
     boxPackStart radios optionpf PackNatural 0
     boxPackStart radios optionn PackNatural 0
     boxPackStart radios options PackNatural 0
     
     textAyuda <- labelNew Nothing
     boxPackStart contentPrincipal textAyuda PackNatural 0
              
     onToggled radioab $ do
              labelSetText textAyuda texttrapecio
           
     onToggled radiopf $ do
              labelSetText textAyuda textsimpson13
             
     onToggled radion $ do
              labelSetText textAyuda textsimpson38
             
     onToggled radioss $ do
              labelSetText textAyuda textIterativos
             
 
            

    
     widgetShowAll windowAyuda
     onDestroy windowAyuda mainQuit
     mainGUI

mtos' :: Matriz -> Integer -> String 
mtos' au n
    | n == 1 = show(map (\x -> snd x) (darFila au 1)) 
    | otherwise = (mtos' au (n-1)) ++ "\n"++ show(map (\x-> snd x)(darFila au n))  


mtos :: Matriz -> Integer -> IO()
mtos au n= do
             putStr ((mtos' au n) ++ "\n")
            

textpparcial = "Este método es una variante de la Eliminación Gaussiana,\n" ++
                                      "por lo que también trabaja con la matriz aumentada. El Pivoteo\n" ++ 
                                      "Parcial surge como respuesta al error de redondeo (propagación)\n" ++
                                      "que es característico en la Eliminación Gaussiana debido a la gran\n " ++
                                      "cantidad de operaciones requeridas para obtener un resultado. Para\n" ++
                                      "disminuir este error se pretende hacer los\n denominadores de los\n"++
                                      "multiplicadores (Es decir, los elementos de la diagonal) lo más grande\n" ++
                                      "posible para que al hallar los multiplicadores sean menores o iguales a 1\n" ++
                                      "y garantizar que al despejar no se tengan divisiones por cero o cercanas a\n " ++
                                      "cero. Esto se logra buscando el mayor elemento de columna k, en la etapa k,\n" ++
                                      "y mover la fila que lo contenga hacia la fila pivote, esto" ++
                                      "es llamado\n pivoteo parcial."    
  

textgs = "  Eliminación Gaussiana Simple\n" ++

         "  Este método es el más conocido para resolver sistemas de ecuaciones. Trabaja con la\n" ++
         "  matriz aumentada del sistema: Primero que todo se necesita hacer cero los valores\n" ++
         "  por debajo de la diagonal, para eso se utilizan los multiplicadores de cada fila los\n" ++
         "  cuales se calculan en k etapas, donde el objetivo de la etapa k es poner ceros en la\n" ++
         "  columna por debajo del elemento Akk, en esta etapa la fila k es llamada fila pivote. Los\n" ++
         "  multiplicadores se calculan con la siguiente fórmula:\n" ++

         "    Mik = Aik / Akk con i = k+1,...,n\n" ++

         "  Con los multiplicadores se calcula la fila nueva:\n" ++

         "  Aij = Aij – Mik * Akj con j = k,...,n+1\n" ++

         "  Cuando se tiene que la matriz es triangular superior se realiza sustitución regresiva\n" ++
         "  para hallar la solución del sistema."

textpt = "   Eliminación Gaussiana con Pivoteo Total\n" ++ 

         "  Este método tiene el mismo objetivo que el Pivoteo Parcial, reducir el error de\n" ++
         "  redondo. Funciona de manera similar simplemente que la búsqueda del elemento\n" ++
         "  mayor se realiza en toda la matriz y no solo en la columna k, esto implica que el\n" ++
         "  orden de las variables sea almacenado en una variable auxiliar pues se intercambian\n" ++
         "  columnas, y al mostrar el resultado se debe indicar qué variable corresponde a cuál.\n" 

textseidel = "Metodo de Gauss Seidel\n" ++

             "Este método es una variante del método de Jacobi por lo cual tiene las mismas\n" ++
             "características y entradas, lo único que se cambia es la ejecución del método ya que al\n" ++
             "contrario de Jacobi los valores que se evalúan en cada iteración son los que han sido\n" ++
             "generados en la iteración actual lo que ayuda a que la convergencia del mismo sea un\n" ++
             "poco más rápida. Esto representa una computación secuencial.\n"

textjacobi = "Metodo de Jacobi\n" ++

             "Este método es un método iterativo (Es una variante de Punto Fijo) para la solución\n" ++
             "de los sistemas de ecuaciones donde se sabe que el error característico es el error de\n" ++
             "truncamiento.\n" ++

             "Estos métodos son más útiles al momento de solucionar problemas de grandes\n" ++
             "magnitudes.\n" ++

             "Para comenzar se debe despejar una variable de cada una de las ecuaciones teniendo en\n" ++
             "cuenta que se despeja la variable que acompaña al máximo coeficiente en valor absoluto de la\n" ++
             "ecuación para que el método no genere errores y de una mejor solución, dado que el proceso\n" ++
             "de despeje es implementado en el algoritmo se recomienda que la matriz de coeficientes se\n" ++
             "ingrese de tal forma que sea diagonalmente dominante.\n" ++

             "Además de esto, se deben de plantear unos valores iniciales para cada variable que se usaran\n" ++
             "para remplazar en las ecuaciones formuladas. Se aconseja para tener una convergencia más\n" ++
             "rápida que estos valores sean cercanos a los valores a los cuales convergen.\n" ++

             "Este método utiliza en para cada iteración en las ecuaciones los valores calculados en la etapa\n" ++
             "anterior, representando una computación paralela.\n"

textbi = "Búsquedas incrementales\n" ++

         "Este es un método para hallar intervalos que contengan raíces para la función, su resultado es\n" ++
         "utilizado para hallar la raíz aproximada por medio de los métodos cerrados y abiertos. El método\n" ++
         "necesita un valor inicial (que puede aproximarse cualitativamente a través de un gráfico de la\n" ++
         "función) y un delta con el cual incrementará el valor inicial para determinar si hay cambio de\n" ++
         "signos entre los dos puntos, si no lo hay continúa haciendo la búsqueda cambiando el valor inicial\n" ++
         "por el calculado anteriormente y calculando un nuevo valor con el delta, el proceso se detiene\n" ++
         "cuando se encuentre un intervalo con esas condiciones o se sobrepasen las iteraciones.\n"

textbiseccion = "Bisección\n" ++

                "Este método pertenece al grupo de métodos llamado cerrado, convergente o por intervalos, ya\n" ++
                "que utiliza un intervalo que contiene una raíz para aproximarse a la misma y siempre converge\n" ++
                "aunque tiene un alto costo pues es lento. Para aproximar la raíz va dividiendo el intervalo en\n" ++
                "mitades iguales y elige el sub-intervalo que contiene la raíz (Es decir en el que haya cambio\n" ++
                "de signos), este proceso se repite hasta que se alcance la tolerancia deseada o hasta que se\n" ++
                "sobrepasen las iteraciones. Su error es En+1 = 1/2En."

textrfalsa = "Regla Falsa\n" ++

             "Este método también hace parte del grupo de métodos convergentes o cerrados, por lo que utiliza\n" ++
             "un intervalo que contenga una raíz y siempre converge aunque lentamente. En vez de dividir el\n" ++
             "intervalo en mitades iguales Regla Falsa traza secantes a la función y divide el intervalo en el punto\n" ++
             "en el cual la recta intersecta al eje x, tomando el intervalo que tenga cambio de signo, la fórmula\n" ++
             "para los nuevos valores de x es la siguiente:\n" ++

             "Xm = Xi – (F(Xi) * (Xi - Xo)) / (F(xi) – F(Xo))\n" ++

             "Su error es igual a En+1 = kEn^(1.7) en el mejor de los casos."


textpunto = "PUNTO FIJO\n" ++

            "Este método pertenece a un grupo de métodos llamados abierto o iterativo que solucionan\n" ++
            "en problema de la lentitud de los métodos convergentes, a costo de que no siempre se\n" ++
            "llegara a la solución. El método de punto fijo cambia la presentación de f(x) = 0 a x = g(x)\n" ++
            "y busca un punto al que la función g no le haga nada es decir un punto fijo.\n" ++

            "AYUDA PARA ELECCIÓN DE INTERVALO DE G(X) PARA PUNTO FIJO\n" ++

            "Necesito encontrar un x que al aplicarlo en g(x) quede intacto.\n" ++

            "Para que el método sea efectivo necesito encontrar una función g(x) que sea continua en\n" ++
            "un intervalo [a,b] para todo x que pertenezca a ese intervalo.\n" ++

            "Estas son las condiciones que debe tener en cuenta para que su intervalo sea bueno.\n" ++

            "• Debe ser continua la función en el intervalo\n" ++
            "• La pendiente de la función g (x) en ese intervalo debe ser menor que 1\n" ++
            "• Parte de un a y llega a un unico b\n" ++

            "Si usted encuentra un intervalo con las tres condiciones anteriores puede estar seguro\n" ++
            "que tomando cualquier x que pertenezca al intervalo [a,b], este le va a llegar a la raíz, eso\n" ++
            "sí depende de su elección del x , que tan rápido llegue. Estadísticamente es más seguro\n" ++
            "tomar el valor del medio del intervalo, porque este se encuentra más cerca de la raíz.\n"

textnewton = "Método de Newton\n" ++

             "Éste método como variante de Punto Fijo también hace parte de los métodos iterativos o\n" ++
             "abiertos, en el cual la funcion g(x) se construye de la siguiente forma:\n" ++

             "g(x) = x – f(x) / f'(x)\n" ++

             "Por lo que la fórmula para las nuevas x es:\n" ++

             "xn+1 = x_n – f(xn) / f'(xn)\n" ++

             "El método de Newton se aproxima a la raíz por medio de tangentes a la función, tomando\n" ++
             "como nuevo valor de x el punto de intersección de la misma con el eje de las abcisas.\n" ++

             "Su error es En = kEn^2\n" ++

             "Como parte de los métodos iterativos comparte sus características: No siempre converge\n" ++
             "aunque sea más rápido, y solo necesita un valor inicial en vez de un intérvalo.\n"

textsecante = "Metodo de la Secante\n" ++

              "Este método es una variante de Punto Fijo, es decir, es un método abierto, a diferencia\n" ++
              "del método de Newton es más fácil de aplicar ya que no necesita el uso de la derivada\n" ++
              "y esto lo hace uno de los métodos más usados para calcular raíces.\n" ++

              "Además en vez de un solo valor inicial necesita dos valores iniciales para comenzar a\n" ++
              "calcular nuevos valores, utilizando la siguiente fórmula\n" ++

              "xn+1=xn- fxn*(xn-xn-1)fxn-f(xn-1)\n" ++

              "Su error es En+1 = kEn^(1.7)\n" 

textraices = "Método Raíces múltiples\n" ++

             "Cuando la derivada de una función se aproxima a cero, es decir, cuando hay un punto\n" ++
             "máximo, mínimo o de inflexión, el método de Newton se vuelve lineal, por lo que hay\n" ++
             "un problema. Esto se debe a que la raíz tiene multiplicidad mayor que uno, lo que\n" ++
             "implica que la raíz se despeja varias veces cuando se factoriza la función.\n" ++

             "Si la multiplicidad es par entonces se puede concluir que el eje x es tangente a la función\n" ++
             "asociada (Máximo o mínimo), y si es impar se dice que la grafica de la función asociada a la\n" ++
             "ecuación cruza al eje x en un punto de inflexión.\n" ++

             "Este método además de la primera derivada utiliza la segunda derivada para recuperar la\n" ++
             "rapidez de convergencia. Con la segunda derivada se puede determinar la multiplicidad,\n" ++
             "sabiendo que si este converge a un numero diferente de cero la función tiene multiplicidad de\n" ++
             "dos, de lo contrario tendrá multiplicidad mayor a dos. La fórmula para calcular nuevos x es la\n" ++
             "siguiente:\n" ++

             "Xn+1 = Xn – (f(xn) * f(xn))/ [f’(xn) ]^2 – f(xn) * f’’(xn)\n" ++

             "Y su error es: En+1 = k* En^2\n"


textlagrange = "Metodo de Lagrange\n" ++

               "Este método halla el polinomio interpolante de la siguiente forma:\n" ++

               "P(x)=L0(x)f(x0)+L1(x)f(x1)+⋯+Ln(x)f(xn)\n" ++

               "Donde los Li(x) son funciones de x calculadas así:\n" ++

               "Li(x)=(x-x0)(x-x1)⋯(x-xi-1)(x-xi+1)⋯(x-xn) / (xi-x0)(xi-x1)⋯(xi-xi-1)(xi-xi+1)⋯(xi-xn)\n" 

textdivididas = "Metodo de Diferencias Divididas\n" ++

                "Este método se usa para hallar un polinomio interpolante que pase por unos puntos dados. Si\n" ++
                "se ingresa un valor a interpolar el método evalúa el polinomio y entrega el valor interpolado.\n" ++

                "El error característico de este método es el de redondeo por la cantidad de operaciones que se\n" ++
                "realizan de las cuales dependen resultados posteriores a ellas y el de truncamiento ya que\n" ++

                "La forma general del polinomio es la siguiente.\n" ++

                "fx=b0+ b1x-x0+b2x-x0x-x1+ ⋯ + bnx-x0x-x1⋯x-xn-1\n" ++

                "Donde cada bi se calcula como una diferencia dividida\n."

textcuadraticos = "Método de Trazadores Cuadraticos\n" ++

                  "En este método se mejora la aproximación que se da en los Trazadores Lineales ya\n" ++
                  "que se trazan parábolas que pasan por los puntos dados. Para esto se plantea un\n" ++
                  "sistema de ecuaciones de tamaño 3n, donde n es el número de puntos a interpolar\n" ++
                  "(Comenzando a contar desde 0), que se resuelve con un método de Eliminación\n" ++
                  "Gaussiana con Pivoteo, las ecuaciones del sistema surgen de las siguientes condiciones\n" ++
                  "que deben cumplir los trazadores:\n" ++

                  "*Conexión: Los polinomios de cada tramo deben conectar, es decir, evaluados en los\n" ++
                  "puntos comunes deben dar lo mismo.\n" ++

                  "*Extremos: Los polinomios de los extremos, evaluados en el primer y último punto\n" ++
                  "deben ser iguales al punto dado.\n" ++

                  "*Suavidad en la conexión: Las derivadas de los polinomios en los puntos de conexión\n" ++
                  "deben ser iguales.\n" ++

                  "*Suposición de la primera traza: Para completar el sistema se supone que la primera\n" ++
                  "traza es lineal, es decir que su segunda derivada es cero.\n" ++

                  "El error característico de estos métodos es el error de redondeo ya que utiliza la Eliminación\n" ++
                  "Gaussiana.\n" 

textcubicos = "Método de Trazadores Cúbicos\n" ++

              "Este método es una mejora del método de Trazadores Cuadráticos ya que la unión de los\n" ++
              "puntos se hace mediante funciones cubicas lo que hacen la aproximación más precisa. Su\n" ++
              "funcionamiento es igual al de los Trazadores Cuadráticos pero forma un sistema de tamaño 4n,\n" ++
              "y sus ecuaciones salen de las siguientes características:\n" ++

              "*Conexión: Los polinomios de cada tramo deben conectar, es decir, evaluados en los\n" ++
              "puntos comunes deben dar lo mismo.\n" ++

              "*Extremos: Los polinomios de los extremos, evaluados en el primer y último punto\n" ++
              "deben ser iguales al punto dado.\n" ++

              "*Suavidad en la conexión: Las derivadas de los polinomios en los puntos de conexión\n" ++
              "deben ser iguales.\n" ++

              "*Concavidad en conexión: La concavidad en los puntos de conexión es igual, es decir, las\n" ++
              "segundas derivadas en los puntos de conexión son iguales.\n" ++

              "*Suposición de los extremos: Para completar el sistema se supone que las trazas de los\n" ++
              "extremos son cuadráticas, es decir que sus segundas derivadas son cero.\n"

textlineales = "Método de Trazadores lineales\n" ++

               "Este método interpola mediante una función por tramos conformada por una serie de rectas\n" ++
               "que unen los puntos dados, es una aproximación no muy exacta pero puede dar una idea de\n" ++
               "cómo se comportara la curva.\n" ++

               "Para comenzar lo que se hace es plantear la formula de la recta para cada intervalo de puntos\n" ++
               "y se genera una función por tramos para cada par de puntos. Al ingresar un valor a interpolar\n" ++
               "se ubica a cuál tramo pertenece y se evalúa la ecuación.\n"

textIterativos = "Trapecio y Simpson 1/3 Iterativo\n" ++

                 "Estos métodos utilizan las Reglas de Sencillas y Generalizadas para aproximar\n" ++
                 "la integral definida entre a y b con varias iteraciones para alcanzar una\n" ++
                 "tolerancia. Los métodos reciben un numero de particiones n inicial, y calculan la\n" ++
                 "integral con la formula sencilla y generalizada, para sacar un error entre ellas,\n" ++
                 "luego incrementa el n en múltiplos de 2 y calcula la fórmula generalizada y el\n" ++
                 "error con cada nueva integral calculada hasta lograr la tolerancia deseada o se\n" ++
                 "sobrepasen las iteraciones.\n" 

textsimpson13 = "Regla de Simpson 1/3\n" ++

                "La Regla Sencilla aproxima una integral definida entre a y b utilizando tres puntos\n" ++
                "igualmente espaciados con una distancia h definida por\n" ++

                "h=b-a/2\n" ++

                "Teniendo los 3 puntos se utiliza la siguiente fómrula:\n" ++

                "I = (h/3) * [f(a)+4*f(a+h)+f(b)]\n" ++

                "La Regla Generalizada utiliza un número de particiones n (que debe ser par) para\n" ++
                "dividir el intervalo en n particiones igualmente espaciadas con una distancia h definida\n" ++
                "por\n" ++

                "h = b-a / n\n" ++

                "Y reemplazando en la siguiente fórmula:\n" ++

                "I = (h/3) * [f(a) + 4* sum(impares) + 2*sum(pares) + f(b)]\n" 

textsimpson38 = "Regla de Simpson 3/8\n" ++

                "La Regla Sencilla utiliza cuatro puntos para aproximar la integral definida entre a y b. El h está\n" ++
                "determinado por:\n" ++

                "h = b-a / 3\n" ++

                "Y la formula es:\n" ++

                "I = (3h/8) * [f(a) + 3*f(a+h) + 3*f(b-h) + f(b)]\n"

texttrapecio = "Regla del Trapecio\n" ++

           "La Regla Sencilla aproxima una integral definida entre a y b utilizando dos puntos\n" ++
           "igualmente espaciados con una distancia h definida por\n" ++

           "h=b-a\n" ++

           "Teniendo los 2 puntos se utiliza la siguiente fómrula:\n" ++

           "I = (h/2) * [f(a)+f(b)]\n" ++
               
           "La Regla Generalizada utiliza un número de particiones n para dividir el intervalo en n\n" ++
           "particiones igualmente espaciadas con una distancia h definida por\n" ++

           "h = b-a / n\n" ++

           "Y reemplazando en la siguiente fórmula:\n" ++

           "I = (h/2) * [f(a) + 2* sum(medio) + f(b)]\n" 
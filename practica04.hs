data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--ejercicio 1

longitud :: Arbol a -> Int 
longitud ArbolVacio = 0
longitud (Raiz a izquierdo derecho) = 1 + (longitud izquierdo)+(longitud derecho)

--ejercicio 2

profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz a izquierdo derecho) = 1 + max (profundidad izquierdo) (profundidad derecho)

-- ejercicio 3

ancho :: Arbol a -> Int 
ancho  ArbolVacio = 0
ancho (Raiz a ArbolVacio ArbolVacio)= 1
ancho (Raiz a izquierdo derecho)= (ancho izquierdo) + (ancho derecho)

--ejercicio 4

data Recorrido = InOrder | PreOrder | PostOrder 

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio a = []
recorrido (Raiz a izquierdo derecho) InOrder =
    recorrido izquierdo InOrder ++ [a] ++ recorrido derecho InOrder
recorrido (Raiz a izquierdo derecho) PreOrder =
    [a] ++ recorrido izquierdo PreOrder ++ recorrido derecho PreOrder
recorrido (Raiz a izquierdo derecho) PostOrder =
    recorrido izquierdo PostOrder ++ recorrido derecho PostOrder ++ [a]

-- ejercicio 5

niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz a izquierdo derecho) = [a] : combinacion (niveles izquierdo) (niveles derecho)

combinacion :: [[a]] -> [[a]] -> [[a]]
combinacion [] ys = ys
combinacion xs [] = xs
combinacion (x:xs) (y:ys) = (x ++ y) : combinacion xs ys

--ejercicio 6

maximo :: Arbol a -> a
maximo ArbolVacio = error "Un arbol vacio no tiene maximo"
maximo (Raiz a b ArbolVacio) = a
maximo (Raiz a b derecho) = maximo derecho  

--ejercicio 7

minimo :: Arbol a -> a
minimo ArbolVacio = error "Un arbol vacio no tiene minimo"
minimo (Raiz a ArbolVacio b) = a 
minimo (Raiz a izquierdo b) = minimo izquierdo  

--ejercicio 8

eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio b = error "No hay ningun elemento para eliminar"
eliminar (Raiz a ArbolVacio derecho) b = if a == b
                                            then derecho
                                            else error "El elemento no existe en el arbol"
eliminar (Raiz a izquierdo ArbolVacio) b = if a == b
                                            then izquierdo
                                            else error "El elemento no existe en el arbol"
eliminar (Raiz a izquierdo derecho) b = if b < a
                                            then (Raiz a (eliminar Izquierdo b) derecho)
                                            else if b > a
                                                  then (Raiz a izquierdo (eliminar derecho b))
                                                  else (Raiz (minimo derecho) izquierdo (eliminar derecho (minimo derecho)))
                                            

data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

--ejercicio 2

profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

--ejercicio 4

data TipoRecorrido = InOrder | PreOrder | PostOrder deriving Show

recorrido :: Arbol a -> TipoRecorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz a izq der) InOrder =
    recorrido izq InOrder ++ [a] ++ recorrido der InOrder
recorrido (Raiz a izq der) PreOrder =
    [a] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz a izq der) PostOrder =
    recorrido izq PostOrder ++ recorrido der PostOrder ++ [a]

--ejercicio 6

maximo :: Ord a => Arbol a -> a
maximo ArbolVacio = error "El árbol está vacío"
maximo (Raiz a ArbolVacio _) = a
maximo (Raiz a _ der) = max a (maximo der)

--ejercicio 7

minimo :: Ord a => Arbol a -> a
minimo ArbolVacio = error "El árbol está vacío"
minimo (Raiz a _ ArbolVacio) = a
minimo (Raiz a izq _) = min a (minimo izq)

--ejercicio 8

eliminar :: Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz a izq der) aEliminar
    | aEliminar < a = Raiz a (eliminar izq aEliminar) der
    | aEliminar > a = Raiz a izq (eliminar der aEliminar)
    | otherwise = eliminarNodo (Raiz a izq der)

eliminarNodo :: Ord a => Arbol a -> Arbol a
eliminarNodo (Raiz _ ArbolVacio der) = der
eliminarNodo (Raiz _ izq ArbolVacio) = izq
eliminarNodo (Raiz _ izq der) = 
    let (minDer, nuevoDer) = eliminarMinimo der
    in Raiz minDer izq nuevoDer

eliminarMinimo :: Arbol a -> (a, Arbol a)
eliminarMinimo (Raiz a ArbolVacio der) = (a, der)
eliminarMinimo (Raiz a izq der) = 
    let (minIzq, nuevoIzq) = eliminarMinimo izq
    in (minIzq, Raiz a nuevoIzq der)

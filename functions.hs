suma a b = a + b

filtrar _ [] = []
filtrar fn (a:b) = if (fn a) == True then
    a : filtrar fn b
    else filtrar fn b

getMenor [] = 0
getMenor [a] = a
getMenor (a:b) = if a < getMenor b
                    then a
                    else getMenor b

getMayor [] = 0
getMayor [a] = a
getMayor (a:b) = if a > getMayor b
                    then a
                    else getMayor b

getUltimo [a] = a
getUltimo (a:b) = getUltimo b

getPrimero [] = error "No hay primer posicion"
getPrimero (a:b) = a

quitarPrimero (a:b) = b

quitarUltimo [a] = []
quitarUltimo (a:b) = a : quitarUltimo b

isInArreglo x [] = False
isInArreglo x [a] = a == x
isInArreglo x (a:b) = if a == x
                        then True
                        else isInArreglo x b

quitarDuplicados [] = []
quitarDuplicados [a] = [a]
quitarDuplicados (a:b) = if isInArreglo a b
                            then quitarDuplicados b
                            else a : quitarDuplicados b

getLongitudArreglo [] = 0
getLongitudArreglo [a] = 1
getLongitudArreglo (a:b) = 1 + getLongitudArreglo b

getAutoSumaArreglo [] = 0
getAutoSumaArreglo [a] = a
getAutoSumaArreglo (a:b) = a + getAutoSumaArreglo b

getPromedioArreglo arr = (getAutoSumaArreglo arr) / getLongitudArreglo arr

quitarElemento x [] = []
quitarElemento x [a] = if a == x
                        then []
                        else [a]
quitarElemento x (a:b) = if a == x
                            then quitarElemento x b
                            else a : quitarElemento x b

--hacer sort
sortear [] = []
sortear [a] = [a]
sortear arr = getMenor arr : sortear( quitarElemento (getMenor arr) arr)

--makeset arr
makeset [a] = [a]
makeset (a:b) = if isInArreglo a b
                    then sortear ( makeset b )
                    else sortear ( a: makeset b )

--union a b
unir [] [] = []
unir [] arr = quitarDuplicados arr
unir arr [] = quitarDuplicados arr
unir (a:b) (c:d) = quitarDuplicados(a:c:unir b d)

--reverse a
reverso arr = if arr == []
                then []
                else 
                    if getLongitudArreglo arr == 1
                        then [getPrimero arr]
                        else getUltimo arr : reverso (quitarUltimo arr)

reverso2 [] = []
reverso2 [a] = [a]
reverso2 (a:b) = (reverso2 b) ++ [a]

--cantidadOcurrencias x arr
ocurrencias _ [] = 0
ocurrencias x (a:b) = if a == x
                        then 1 + ocurrencias x b
                        else ocurrencias x b

ocurrenciasV [] = []
ocurrenciasV (a:b) = (a,1+(ocurrencias a b)) : ocurrenciasV (quitarElemento a b)

charAt x [] = error "No hay donde buscar"
charAt x (a:b) = if x > 0
                    then charAt (x-1) b
                    else a

removerPosicionesIniciales [] _ = []
removerPosicionesIniciales arr pos = if pos > 0
                                then removerPosicionesIniciales (quitarPrimero arr) (pos -1)
                                else arr

dejarPosicionesIniciales [] _ = []
dejarPosicionesIniciales (a:b) pos = if pos > 0
                                        then a: dejarPosicionesIniciales b (pos -1)
                                        else []

searchChar _ [] _ = -1
searchChar x (a:b) y= if a == x
                        then y
                        else searchChar x b (y+1)

--search string src searched
{-searchString _ [] _ = -1
searchString [] _ _ = error "No hay donde buscar"
searchString arr (c:d) x = if (searchChar c arr 0) >= 0
                                then 
                                    if (searchString (removerPosicionesIniciales c:d (x+1) ) d x) >= 0
                                        then searchChar c arr
                                        else -1
                                else -1

findString [] [] = False
findString _ [] = False
findString [] _ = False
findString (a:b) (c:d) = False
-}
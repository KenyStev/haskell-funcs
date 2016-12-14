--verifica si un valor existe en el arreglo
exist _ [] = False
exist x (a:ss) = if x == a
                 then True
                 else exist x ss

--elimina los que cumplen con la condicion
filtrar _ [] = []
filtrar fn (a:ss) = if (fn a)
                      then filtrar fn ss
                      else a:(filtrar fn ss)

--elimina los duplicados y los ordena de forma asendente
makeSet [] = []
makeSet (a:ss) = sortA (a:(makeSet (filtrar (a==) ss)))

--del teorema de conjuntos, hace union de A y B
unionAB [] [] = []
unionAB [] (xs) = xs
unionAB (ys) [] = ys
unionAB (a:ss) (x:xs) = if a == x
                          then a:(unionAB (filtrar (a==) ss) (filtrar (a==) xs))
                          else a:x:(unionAB (filtrar (x==) (filtrar (a==) ss)) (filtrar (x==) (filtrar (a==) xs)))

--le hace revenso a una colenccion
reverseA [] = []
reverseA (a:ss) = (reverseA ss) ++ [a]

--reemplaza un valor en el la coleccion
replace [] _ _ = []
replace (a:ss) x y = if a == x
	                   then y:(replace ss x y)
	                   else a:(replace ss x y)

--devuelve el arreglo despues desde el offset en adelante
sliceTo [] _ = []
sliceTo (a:ss) x = if x > 0
                    then sliceTo (ss) (x-1)
                    else a:ss

--reemplaza una coleccion con otra, si existe dentro de una coleccion
replaceWord [] [] [] = []
replaceWord [] (ass) [] = []
replaceWord [] [] (sss) = []
replaceWord [] (ass) (sss) = []
replaceWord (a:ss) [] [] = a:ss
replaceWord (a:ss) (ass) [] = a:ss
replaceWord (a:ss) [] (sss) = a:ss
replaceWord (a:ss) (xs) (y:ys) = if (slice (a:ss) (len xs)) == xs
                                    then (y:ys) ++ (replaceWord (sliceTo (a:ss) (len xs)) xs (y:ys))
                                    else a:(replaceWord (ss) xs (y:ys))

--devuelve el offset donde comienza la coleccion en el arreglo
getOffsetStart [] [] _ = -1
getOffsetStart (a:ss) (xs) c = if (slice (a:ss) (len xs)) == xs
                            then c
                            else getOffsetStart ss xs (c+1)

--devuelve el unltimo valor del arreglo
getLast [a] = a
getLast (_:ss) = getLast ss

--devuelve el largo de una coleccion
len [] = 0
len [_] = 1
len (_:ss) = 1 + (len ss)

--devuelde el valor maximo de una coleccion
maxim [] = 0
maxim [a] = a
maxim (a:ss) = if a > (maxim ss)
                then a
                else (maxim ss)

--devuelve el valor minimo de una coleccion
minim [] = 0
minim [a] = a
minim (a:ss) = if a < (minim ss)
                then a
                else (minim ss)

--devuleve ordenado la coleccion
sortA [] = []
sortA [a] = [a]
sortA (ass) = (minim ass):sortA((filtrar ((minim ass)==) ass))

--cuenta cuantas veces se encuentra un valor en el arreglo
counting [] _ = 0
counting (a:ss) x = if a == x
                    then 1 + (counting ss x)
                    else counting ss x

--devuelve las ocurrencias de cuantas veces aparecen cada valor del arreglo
ocur [] = []
ocur (a:ss) = if len ss > 0
                then (a,counting (a:ss) a):(ocur (filtrar (a==) ss))
                else [(a,counting (a:ss) a)]

--devuelve la cantidad de elementos del arreglo desde 0 hasta la x
slice [] _ = []
slice (a:ss) x = if x > 0
                    then a:(slice ss (x-1))
                    else []
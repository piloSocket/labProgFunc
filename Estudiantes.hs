{- Grupo: X
   Integrante(s):
     Piloni, Francisco, 5.394.835-6
     Apellido, Nombre, XXXXXXXX
-}

module Estudiantes where

import JSONLibrary
    ( JSON,
      Object,
      lookupField,
      lookupFieldObj,
      filterArray,
      mkJArray,
      fromJString,
      fromJNumber,
      fromJArray, mkJObject,insertKV,fromJObject )
import TypedJSON ( typeOf,hasType, JSONType(TyArray, TyNum, TyString,TyObject) )
import AST (JSON(JNumber))
import Data.List (sort)


---------------------------------------------------------------------------------------
-- Importante:
-- Notar que NO se puede importar el módulo AST, que es interno a la biblioteca.
---------------------------------------------------------------------------------------
tyEstudiante :: JSONType
tyEstudiante =
  TyObject
    [ ("CI",       TyNum)
    , ("apellido", TyString)
    , ("cursos",   TyArray tyCurso)
    , ("nombre",   TyString)
    ]
tyCurso :: JSONType
tyCurso =
  TyObject
    [ ("anio",     TyNum)
    , ("codigo",   TyNum)
    , ("nombre",   TyString)
    , ("nota",     TyNum)
    , ("semestre", TyNum)
    ]



estaBienFormadoEstudiante :: JSON -> Bool
estaBienFormadoEstudiante e =
  case hasType e tyEstudiante of
    False -> False
    True  ->
      case lookupField e "cursos" >>= fromJArray of
        Nothing -> False
        Just cursos ->
          cursosOrdenados cursos


cursosOrdenados :: [JSON] -> Bool
cursosOrdenados []  = True
cursosOrdenados [_] = True
cursosOrdenados (c1:c2:cs) =
  ordenCorrecto c1 c2 && cursosOrdenados (c2:cs)


ordenCorrecto :: JSON -> JSON -> Bool
ordenCorrecto c1 c2 =
  let (a1,s1,co1) = claves c1
      (a2,s2,co2) = claves c2
  in
       (a1 > a2)
    || (a1 == a2 && s1 > s2)
    || (a1 == a2 && s1 == s2 && co1 < co2)


claves :: JSON -> (Integer,Integer,Integer)
claves c =
  ( getNum "anio"     c
  , getNum "semestre" c
  , getNum "codigo"   c
  )


getNum :: String -> JSON -> Integer
getNum key c =
  case lookupField c key >>= fromJNumber of
    Just n  -> n
    Nothing -> -1


-- getters
getCI :: JSON -> Maybe Integer
getCI e =
          case estaBienFormadoEstudiante e of
            False -> Nothing
            True  ->
              case lookupField e "CI" of
                Nothing -> Nothing
                Just v  -> fromJNumber v

getNombre :: JSON -> Maybe String
getNombre e = case estaBienFormadoEstudiante e of
                False -> Nothing
                True  ->
                  case lookupField e "nombre" of
                    Nothing -> Nothing
                    Just v -> fromJString v

getApellido :: JSON -> Maybe String
getApellido e = case estaBienFormadoEstudiante e of
                False -> Nothing
                True  ->
                  case lookupField e "apellido" of
                    Nothing -> Nothing
                    Just v -> fromJString v

getCursos :: JSON -> Maybe JSON
getCursos e = if estaBienFormadoEstudiante e then lookupField e "cursos" else Nothing

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados e =
  case getCursos e of
    Nothing -> Nothing
    Just cursosJSON ->
      case fromJArray cursosJSON of        
        Nothing -> Nothing                 
        Just cursos ->
          let aprobados = filterArray
                (\c ->
                  case lookupField c "nota" >>= fromJNumber of
                    Just n  -> n > 2
                    Nothing -> False
                ) cursos
          in Just (mkJArray aprobados)

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio a e = 
  case getCursos e of
    Nothing-> Nothing
    Just cursos ->
      case fromJArray cursos of
        Nothing-> Nothing
        Just cs ->
          let jeje = filterArray 
                (\c->
                  case lookupField c "anio" >>= fromJNumber of
                    Just n -> n == a 
                    Nothing -> False
                ) cs
          in Just (mkJArray jeje)

getNota :: JSON -> Maybe Integer
getNota c = lookupField c "nota" >>= fromJNumber

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad est =
  if not (estaBienFormadoEstudiante est) then
      Nothing
  else
    case lookupField est "cursos" >>= fromJArray of
      Nothing -> Nothing
      Just [] -> Nothing
      Just cursos ->
        -- obtenemos las notas: [Maybe Integer]
        let notas = map getNota cursos
        in case sequence notas of
             Nothing -> Nothing               -- si alguna nota es Nothing
             Just ns ->
               let total = sum ns
                   cant  = length ns
               in Just (fromIntegral total / fromIntegral cant)

get :: String -> JSON -> Integer
get key curso =
  case lookupField curso key >>= fromJNumber of
    Just n  -> n
    Nothing -> -1


lessCurso :: JSON -> JSON -> Bool
lessCurso c1 c2 =
     a1 > a2
  || (a1 == a2 && s1 > s2)
  || (a1 == a2 && s1 == s2 && co1 < co2)
  where
    a1  = get "anio" c1
    a2  = get "anio" c2
    s1  = get "semestre" c1
    s2  = get "semestre" c2
    co1 = get "codigo" c1
    co2 = get "codigo" c2

insertCurso :: JSON -> [JSON] -> [JSON]
insertCurso c [] = [c]
insertCurso c (x:xs) = if lessCurso c x then c:x:xs
                        else x: insertCurso c xs
-- agrega curso a lista de cursos de un estudiante
-- agrega curso a lista de cursos de un estudiante
-- PRIMER parámetro: curso como Object JSON (viene de fromJObject (mkcurso ...))
-- SEGUNDO parámetro: estudiante como JSON (JObject ...)
addCurso :: Object JSON -> JSON -> JSON
addCurso cursoObj est =
  case fromJObject est of
    Nothing -> est   -- por las dudas; en los tests siempre es JObject
    Just estudiante ->
      case lookupFieldObj estudiante "cursos" >>= fromJArray of
        -- si no existe "cursos", lo agregamos al final
        Nothing ->
          let nuevoCurso = mkJObject cursoObj
          in mkJObject (estudiante ++ [("cursos", mkJArray [nuevoCurso])])

        -- si existe, insertamos ordenado y reemplazamos el campo manteniendo el orden
        Just cursos ->
          let nuevoCurso = mkJObject cursoObj
              nuevos     = insertCurso nuevoCurso cursos
              reemplazar (k,v)
                | k == "cursos" = ("cursos", mkJArray nuevos)
                | otherwise     = (k,v)
          in mkJObject (map reemplazar estudiante)



replace :: Eq k => k -> (k,v) -> [(k,v)] -> [(k,v)]
replace key new [] = []
replace key new ((k,v):xs)
  | key == k  = new : xs
  | otherwise = (k,v) : replace key new xs



{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module JSONLibrary
 (lookupField,
  lookupFieldObj,
  keysOf,
  valuesOf,
  entriesOf,
  leftJoin,
  rightJoin,
  filterArray,
  insertKV,
  sortKeys,
  mkJString, mkJNumber, mkJBoolean, mkJNull, mkJObject, mkJArray,
  fromJString, fromJNumber, fromJBoolean, fromJObject, fromJArray,
  isJString, isJNumber, isJBoolean, isJNull, isJObject, isJArray,
  JSON(),importJSON,
  Object()
 )
where

import AST
import Text.Read (Lexeme(String))
import Control.Arrow (Arrow(first))
import Data.Foldable (minimumBy)


{- lookupField:
 Cuando el primer argumento es un objeto y tiene como clave el valor
 dado como segundo argumento, entonces se retorna el valor JSON
 correspondiente (bajo el constructor {\tt Just}). De lo contrario se
 retorna {\tt Nothing}. Si un objeto tiene claves repetidas, se
 retorna el valor de más a la derecha.
-}
lookupField :: JSON -> Key -> Maybe JSON
lookupField (JObject []) _ = Nothing
lookupField (JObject (x:xs)) k = if fst (x) == k then Just (snd (x))
                              else lookupField (JObject xs) k
lookupField _ _ = Nothing

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj :: Object JSON -> Key -> Maybe JSON
lookupFieldObj o k = lookupField (JObject o) k

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object JSON -> [Key]
keysOf [] = []
keysOf (x:xs) = fst (x) : keysOf (xs)

-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object JSON -> [JSON]
valuesOf []= []
valuesOf (x:xs) = snd (x) : valuesOf (xs)

-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object JSON -> [(Key,JSON)]
entriesOf [] = []
entriesOf (x:xs) = x:xs

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del primer objeto.
leftJoin :: Object a -> Object a -> Object a
leftJoin [] [] = []
leftJoin [] (x:xs) = x:xs
leftJoin (x:xs) [] = x:xs
leftJoin (x:xs) (y:ys) = if elem (fst y) (map fst (x:xs)) then leftJoin (x:xs) ys
                        else leftJoin (y:x:xs) ys
-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del segundo objeto.
rightJoin :: Object a -> Object a -> Object a
rightJoin [] [] = []
rightJoin [] (y:ys) = y:ys
rightJoin (x:xs) [] = x:xs
rightJoin (x:xs) (y:ys) = if elem (fst x) (map fst (y:ys)) then rightJoin xs (y:ys)
                          else rightJoin xs (x:y:ys)

-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) ->  Array -> Array
filterArray f [] = []
filterArray f (x:xs) = if f x then x : filterArray f xs
                        else filterArray f xs

-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV (k, v) [] = [(k,v)]
insertKV (k,v) (x:xs) = if k <= fst x then (k,v):x:xs
                        else x: insertKV (k,v) xs
-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV (k,v) xs = (k,v) : xs

-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys [] = []
sortKeys (x:xs) = insertKV x (sortKeys xs)


-- constructoras
mkJString :: String -> JSON
mkJString = JString

mkJNumber :: Integer -> JSON
mkJNumber = JNumber

mkJBoolean :: Bool -> JSON
mkJBoolean = JBoolean

mkJNull :: () -> JSON
mkJNull () = JNull

mkJArray :: [JSON] -> JSON
mkJArray = JArray

mkJObject :: [(Key, JSON)] -> JSON
mkJObject = JObject


-- destructoras
fromJString :: JSON -> Maybe String
fromJString (JString s) = Just s
fromJString _ = Nothing

fromJNumber :: JSON -> Maybe Integer
fromJNumber (JNumber n) = Just n
fromJNumber _ = Nothing

fromJBoolean  :: JSON -> Maybe Bool
fromJBoolean (JBoolean b)= Just b
fromJBoolean _ = Nothing

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject (JObject o) = Just o
fromJObject _ = Nothing

fromJArray :: JSON -> Maybe [JSON]
fromJArray (JArray s)= Just s
fromJArray _ = Nothing


-- predicados
isJNumber :: JSON -> Bool
isJNumber (JNumber n) = True
isJNumber _ = False

isJNull :: JSON -> Bool
isJNull JNull = True
isJNull _ = False

isJString :: JSON -> Bool
isJString (JString s)  = True
isJString _ = False

isJObject :: JSON -> Bool
isJObject (JObject o) = True
isJObject _ = False

isJArray :: JSON -> Bool
isJArray (JArray a)  = True
isJArray _ = False

isJBoolean :: JSON -> Bool
isJBoolean (JBoolean b) = True
isJBoolean _ = False


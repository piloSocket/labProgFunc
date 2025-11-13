{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module TypedJSON where

import AST
import JSONLibrary
import Control.Monad
import Data.List
import Data.Maybe (isJust)
import Data.Maybe(fromJust)


-- Tipos JSON
data JSONType
  = TyString
  | TyNum
  | TyObject (Object JSONType)
  | TyArray JSONType
  | TyBool
  | TyNull
  deriving (Show, Eq)


-- dado un valor JSON se infiere el tipo. Se devuelve
-- Nothing si el valor está mal tipado
typeOf :: JSON -> Maybe JSONType
typeOf (JBoolean b) = Just TyBool
typeOf (JNumber n) = Just TyNum
typeOf (JNull) = Just TyNull
typeOf (JString s) = Just TyString
typeOf (JArray []) = Nothing
typeOf (JArray xs) = let mts = map typeOf (xs)
                        in if all isJust mts
                            then let ts = map fromJust mts
                                in if all (== head ts) ts
                                    then Just (TyArray (head ts))
                                    else Nothing
                            else Nothing
typeOf (JObject []) = Nothing
typeOf (JObject xs) =
    let keys = map fst xs
    in if length keys /= length (nub keys)
          then Nothing
          else let mts = map (\(_,v) -> typeOf v) xs
               in if all isJust mts
                     then let ts    = map fromJust mts
                              pairs = zip keys ts
                          in Just (TyObject (sortKeys pairs))
                     else Nothing

-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf [] = True
objectWf (x:xs) = if sortKeys (x:xs) == x:xs then True
                  else False

-- decide si todos los tipos objeto contenidos en un tipo JSON
-- están bien formados.
typeWf :: JSONType -> Bool
typeWf TyBool = True
typeWf TyNull = True
typeWf TyNum = True
typeWf TyString = True
--typeWf TyObject o = if objectWf o && 



-- dado un valor JSON v, y un tipo t, decide si v tiene tipo t.
hasType :: JSON -> JSONType -> Bool
hasType = undefined

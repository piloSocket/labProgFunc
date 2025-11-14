module Tests where

import AST
import JSONLibrary
import TypedJSON
import Estudiantes

import Data.Maybe (fromJust)

-- Tipo que modeliza los resultados de test: o bien el test se aprueba,
-- o se falla. Usamos el mensaje asociado a la falla para identificar el test. 
data TestRes = Ok | Fail Message deriving (Show, Eq)

type Message = String

-- La función assertEq testea si a y b son valores estructuralmente iguales.
-- Si no lo son, se construye la falla con el mensaje m.
assertEq :: Eq a => Message -> a -> a -> TestRes
assertEq m a b = if a == b then Ok else Fail m

-- tipo algebraico que enumera los tests a realizar
data TestSet
  = Show'

  | LookupField | LookupFieldObj | KeysOf | ValuesOf | EntriesOf
  | LeftJoin | RightJoin | FilterArray | InsertKV | ConsKV | SortKeys
  | Mk | From | Is

  | TypeOf | ObjectWf | TypeWf | HasType

  | EstaBienFormadoEstudiante | Get
  | Aprobados | EnAnio | PromedioEscolaridad | AddCurso
  deriving Enum


{------------------------------------------------------------------------------
A continuación se definen funciones para construir valores que usamos para
construir tests. Notar que son funciones parciales,
por ejemplo no tiene sentido evaluar `jval 5`.

Pueden extender los tests sin modificar los existentes construyendo valores
con índices nuevos
------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
-- ejemplos de valores JSON
------------------------------------------------------------------------------}
jval 1 =
    JObject
    [("employees",
     JArray [
       JObject [("firstName", JString "John"),
                ("lastName", JString "Doe")],
       JObject [("firstName", JString "Anna"),
                ("lastName", JString "Smith")],
       JObject [("firstName", JString "Peter"),
                ("lastName", JString "Jones")]
      ]),
     ("number", JNumber 12),
     ("boolean", JBoolean False),
     ("null", JNull)]
jval 2 =
    JArray [JNull,
            JObject [("key 1", JNumber 1),
                     ("key 2", JBoolean False)],
            jval 3,
            JNull ]
jval 3 = JObject [("1", JNull), ("2", JObject (oval 2))]
jval 4 = JObject [("1", JNull), ("3", JArray [JNull, JNull]), ("2", jval 1)]


{------------------------------------------------------------------------------
-- respectivos resultados de typeOf
------------------------------------------------------------------------------}
jty 1 = Just $ TyObject
        [ ("boolean", TyBool)
        , ( "employees"
              , TyArray (TyObject [("firstName", TyString),
                                   ("lastName", TyString)]))
        , ("null", TyNull)
        , ("number", TyNum)
       ]
jty 2 = Nothing
jty 3 = Nothing
jty 4 = Just $ TyObject [("1", TyNull), ("2", fromJust $ jty 1), ("3", TyArray TyNull)]


{------------------------------------------------------------------------------
-- ejemplos de objetos JSON
------------------------------------------------------------------------------}

-- bien tipado, ordenado
oval 1 = [("1", JNumber 1), ("2", JNumber 1), ("3", JNumber 1)]

-- mismo tipo
oval 2 = [("3", JNumber 2), ("1", JNumber 2), ("2", JNumber 2)]

-- bien tipado, no ordenado
oval 3 = [("4", JNumber 1), ("6", JNumber 1), ("5", JObject (oval 1))]
oval 4 = [("1", JNull)]

-- mal tipado
oval 5 = [("1", JNumber 1), ("3", JNumber 1), ("3", JNumber 2)]
oval 6 = []


{------------------------------------------------------------------------------
-- respectivos resultados de typeOf para TyObject (obj n)
------------------------------------------------------------------------------}
-- ok
oty 1 = Just $ TyObject [("1", TyNum),("2", TyNum),("3", TyNum)]
oty 2 = oty 1
-- objectWf pero no se cumple `typeWf (TyObject (typObj 2))`
oty 3 = Just $ TyObject $
  [("4", TyNum), ("5", fromJust $ oty 1), ("6", TyNum)]
oty 4 = Just $ TyObject [("1", TyNull)]
oty 5 = Nothing
oty 6 = Just $ TyObject []


-- tipos mal formados
illty 1 = TyArray $ illty 3
illty 2 = TyObject  [("1", TyNull), ("2", illty 1)]
illty 3 = TyObject [("1", TyNull),("2", TyArray TyNull),("2", TyNull)]
illty 4 = TyObject [("1", TyNull),("4", TyArray TyNull),("2", TyNull)]



-- curso dummy, solo los campos que interesan para testear 
mkcurso anio semestre codigo
  = JObject
    [ ("nombre", JString "")
    , ("codigo", JNumber codigo)
    , ("anio", JNumber anio)
    , ("semestre", JNumber semestre)
    , ("nota", JNumber 1)
    ]

{------------------------------------------------------------------------------
Ejemplos de estudiante
------------------------------------------------------------------------------}

-- bien formado
est 1 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 3 2 1,
                    mkcurso 3 2 2,
                    mkcurso 3 2 3,
                    mkcurso 3 1 1,
                    mkcurso 3 1 2,
                    mkcurso 3 1 3,
                    mkcurso 2 2 0,
                    mkcurso 2 2 1,
                    mkcurso 1 2 0,
                    mkcurso 1 1 0
                    ])]
-- bien formado, "realista"
est 2 =
  (JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos"
       , JArray
           [ JObject
               [ ("nombre", JString "Calculo DIV")
               , ("codigo", JNumber 123)
               , ("anio", JNumber 2024)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 1)
               ]
           , JObject
               [ ("nombre", JString "Calculo DIV")
               , ("codigo", JNumber 123)
               , ("anio", JNumber 2024)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 7)
               ]
           , JObject
               [ ("nombre", JString "Calculo DIVV")
               , ("codigo", JNumber 124)
               , ("anio", JNumber 2023)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 2)
               ]
           , JObject
               [ ("nombre", JString "Programación 1")
               , ("codigo", JNumber 130)
               , ("anio", JNumber 2023)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 12)
               ]
           , JObject
               [ ("nombre", JString "Programación 2")
               , ("codigo", JNumber 131)
               , ("anio", JNumber 2023)
               , ("semestre", JNumber 2)
               , ("nota", JNumber 0)
               ]
           , JObject
               [ ("nombre", JString "Programación 2")
               , ("codigo", JNumber 131)
               , ("anio", JNumber 2021)
               , ("semestre", JNumber 1)
               , ("nota", JNumber 0)
               ]
           ])
     ])
-- mal formado, años desordenados
est 3 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 2 2 1,
                    mkcurso 3 2 2
                    ])]
-- mal formado, semestres desordenados
est 4 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 2 1 1,
                    mkcurso 2 2 2
                    ])]

-- mal formado, códigos desordenados
est 5 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 1 1 2,
                    mkcurso 1 1 1
                    ])]
-- mal formado, falta clave
est 6 =
  (JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)])
-- mal formado, sobra clave
est 7 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ("cursos", JArray [mkcurso 1 1 1])
     , ("?", JNull)]
-- mal formado, clave mal tipada
est 8 = JObject
     [ ("nombre", JNull)
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ("cursos", JArray [mkcurso 1 1 1])]
-- mal formado, arreglo mal tipado
est 9 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ("cursos", JArray [])]

-- para testear addcurso
est 10 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 3 2 10,
                    mkcurso 3 1 10,
                    mkcurso 2 2 10,
                    mkcurso 2 2 20
                    ])]
est 11 = JObject
     [ ("nombre", JString "Haskell")
     , ("apellido", JString "Curry")
     , ("CI", JNumber 12345678)
     , ( "cursos", JArray
                   [mkcurso 4 1 10,
                    mkcurso 3 2 9,
                    mkcurso 3 2 10,
                    mkcurso 3 2 11,
                    mkcurso 3 1 9,
                    mkcurso 3 1 10,
                    mkcurso 3 1 11,
                    mkcurso 2 2 10,
                    mkcurso 2 2 20,
                    mkcurso 1 1 1
                    ])]


{------------------------------------------------------------------------------
 TESTS
------------------------------------------------------------------------------}

-- La función `tests :: TestSet -> [TestRes]` construye la lista de tests
-- correspondiente a cada conjunto de tests, 

tests LookupField
  = assertEq "lookupField 1"
    (lookupField (jval 1) "number")
    (Just (JNumber 12))

  : assertEq "lookupField 2"
    (lookupField (jval 1) "employees")
    (Just (JArray [
       JObject [("firstName", JString "John"),
                ("lastName", JString "Doe")],
       JObject [("firstName", JString "Anna"),
                ("lastName", JString "Smith")],
       JObject [("firstName", JString "Peter"),
                ("lastName", JString "Jones")]
      ]))

  : assertEq "lookupField 3"
    (lookupField (jval 1) "1")
    Nothing

  : assertEq "lookupField 4"
    (lookupField (JNull) "1")
    Nothing

  : assertEq "lookupField 5"
    (lookupField (mkJBoolean True) "1")
    Nothing

  : assertEq "lookupField 6"
    (lookupField (mkJArray []) "1")
    Nothing

  : assertEq "lookupField 7"
    (lookupField (mkJObject (oval 3)) "3")
    Nothing

  : assertEq "lookupField 8"
    (lookupField (mkJObject (oval 3)) "5")
    (Just (JObject $ oval 1))

  : []

tests LookupFieldObj
  = assertEq "lookupFieldObj 1"
    (lookupFieldObj (oval 3) "6")
    (Just (JNumber 1))

  : assertEq "lookupFieldObj 2"
    (lookupFieldObj (oval 3) "1")
    Nothing
    
  : []

tests KeysOf
  =  assertEq "keysOf 1"
     (keysOf (oval 1))
     ["1", "2", "3"]

   : assertEq "keysOf 2"
     (keysOf (oval 2))
     ["3", "1", "2"]
    
   : assertEq "keysOf 3"
     (keysOf (oval 3))
     ["4", "6", "5"]
     
   : assertEq "keysOf 4"
     (keysOf (oval 4))
     ["1"]

   : assertEq "keysOf 5"
     (keysOf (oval 5))
     ["1", "3", "3"]

     : []

tests ValuesOf
  = assertEq "valuesOf 1"
    (valuesOf (oval 1))
    (take 3 (repeat (JNumber 1)))

  : assertEq "valuesOf 2"
    (valuesOf (oval 2))
    (take 3 (repeat (JNumber 2)))

  : assertEq "valuesOf 3"
    (valuesOf (oval 3) !! 2)
    (JObject (oval 1))

  : assertEq "valuesOf 4"
    (valuesOf (oval 5))
    (map JNumber [1,1,2])

  : []

tests EntriesOf
  = [assertEq ("entriesOf " ++ show i) (zip (keysOf (oval i)) (valuesOf (oval i)))
                                      (entriesOf (oval i))
    | i <- [1,2,3]]


tests LeftJoin
  = assertEq "leftJoin 1"
       (leftJoin (oval 1)(oval 2))
       (oval 1)

  : assertEq "leftJoin 2"
       (leftJoin (oval 1)(oval 3))
        [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1),
         ("4",JNumber 1),("6",JNumber 1),("5",JObject (oval 1))]

  : assertEq "leftJoin 3"
       (leftJoin (oval 1)(oval 4))
       (oval 1)

  : assertEq "leftJoin 4"
       (leftJoin (oval 4)(oval 1))
       [("1",JNull),("2", JNumber 1), ("3", JNumber 1)]

  : assertEq "leftJoin 5"
       (leftJoin (oval 3) [("4", JNull)])
       (oval 3)

  : []

tests RightJoin
  = assertEq "rightJoin 1"
     (rightJoin (oval 1) (oval 4))
     [("2", JNumber 1), ("3", JNumber 1), ("1",JNull)]

  : assertEq "rightJoin 2"
     (rightJoin (oval 2) (oval 3))
     (oval 2 ++ oval 3)

  : assertEq "rightJoin 3"
     (rightJoin (oval 4) (oval 3))
     (oval 4 ++ oval 3)

  : assertEq "rightJoin 4"
     (rightJoin (oval 4) (oval 1))
     (oval 1)

  : []

tests FilterArray
  = assertEq "filterArray 1"
     (filterArray (isJNull) ((fromJust . fromJArray) (jval 2)))
     [JNull,JNull]

  : assertEq "filterArray 2"
     (filterArray (const False) [])
     []

  : assertEq "filterArray 3"
     (filterArray (const True) ((fromJust . fromJArray) (jval 2)))
     ((fromJust . fromJArray) (jval 2))

  : assertEq "filterArray 4"
     (filterArray (const False) ((fromJust . fromJArray) (jval 2)))
     []

  : []

tests InsertKV
  = assertEq "insertKV 1"
    (insertKV ("4", JNull) (oval 1))
    [("1",JNumber 1),("2",JNumber 1),("3",JNumber 1),("4",JNull)]

  : assertEq "insertKV 2"
     (insertKV ("5", JNull) (oval 3))
     [("4",JNumber 1),("5",JNull),("6",JNumber 1),("5", JObject (oval 1))]

  : assertEq "insertKV 3"
     (insertKV ("1", JNull) (oval 3))
     [("1",JNull),("4",JNumber 1),("6",JNumber 1),("5",JObject (oval 1))]

  : []

tests SortKeys
  = assertEq "sortKeys 1"
     (sortKeys $ oval 1)
     (oval 1)

  : assertEq "sortKeys 2"
     (sortKeys $ oval 4)
     (oval 4)

  : assertEq "sortKeys 3"
     (sortKeys $ oval 3)
     [("4", JNumber 1), ("5", JObject (oval 1)), ("6", JNumber 1) ]

  : []

tests TypeWf
 = assertEq "typeWf 1"
    (typeWf (fromJust $ jty 1))
    True

 : assertEq "typeWf 2"
    (typeWf (fromJust $ jty 4))
    True

 : assertEq "typeWf 3"
    (typeWf $ fromJust $ oty 1)
    True

 : assertEq "typeWf 4"
    (typeWf $ fromJust $ oty 3)
    True

 : assertEq "typeWf 5"
    (typeWf $ fromJust $ oty 4)
    True

 : assertEq "typeWf 6"
   (typeWf $ fromJust $ oty 6)
   False

 : assertEq "typeWf 7"
   (typeWf (TyObject [("b", TyNull), ("a", TyNull)]))
   False

 : assertEq "typeWf 8"
   (typeWf (TyObject [("a", TyNull), ("A", TyNull)]))
   False

 : assertEq "typeWf 9"
   (typeWf (TyObject [("a", TyNull), ("b", TyObject [ ("b", TyNull), ("a", TyNull)])]))
   False

 : assertEq "typeWf 10"
   (typeWf $ illty 1)
   False

 : assertEq "typeWf 11"
   (typeWf $ illty 2)
   False

 : assertEq "typeWf 12"
   (typeWf $ illty 3)
   False

 : assertEq "typeWf 13"
   (typeWf $ illty 4)
   False

  : []

tests ObjectWf
 = assertEq "objectWf 1"
   (objectWf [("a", TyNull), ("b", TyObject [ ("b", TyNull), ("a", TyNull)])])
   True

  : assertEq "objectWf 2"
   (objectWf [("b", TyNull), ("a", TyNull)])
   False

  : assertEq "objectWf 3"
   (objectWf [("a", TyNull), ("b", TyNull)])
   True

  : []

tests TypeOf
  = assertEq "typeOf 1"
     (typeOf (jval 1))
     (jty 1)

  : assertEq "typeOf 2"
     (typeOf (jval 4))
     (jty 4)

  : assertEq "typeOf 3"
    (typeOf (JObject [("", JNull)]))
    (Just $ TyObject [("", TyNull)])

  : assertEq "typeOf 4"
     (typeOf (est 1))
     (Just tyEstudiante)

  : assertEq "typeOf 5"
     (typeOf (est 2))
     (Just tyEstudiante)

  -- notar que est 3, 4, 5, tipan, aunque no satisfagan las invariantes
  -- de buena formación que chequea estaBienFormadoEstudiante
  : assertEq "typeOf 6"
     (typeOf (est 3))
     (Just tyEstudiante)

  : assertEq "typeOf 7"
     (typeOf (est 4))
     (Just tyEstudiante)

  : assertEq "typeOf 8"
     (typeOf (est 5))
     (Just tyEstudiante)

  : []
    where
        tyEstudiante =
          TyObject [ ("CI",       TyNum),
                     -- notar que 'C'<'a' en el orden de Char
                     ("apellido", TyString),
                     ("cursos",   TyArray tyCurso),
                     ("nombre",   TyString)]
        tyCurso =
          TyObject [("anio",     TyNum),
                    ("codigo",   TyNum),
                    ("nombre",   TyString),
                    ("nota",     TyNum),
                    ("semestre", TyNum)]

tests HasType
  = assertEq "hasType 1"
    (hasType (jval 1) (fromJust $ jty 1)) True

  : assertEq "hasType 2"
    (hasType (jval 2) (TyArray TyNull)) False

  : assertEq "hasType 3"
    (hasType (JArray []) (TyArray TyNull)) False

  :[]

tests EstaBienFormadoEstudiante
  = assertEq "estaBienFormadoEstudiante 1"
     (estaBienFormadoEstudiante (est 1))
     True

  : assertEq "estaBienFormadoEstudiante 2"
     (estaBienFormadoEstudiante (est 2))
     True

  : assertEq "estaBienFormadoEstudiante 3"
     (estaBienFormadoEstudiante (est 3))
     False

  : assertEq "estaBienFormadoEstudiante 4"
     (estaBienFormadoEstudiante (est 4))
     False

  : assertEq "estaBienFormadoEstudiante 5"
     (estaBienFormadoEstudiante (est 5))
     False

  : assertEq "estaBienFormadoEstudiante 6"
     (estaBienFormadoEstudiante (est 6))
     False

  : assertEq "estaBienFormadoEstudiante 7"
     (estaBienFormadoEstudiante (est 7))
     False

  : assertEq "estaBienFormadoEstudiante 8"
     (estaBienFormadoEstudiante (est 8))
     False

  : assertEq "estaBienFormadoEstudiante 9"
     (estaBienFormadoEstudiante (est 9))
     False

  : []

tests Get
  = assertEq "get 1"
     (getCI (est 1))
     (Just 12345678)

  : assertEq "get 2"
     (getNombre (est 1))
     (Just "Haskell")

  : assertEq "get 3"
     (getApellido (est 1))
     (Just "Curry")

  : assertEq "get 4"
     (getCursos (est 1))
     (Just $ JArray [mkcurso 3 2 1,
                     mkcurso 3 2 2,
                     mkcurso 3 2 3,
                     mkcurso 3 1 1,
                     mkcurso 3 1 2,
                     mkcurso 3 1 3,
                     mkcurso 2 2 0,
                     mkcurso 2 2 1,
                     mkcurso 1 2 0,
                     mkcurso 1 1 0
                    ])

  : []

tests Aprobados
  = assertEq "aprobados 1"
     (aprobados (est 2))
     (Just
       (JArray
         [ JObject
           [ ("nombre", JString "Calculo DIV")
           , ("codigo", JNumber 123)
           , ("anio", JNumber 2024)
           , ("semestre", JNumber 1)
           , ("nota", JNumber 7)
           ]
         , JObject
           [ ("nombre", JString "Programación 1")
           , ("codigo", JNumber 130)
           , ("anio", JNumber 2023)
           , ("semestre", JNumber 2)
           , ("nota", JNumber 12)
           ]
         ])
     )

  : []

tests EnAnio
  = assertEq "enAnio 1"
    (enAnio 3 (est 1))
    (Just (JArray
        [mkcurso 3 2 1,
          mkcurso 3 2 2,
          mkcurso 3 2 3,
          mkcurso 3 1 1,
          mkcurso 3 1 2,
          mkcurso 3 1 3]
          ))
  : []

tests PromedioEscolaridad
  = assertEq "promedioEscolaridad 1"
      (promedioEscolaridad (est 2))
      (Just ((1 + 7 + 2 + 12 + 0 + 0) /6))

  : []

-- test para show (asume instancia de Eq con igualdad estructural para JSON, se
-- puede implementar (al final), o agregar cláusula deriving en AST..)
-- Ojo, si a la llamada a read se le pasa un objeto mal formado vamos
-- a tener error en tiempo de ejecución y se va a abortar la ejecución
-- del resto de los tests. Si tienen la instancia de show mal
-- implementada pero quieren probar el resto, pueden no correr estos
-- tests modificando la definición de allTests
tests Show' =
  map
    (\a ->
       if a == ((read :: String -> JSON) . show) a
         then Ok
         else Fail "show")
    ([jval n | n <- [1, 2]] ++ map JObject [oval n | n <- [1..4]])

tests AddCurso
  = assertEq "addCurso 1"
    (addCurso (fromJust $ fromJObject $ mkcurso 4 1 10) .
     addCurso (fromJust $ fromJObject $ mkcurso 3 2 9)  .
     addCurso (fromJust $ fromJObject $ mkcurso 3 2 11) .
     addCurso (fromJust $ fromJObject $ mkcurso 3 1 9)  .
     addCurso (fromJust $ fromJObject $ mkcurso 3 1 11) .
     addCurso (fromJust $ fromJObject $ mkcurso 1 1 1)
     $ est 10)
    (est 11)

  : assertEq "addCurso 2"
    (addCurso (fromJust $ fromJObject $ mkcurso 3 2 11) .
     addCurso (fromJust $ fromJObject $ mkcurso 3 1 9)  .
     addCurso (fromJust $ fromJObject $ mkcurso 4 1 10) .
     addCurso (fromJust $ fromJObject $ mkcurso 3 2 9)  .
     addCurso (fromJust $ fromJObject $ mkcurso 3 1 11) .
     addCurso (fromJust $ fromJObject $ mkcurso 1 1 1)
     $ est 10)
    (est 11)
  : []

tests _ = []


-- concatena todos los tests
allTests = concat [tests a | a <- [LookupField .. AddCurso]]

main :: IO ()
main =
  case filter (/= Ok) allTests of
    [] -> putStrLn "Todos los tests Ok!"
    l  -> sequence_ $ map f l
      where f (Fail m) = putStrLn $ "Fallo en test: " ++ m



instance Eq JSON where
  (JString s) == (JString t)
    = s == t
  (JNumber n) == (JNumber m)
    = n == m
  (JBoolean b) == (JBoolean c)
    = b == c
  JNull == JNull
    = True
  JObject o == JObject p
    = o == p
  JArray r == JArray s
    = r == s
  _ == _ = False
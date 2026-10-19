-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Brian Orta
--  Carnet: 21-10447
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  He usado Gemini para realizar preuntas puntuales y entender la sintaxis
--  de funciones y palabras claves del lenguaje.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)
import Data.Maybe (fromMaybe)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo [_] = True
esPalindromo (x:xs)
  | not (isAlphaNum x) = esPalindromo xs
  | not (isAlphaNum (last xs)) = esPalindromo (x:init xs)
  | otherwise = (toLower x == toLower (last xs)) && esPalindromo (init xs) 

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: 
-- Realiza una llamada recursiva por cada elemento de la lista
-- hasta que llega al ultimo elemento, luego calcula todos los
-- maximos y minimos con los datos obtenidos. 
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor [b] = Just (b, b)
mayorYMenor (x:xs) = Just (max x high, min x low)
  where (high, low) = case mayorYMenor xs of
                      Just result -> result
                      Nothing -> (x, x)
-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | not (isDigit a && isDigit b && isDigit c && isDigit d) = Left "formato inválido"
  | (read ([a] ++ [b]) :: Int) > 23 = Left "hora fuera de rango"
  | (read ([c] ++ [d]) :: Int) > 59 = Left "minutos fuera de rango"
  | otherwise = Right (read ([a] ++ [b]) :: Int, read ([c] ++ [d]) :: Int)
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion a b = 
  case leerHora a of
    Left s -> Left s
    Right (a1, b1) -> case leerHora b of
                        Left t -> Left t
                        Right (a2, b2) -> if time1 > time2 then Right ((time2+1440) - time1) else Right (time2 - time1)
                         where
                          time1 = (a1*60) + b1
                          time2 = (a2*60) + b2

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined

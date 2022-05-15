
module TestLib.Aeson where

import Data.Aeson as A
import Data.Char (isUpper, toLower)
import Data.List as L


baseOptions :: A.Options
baseOptions = A.defaultOptions { A.omitNothingFields = True }

toSnake0, toSnake1, toSnake2, toSnake3, toSnake4, toSnake5 :: A.Options
toSnake0 = baseOptions { A.fieldLabelModifier = toSnake . dropLeadingUnderscore }
toSnake1 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFirstWord . dropLeadingUnderscore }
toSnake2 = baseOptions { A.fieldLabelModifier = toSnakeAndDropTwoWords . dropLeadingUnderscore }
toSnake3 = baseOptions { A.fieldLabelModifier = toSnakeAndDropThreeWords . dropLeadingUnderscore }
toSnake4 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFourWords . dropLeadingUnderscore }
toSnake5 = baseOptions { A.fieldLabelModifier = toSnakeAndDropFiveWords . dropLeadingUnderscore }

toSnakeC0, toSnakeC1, toSnakeC2, toSnakeC3, toSnakeC4, toSnakeC5 :: A.Options
toSnakeC0 = baseOptions { A.constructorTagModifier = toSnake }
toSnakeC1 = baseOptions { A.constructorTagModifier = toSnakeAndDropFirstWord }
toSnakeC2 = baseOptions { A.constructorTagModifier = toSnakeAndDropTwoWords }
toSnakeC3 = baseOptions { A.constructorTagModifier = toSnakeAndDropThreeWords }
toSnakeC4 = baseOptions { A.constructorTagModifier = toSnakeAndDropFourWords }
toSnakeC5 = baseOptions { A.constructorTagModifier = toSnakeAndDropFiveWords }

dropLeadingUnderscore :: [Char] -> [Char]
dropLeadingUnderscore ('_':xs) = xs
dropLeadingUnderscore xs = xs

toSnake :: String -> String
toSnake = fmap toLower . L.concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : fmap ('_':) t

splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case L.break p s' of
      (b', [])     -> [ m:b' ]
      (b', x:xs) -> ( m:b' ) : go x xs
  in case L.break p s of
    (b,  [])    -> [ b ]
    ([], h:t) -> go h t
    (b,  h:t) -> b : go h t

toSnakeAndDropFirstWord :: String -> String
toSnakeAndDropFirstWord = L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropTwoWords :: String -> String
toSnakeAndDropTwoWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropThreeWords :: String -> String
toSnakeAndDropThreeWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropFourWords :: String -> String
toSnakeAndDropFourWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

toSnakeAndDropFiveWords :: String -> String
toSnakeAndDropFiveWords = L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . L.drop 1 . L.dropWhile (/= '_') . toSnake

dropNAndToCamelCaseOptions :: Int -> A.Options
dropNAndToCamelCaseOptions n = A.defaultOptions { A.fieldLabelModifier = dropNAndCamelCase n }

dropNAndCamelCase :: Int -> String -> String
dropNAndCamelCase n = lowercaseFirst . L.drop n

lowercaseFirst :: [Char] -> [Char]
lowercaseFirst (x:xs) = (toLower x) : xs
lowercaseFirst [] = []

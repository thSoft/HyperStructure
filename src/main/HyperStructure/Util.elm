module HyperStructure.Util where

import String (..)
import List (..)
import List

insertAtMiddle : List a -> List a -> List a
insertAtMiddle toInsert original =
  let half = (original |> List.length) // 2
      firstHalf = original |> take half
      secondHalf = original |> drop half
  in [firstHalf, toInsert, secondHalf] |> List.concat

containsIgnoreCase : String -> String -> Bool
containsIgnoreCase haystack needle = haystack |> toLower |> contains (needle |> toLower)
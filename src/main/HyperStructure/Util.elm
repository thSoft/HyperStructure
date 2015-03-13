module HyperStructure.Util where

import Basics (..)
import String (..)
import String
import Regex (..)
import Regex
import List (..)
import List
import Html (..)

insertAtMiddle : List a -> List a -> List a
insertAtMiddle toInsert original =
  let half = (original |> List.length) // 2
      firstHalf = original |> take half
      secondHalf = original |> drop half
  in [firstHalf, toInsert, secondHalf] |> List.concat

fuzzyContains : String -> String -> Bool
fuzzyContains haystack needle =
  needle |> words |> List.all (\word ->
    if word |> String.isEmpty then False else haystack `containsIgnoreCase` word
  )

containsIgnoreCase : String -> String -> Bool
containsIgnoreCase haystack needle = haystack |> Regex.contains (needle |> escape |> regex |> caseInsensitive)

highlightOccurencesOfWords : String -> String -> Html
highlightOccurencesOfWords haystack needle =
  let words = needle |> String.words |> List.filter (\word -> not (word |> String.isEmpty))
      occurences =
        words |> concatMap (\word ->
          haystack |> find All (word |> escape |> regex |> caseInsensitive)
        ) |> List.map (\match ->
          { start = match.index, end = match.index + (match.match |> String.length) - 1 }
        )
      initialPartitions = [{ start = 0, end = (haystack |> String.length) - 1 }]
      partitions =
        occurences |> List.foldr (\occurence currentPartitions ->
          currentPartitions |> concatMap (subdivide occurence)
        ) initialPartitions |> sortBy .start
      children =
        partitions |> List.map (\partition ->
          let subtext = haystack |> slice partition.start (partition.end + 1) |> text
          in if occurences |> member partition then strong [] [subtext] else subtext
        )
  in span [] children

subdivide : IntRange -> IntRange -> List IntRange
subdivide subrange range =
  let first = if range.start < subrange.start then [{ start = range.start, end = min (subrange.start - 1) range.end }] else []
      second = if range.start <= subrange.start && subrange.end <= range.end then [subrange] else []
      third = if subrange.end < range.end then [{ start = max range.start (subrange.end + 1), end = range.end }] else []
  in first ++ second ++ third

-- both inclusive
type alias IntRange = {
  start: Int,
  end: Int
}
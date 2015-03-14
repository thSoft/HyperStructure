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
      matches =
        words |> concatMap (\word ->
          haystack |> find All (word |> escape |> regex |> caseInsensitive)
        )
      appendIndexedCharToSegments (index, char) segments =
        let matching =
              matches |> List.any (\match ->
                match.index <= index && index < match.index + (match.match |> String.length)
              )
            charAsString = char |> fromChar
        in
          if (segments |> List.isEmpty) || ((segments |> head).matching /= matching) then
            { string = charAsString, matching = matching } :: segments
          else
            let lastSegment = segments |> head
                lastSegmentAugmented = { lastSegment | string <- lastSegment.string |> String.append charAsString }
            in lastSegmentAugmented :: (segments |> tail)
      segments = haystack |> toList |> indexedMap (,) |> List.foldr appendIndexedCharToSegments []
      children =
        segments |> List.map (\segment ->
          let segmentText = segment.string |> text
          in if segment.matching then strong [] [segmentText] else segmentText
        )
  in span [] children

type alias Segment = {
  string: String,
  matching: Bool
}
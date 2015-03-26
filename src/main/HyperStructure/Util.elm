module HyperStructure.Util where

import Basics (..)
import String (..)
import String
import Regex (..)
import Regex
import List (..)
import List
import Html (..)
import IntRange (..)
import IntRange

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
      buildSegments index segments =
        let matching =
              matches |> List.any (\match ->
                match.index <= index && index < match.index + (match.match |> String.length)
              )
        in
          if (segments |> List.isEmpty) || ((segments |> head).matching /= matching) then
            let newSegment = { start = index, end = index + 1, matching = matching }
            in newSegment :: segments
          else
            let lastSegment = segments |> head
                lastSegmentContinued = { lastSegment | end <- index + 1 }
            in lastSegmentContinued :: (segments |> tail)
      segments = (0 `to` ((haystack |> String.length) - 1)) |> IntRange.foldl buildSegments [] |> List.reverse
      children =
        segments |> List.map (\segment ->
          let segmentText = haystack |> String.slice segment.start segment.end |> text
          in if segment.matching then strong [] [segmentText] else segmentText
        )
  in span [] children

type alias Segment = {
  start: Int,
  end: Int,
  matching: Bool
}
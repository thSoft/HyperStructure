module Main where

import Signal (..)
import Signal
import HyperStructure.Model (..)
import HyperStructure.View (..)
import Graphics.Collage (..)
import Mouse
import Html (..)
import Html.Attributes (..)

main : Html
main = node |> viewNode

node =
  {
    id = "main expression",
    children = [
      NodeChild {
        node = {
          id = "foo",
          children = [
            TextChild { text = "foo" }
          ],
          relationships = [
            Relationship {
              text = "value",
              node = "42" |> textNode "value of foo"
            }
          ]
        }
      },
      TextChild { text = "+" },
      NodeChild {
        node = {
          id = "subexpression",
          children = [
            NodeChild {
              node = {
                id = "literal",
                children = [
                  TextChild { text = "2" }
                ],
                relationships = []
              }
            },
            TextChild { text = "*" },
            NodeChild {
              node = {
                id = "bar",
                children = [
                  TextChild { text = "bar" }
                ],
                relationships = [
                  Relationship {
                    text = "value",
                    node = "1" |> textNode "value of bar"
                  }
                ]
              }
            }
          ],
          relationships = []
        }
      }
    ],
    relationships = [
      Relationship {
        text = "value",
        node = "44" |> textNode "value of main expression"
      },
      Relationship {
        text = "type",
        node = "number" |> textNode "type of main expression"
      }
    ]
  }

textNode : String -> String -> Node
textNode id text =
  {
    id = id,
    children = [
      TextChild { text = text }
    ],
    relationships = []
  }
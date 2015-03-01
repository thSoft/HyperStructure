module Main where

import Signal (..)
import Signal
import Graphics.Collage (..)
import Mouse
import Html (..)
import Html
import Html.Attributes (..)
import Graphics.Input.Field (..)
import HyperStructure.Model (..)
import HyperStructure.View (..)

main : Signal Html
main = map2 view editorState (constant node)

view : EditorState -> Node -> Html
view editorState node = node |> viewNode editorState

port focus : Signal String
port focus = focusSignal

mainCommands : Channel ()
mainCommands = channel ()

foo : Node
foo =
  {
    id = "foo",
    children = [
      ContentChild { content = "foo" |> text }
    ],
    relationships = [
      Relationship {
        text = "value",
        node = "42" |> textNode "value of foo"
      }
    ],
    commands = [
      Group {
        text = "Show",
        children = [
          Command {
            text = "Value",
            message = (() |> send mainCommands)
          }
        ]
      }
    ]
  }

node : Node
node =
  {
    id = "main expression",
    children = [
      NodeChild {
        node = foo
      },
      ContentChild { content = "+" |> text },
      NodeChild {
        node = {
          id = "subexpression",
          children = [
            NodeChild {
              node = {
                id = "literal",
                children = [
                  ContentChild { content = "2" |> text }
                ],
                relationships = [],
                commands = []
              }
            },
            ContentChild { content = "*" |> text },
            NodeChild {
              node = {
                id = "bar",
                children = [
                  ContentChild { content = "bar" |> text }
                ],
                relationships = [
                  Relationship {
                    text = "value",
                    node = "1" |> textNode "value of bar"
                  }
                ],
                commands = []
              }
            }
          ],
          relationships = [],
          commands = []
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
    ],
    commands = []
  }

textNode : String -> String -> Node
textNode id text =
  {
    id = id,
    children = [
      ContentChild { content = text |> Html.text }
    ],
    relationships = [],
    commands = []
  }
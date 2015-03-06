module Main where

import String (..)
import Signal (..)
import Signal
import List (..)
import List
import Mouse
import Html (..)
import Html
import Html.Attributes (..)
import Graphics.Input.Field (..)
import HyperStructure.Model (..)
import HyperStructure.View (..)
import HyperStructure.Util (..)

main : Signal Html
main = Signal.map2 view (editorState charCodes) (constant node)

view : EditorState -> Node -> Html
view editorState node = node |> viewNode editorState

port charCodes : Signal Int

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
            id = ["Show", "Value"],
            text = "Value",
            message = (ShowValue |> send mainCommands)
          }
        ]
      }
    ],
    commandsWithInput input =
      [
        Command {
          id = ["Rename"],
          text = "Rename to " ++ input,
          message = (Rename |> send mainCommands)
        }
      ] ++ ([
        Group {
          text = "Replace with",
          children =
            methods |> List.map (\method ->
              Command {
                id = ["Replace", method],
                text = method,
                message = (Replace |> send mainCommands)
              }
            )
        }
      ] |> filterCommands input)
  }

methods = ["plus", "minus"]

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
                commands = [], commandsWithInput = always []
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
                commands = [], commandsWithInput = always []
              }
            }
          ],
          relationships = [],
          commands = [], commandsWithInput = always []
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
    commands = [], commandsWithInput = always []
  }

textNode : String -> String -> Node
textNode id text =
  {
    id = id,
    children = [
      ContentChild { content = text |> Html.text }
    ],
    relationships = [],
    commands = [], commandsWithInput = always []
  }

mainCommands : Channel MainCommand
mainCommands = channel Nop

type MainCommand =
  Nop |
  ShowValue |
  Rename |
  Replace
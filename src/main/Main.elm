module Main where

import Signal (..)
import Signal
import List (..)
import List
import Html (..)
import Html
import Html.Attributes (..)
import HyperStructure.Model (..)
import HyperStructure.View (..)

main : Signal Html
main = Signal.map2 view (editorState charCodes model) model

view : EditorState -> Node -> Html
view editorState node = node |> viewNode editorState

port charCodes : Signal Int

model : Signal Node
model = constant node

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
        node = {
          id = "type of main expression",
          children = [
            ContentChild { content = "number" |> Html.text },
            NodeChild { node = "(real)" |> textNode "real" }
          ],
          relationships = [],
          commands = [], commandsWithInput = always []
        }
      }
    ],
    commands = [], commandsWithInput = always []
  }

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
            message = (ShowValue |> send mainCommandChannel)
          }
        ]
      }
    ],
    commandsWithInput input =
      [
        Command {
          id = ["Rename"],
          text = "Rename to: " ++ input,
          message = (Rename |> send mainCommandChannel)
        }
      ] ++ ([
        Group {
          text = "Replace with",
          children =
            methods |> List.map (\method ->
              Command {
                id = ["Replace", method],
                text = method,
                message = (Replace |> send mainCommandChannel)
              }
            )
        }
      ] |> filterCommands input)
  }

methods = ["plus", "minus"]

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

mainCommandChannel : Channel MainCommand
mainCommandChannel = channel Nop

type MainCommand =
  Nop |
  ShowValue |
  Rename |
  Replace
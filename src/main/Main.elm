module Main where

import Signal (..)
import Signal
import List
import Html (..)
import Html
import HyperStructure.Model (..)
import HyperStructure.EditorState (..)
import HyperStructure.View (..)

main : Signal Html
main = Signal.map2 viewNode (editorState charCodes model) model

port charCodes : Signal Int

model : Signal Node
model = constant node

node : Node
node =
  {
    id = "mainExpression",
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
                commands = [],
                commandsWithInput = always []
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
                    node = "1" |> textNode "barValue"
                  }
                ],
                commands = [],
                commandsWithInput = always []
              }
            }
          ],
          relationships = [],
          commands = [],
          commandsWithInput = always []
        }
      }
    ],
    relationships = [
      Relationship {
        text = "value",
        node = "44" |> textNode "mainExpressionValue"
      },
      Relationship {
        text = "type",
        node = {
          id = "mainExpressionType",
          children = [
            ContentChild { content = "number" |> Html.text },
            NodeChild { node = "(real)" |> textNode "real" }
          ],
          relationships = [],
          commands = [],
          commandsWithInput = always []
        }
      }
    ],
    commands = [],
    commandsWithInput = always []
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
        node = "42" |> textNode "fooValue"
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
      ([
        Group {
          text = "Surround with",
          children =
            functions |> List.map (\function ->
              Command {
                id = ["Surround", function],
                text = function,
                message = (Surround |> send mainCommandChannel)
              }
            )
        }
      ] |> filterCommands input) ++ [
        Command {
          id = ["Rename"],
          text = "Rename to: " ++ input,
          message = (Rename |> send mainCommandChannel)
        }
      ]
  }

functions = ["+", "-", "*", "/", "square root"]

textNode : String -> String -> Node
textNode id text =
  {
    id = id,
    children = [
      ContentChild { content = text |> Html.text }
    ],
    relationships = [],
    commands = [],
    commandsWithInput = always []
  }

mainCommandChannel : Channel MainCommand
mainCommandChannel = channel Nop

type MainCommand =
  Nop |
  ShowValue |
  Rename |
  Surround
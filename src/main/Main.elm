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
    content = ChildrenContent [
      foo,
      "+" |> textNode "mainExpressionPlus",
      {
        id = "subexpression",
        content = ChildrenContent [
          "2" |> textNode "literal",
          "*" |> textNode "subExpressionMultiply",
          {
            id = "bar",
            content = HtmlContent ("bar" |> text),
            commands = [],
            commandsWithInput = always []
          }
        ],
        commands = [],
        commandsWithInput = always []
      }
    ],
    commands = [],
    commandsWithInput = always []
  }

foo : Node
foo =
  {
    id = "foo",
    content = HtmlContent ("foo" |> text),
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
    content = HtmlContent (text |> Html.text),
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
module HyperStructure.Model where

import Signal (..)
import Html (..)

type alias Node = {
  id: String,
  children: List Child,
  relationships: List Relationship,
  commands: List Command,
  commandsWithInput: String -> List Command
}

type Child =
  NodeChild {
    node: Node
  } |
  ContentChild {
    content: Html
  }

type Relationship = Relationship {
  text: String,
  node: Node
}

type Command =
  Command {
    text: String,
    message: Message
  } |
  Group {
    text: String,
    children: List Command
  }

type alias EditorState = {
  selection: Selection,
  inputText: String
}

type alias Selection = Maybe Node
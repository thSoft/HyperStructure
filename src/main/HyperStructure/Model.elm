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
  Command CommandInfo |
  Group {
    text: String,
    children: List Command
  }

type alias CommandInfo = {
  id: CommandId,
  text: String,
  message: Message
}

type alias CommandId = List String

type alias EditorState = {
  selection: Selection,
  inputText: String,
  selectedCommandId: Maybe CommandId
}

type alias Selection = Maybe Node
module HyperStructure.Model where

import Signal (..)
import Html (..)

type alias Node = {
  id: String,
  content: Content,
  relationships: List Relationship,
  commands: List Command,
  commandsWithInput: String -> List Command
}

type Content =
  HtmlContent Html |
  ChildrenContent (List Node)

type Relationship = Relationship {
  text: String,
  node: Node
}

type Command =
  Command CommandInfo |
  Group GroupInfo

type alias CommandInfo = {
  id: CommandId,
  text: String,
  message: Message
}

type alias CommandId = List String

type alias GroupInfo = {
  text: String,
  children: List Command
}

type alias EditorState = {
  selection: Selection,
  inputText: String,
  selectedCommandId: Maybe CommandId
}

type alias Selection = Maybe Node
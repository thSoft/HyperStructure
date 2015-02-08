module HyperStructure.Model where

type alias Node = {
  id: String,
  children: List Child,
  relationships: List Relationship
}

type Child =
  NodeChild {
    node: Node
  } |
  TextChild {
    text: String
  }

type Relationship = Relationship {
  text: String,
  node: Node
}
module HyperStructure.View where

import List (..)
import List
import Html (..)
import Html
import Html.Attributes (..)
import Html.Attributes as Attr
import Html.Events (..)
import Svg (..)
import Svg
import Svg.Attributes as SvgAttr
import HyperStructure.Model (..)
import Signal (..)
import Signal
import Json.Decode (..)
import Json.Decode
import Mouse

viewNode : EditorState -> Node -> Html
viewNode editorState node =
  let childNodes = node.children |> List.map (viewChild editorState)
      relatedNodes = node |> getAllRelationships |> List.map (viewRelationship editorState)
      menu = node |> viewMenu editorState
  in
    div [
      class "node",
      onClick (Select Nothing |> send editorCommands)
    ] ([
      div (node |> attributes editorState) (childNodes ++ menu),
      div [class "relationships"] relatedNodes
    ])

viewMenu : EditorState -> Node -> List Html
viewMenu editorState node =
  [
    menu [
      id (node |> menuId),
      attribute "type" "context"
    ] (node.commands |> List.map viewCommand)
  ]

menuId : Node -> String
menuId node = node.id ++ "menu"

viewCommand : Command -> Html
viewCommand command =
  case command of
    Command { text, message } ->
      menuitem [
        onClick message,
        attribute "label" text
      ] []
    Group { text, children } ->
      menu [
        attribute "label" text
      ] (children |> List.map viewCommand)

attributes : EditorState -> Node -> List Html.Attribute
attributes editorState node =
  [
    id node.id,
    classList [
      ("children", True),
      ("selected", node |> isSelected editorState)
    ],
    onClick (Select (Just node) |> send editorCommands),
    attribute "contextmenu" (node |> menuId)
  ]

isSelected : EditorState -> Node -> Bool
isSelected editorState node =
  case editorState.selection of
    Nothing -> False
    Just selectedNode -> node == selectedNode

viewChild : EditorState -> Child -> Html
viewChild editorState child =
  case child of
    ContentChild { content } -> content
    NodeChild { node } ->
      let children = node.children |> List.map (viewChild editorState)
          menu = node |> viewMenu editorState 
      in span (node |> attributes editorState) (children ++ menu)

viewRelationship : EditorState -> (Node, Relationship) -> Html
viewRelationship editorState (originalNode, relationship) =
  case relationship of 
    Relationship { text, node } ->
      span [] [
        Svg.svg [] [
          g [attribute "data-source" originalNode.id, attribute "data-target" node.id] [
            line [SvgAttr.markerStart "url(#markerStart)", SvgAttr.markerEnd "url(#markerEnd)"] [],
            Svg.text [SvgAttr.dominantBaseline "middle"] [text |> Html.text]
          ]
        ],
        node |> viewNode editorState
      ]

getAllRelationships : Node -> List (Node, Relationship)
getAllRelationships node =
  let ownRelationships = node.relationships |> List.map ((,) node)
      childRelationships = node.children |> List.map getChildRelationships
      getChildRelationships child =
        case child of
          ContentChild _ -> []
          NodeChild { node } -> node |> getAllRelationships
  in childRelationships |> insertAtMiddle [ownRelationships] |> concat

insertAtMiddle : List a -> List a -> List a
insertAtMiddle toInsert original =
  let half = (original |> length) // 2
      firstHalf = original |> take half
      secondHalf = original |> drop half
  in [firstHalf, toInsert, secondHalf] |> concat  

type EditorCommand =
  Nop |
  Select (Maybe Node)

editorCommands : Channel EditorCommand
editorCommands = channel Nop

editorState : Signal EditorState
editorState = foldp updateEditorState initialEditorState (editorCommands |> subscribe)

updateEditorState : EditorCommand -> EditorState -> EditorState
updateEditorState editorCommand editorState =
  case editorCommand of
    Nop -> editorState
    Select newSelection -> { editorState | selection <- newSelection }

isJust : Maybe a -> Bool
isJust maybe =
  case maybe of
    Just _ -> True
    Nothing -> False

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    inputText = ""
  }
module HyperStructure.View where

import List (..)
import List
import Maybe (..)
import Maybe
import Html (..)
import Html
import Html.Attributes (..)
import Html.Attributes as Attr
import Html.Events (..)
import Svg (..)
import Svg
import Svg.Attributes as SvgAttr
import Signal (..)
import Signal
import Json.Decode (..)
import Json.Decode
import Mouse
import Char (..)
import String (..)
import String
import HyperStructure.Model (..)

viewNode : EditorState -> Node -> Html
viewNode editorState node =
  let relatedNodes = node |> getAllRelationships |> List.map (viewRelationship editorState)
  in
    div [
      class "node",
      onClick (Select Nothing |> send editorCommands)
    ] [
      node |> viewChildren editorState,
      div [class "relationships"] relatedNodes
    ]

viewChildren : EditorState -> Node -> Html
viewChildren editorState node =
  let viewChild child =
        case child of
          ContentChild { content } -> content
          NodeChild nodeChild -> nodeChild.node |> viewChildren editorState
      children = node.children |> List.map viewChild
      menu = node |> viewMenu editorState
      onInput string = Type { input = string, first = False} |> send editorCommands
      onKeyPress key =
        let editorCommand = if key == "Esc" then Type { input = "", first = False } else Nop
        in editorCommand |> send editorCommands
      inputField =
        if (editorState.selection == Just node) && not (editorState.inputText |> String.isEmpty) then
          [
            input [
              id inputFieldId,
              Attr.value editorState.inputText,
              on "input" targetValue onInput,
              on "keypress" ("key" := string) onKeyPress
            ] []
          ]
        else []
  in span (node |> attributes editorState) (children ++ menu ++ inputField)

inputFieldId = "commandInput"

attributes : EditorState -> Node -> List Html.Attribute
attributes editorState node =
  let selected =
        case editorState.selection of
          Nothing -> False
          Just selectedNode -> node == selectedNode
      onKeyPress key =
        let editorCommand =
              if ((key |> String.length) == 1) && (editorState.inputText |> String.isEmpty) then
                Type { input = key, first = True }
              else Nop
        in editorCommand |> send editorCommands
  in
    [
      id node.id,
      classList [
        ("children", True),
        ("selected", selected)
      ],
      on "keypress" ("key" := string) onKeyPress,
      onClick (Select (Just node) |> send editorCommands),
      tabindex 0,
      attribute "contextmenu" (node |> menuId)
    ]

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
  in childRelationships |> insertAtMiddle [ownRelationships] |> List.concat

insertAtMiddle : List a -> List a -> List a
insertAtMiddle toInsert original =
  let half = (original |> List.length) // 2
      firstHalf = original |> take half
      secondHalf = original |> drop half
  in [firstHalf, toInsert, secondHalf] |> List.concat

type EditorCommand =
  Nop |
  Select (Maybe Node) |
  Type {
    input: String,
    first: Bool
  }

editorCommands : Channel EditorCommand
editorCommands = channel Nop

editorState : Signal EditorState
editorState = foldp updateEditorState initialEditorState (editorCommands |> subscribe)

updateEditorState : EditorCommand -> EditorState -> EditorState
updateEditorState editorCommand editorState =
  case editorCommand of
    Nop -> editorState
    Select newSelection -> { editorState | selection <- newSelection, inputText <- "" }
    Type { input } -> { editorState | inputText <- input }

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    inputText = ""
  }

focusSignal : Signal String
focusSignal =
  let toFocus editorCommand currentEditorState =
        case editorCommand of
          Type { input, first } ->
            if first then inputFieldId
            else if input |> String.isEmpty then currentEditorState.selection |> Maybe.map .id |> withDefault ""
            else ""
          _ -> ""
  in Signal.map2 toFocus (editorCommands |> subscribe) editorState
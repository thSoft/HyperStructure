module HyperStructure.View where

import List (..)
import List
import Html (..)
import Html
import Html.Attributes (..)
import Html.Events (..)
import Svg (..)
import Svg
import Svg.Attributes as SvgAttr
import HyperStructure.Model (..)
import Signal (..)

viewNode : EditorState -> Node -> Html
viewNode editorState node =
  let childNodes = node.children |> List.map (viewChild editorState)
      relatedNodes = node |> getAllRelationships |> List.map (viewRelationship editorState)
  in
    div [
      class "node",
      onClick (Select Nothing |> send editorCommands)
    ][
      div (node |> attributes editorState) childNodes,
      div [class "relationships"] relatedNodes
    ]

attributes : EditorState -> Node -> List Html.Attribute
attributes editorState node =
  [
    id node.id,
    classList [
      ("children", True),
      ("selected", node |> isSelected editorState)
    ],
    onClick (Select (Just node) |> send editorCommands)
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
      span (node |> attributes editorState) (node.children |> List.map (viewChild editorState))

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

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    menuActivated = False,
    inputText = ""
  }
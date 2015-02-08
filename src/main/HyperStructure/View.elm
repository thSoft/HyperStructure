module HyperStructure.View where

import List (..)
import List
import Html (..)
import Html
import Html.Attributes (..)
import Svg (..)
import Svg
import Svg.Attributes as SvgAttr
import HyperStructure.Model (..)

viewNode : Node -> Html
viewNode node =
  let childNodes = node.children |> map viewChild
      relatedNodes = node |> getAllRelationships |> map viewRelationship
  in
    div [class "node"] [
      div [id node.id, class "children"] childNodes,
      div [class "relationships"] relatedNodes
    ]

viewChild : Child -> Html
viewChild child =
  case child of
    TextChild { text } -> text |> Html.text
    NodeChild { node } -> span [id node.id] (node.children |> map viewChild)

viewRelationship : (Node, Relationship) -> Html
viewRelationship (originalNode, relationship) =
  case relationship of 
    Relationship { text, node } ->
      span [] [
        Svg.svg [] [
          g [attribute "data-source" originalNode.id, attribute "data-target" node.id] [
            line [SvgAttr.markerStart "url(#markerStart)", SvgAttr.markerEnd "url(#markerEnd)"] [],
            Svg.text [SvgAttr.dominantBaseline "middle"] [text |> Html.text]
          ]
        ],
        node |> viewNode
      ]

getAllRelationships : Node -> List (Node, Relationship)
getAllRelationships node =
  let ownRelationships = node.relationships |> map ((,) node)
      childRelationships = node.children |> map getChildRelationships
      getChildRelationships child =
        case child of
          TextChild _ -> []
          NodeChild { node } -> node |> getAllRelationships
  in childRelationships |> insertAtMiddle [ownRelationships] |> concat

insertAtMiddle : List a -> List a -> List a
insertAtMiddle toInsert original =
  let half = (original |> length) // 2
      firstHalf = original |> take half
      secondHalf = original |> drop half
  in [firstHalf, toInsert, secondHalf] |> concat
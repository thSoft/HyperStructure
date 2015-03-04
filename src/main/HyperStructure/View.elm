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
  let children =
        node.children |> List.map (\child ->
          case child of
            ContentChild { content } -> content
            NodeChild nodeChild -> nodeChild.node |> viewChildren editorState
        )
      menu = node |> viewMenu editorState
      onInput string = Type { input = string, first = False } |> send editorCommands
      commandsWithInput = editorState.inputText |> node.commandsWithInput
      onEnter = editorState.inputText |> node.commandsWithInput |> findCommandInfo editorState.selectedCommandId |> Maybe.map .message |> withDefault (Nop |> send editorCommands)
      onKeyPress key =
        case key of
          "Esc" -> Type { input = "", first = False } |> send editorCommands
          "Down" -> SelectCommand (moveCommandSelectionBy editorState commandsWithInput 1) |> send editorCommands
          "Up" -> SelectCommand (moveCommandSelectionBy editorState commandsWithInput -1) |> send editorCommands
          "Enter" -> onEnter
          _ -> Nop |> send editorCommands
      commandsWithInputView =
        if (editorState.selection == Just node) && not (editorState.inputText |> String.isEmpty) then
          [
            div [
              class "commandsWithInput",
              on "keypress" ("key" := string) onKeyPress
            ] [
              input [
                id inputFieldId,
                Attr.value editorState.inputText,
                on "input" targetValue onInput
              ] [],
              span [
                class "commands"
              ] (commandsWithInput |> List.map (viewCommandWithInput editorState))
            ]
          ]
        else []
  in span (node |> attributes editorState) (children ++ menu ++ commandsWithInputView)

moveCommandSelectionBy : EditorState -> List Command -> Int -> Maybe CommandId
moveCommandSelectionBy editorState commandsWithInput offset =
  let commandInfosWithIndex = commandsWithInput |> collectCommandInfos |> indexedMap (,)
      stepIndex index = (index + offset) % (commandInfosWithIndex |> List.length)
  in commandInfosWithIndex |> indexOf editorState.selectedCommandId |> stepIndex |> findCommandIdByIndex commandInfosWithIndex

collectCommandInfos : List Command -> List CommandInfo
collectCommandInfos commands =
  commands |> concatMap (\command ->
    case command of
      Command info -> [info]
      Group { children } -> children |> collectCommandInfos
  )

indexOf : Maybe CommandId -> List (Int, CommandInfo) -> Int 
indexOf maybeCommandId commandInfosWithIndex =
  case maybeCommandId of
    Nothing -> 0
    Just commandId ->
      commandInfosWithIndex |> List.map (\indexed ->
        if (indexed |> snd).id == commandId then Just (indexed |> fst) else Nothing
      ) |> Maybe.oneOf |> withDefault 0

findCommandIdByIndex : List (Int, CommandInfo) -> Int -> Maybe CommandId
findCommandIdByIndex commandInfosWithIndex index =
  commandInfosWithIndex |> List.map (\indexed ->
    if (indexed |> fst) == index then Just (indexed |> snd).id else Nothing
  ) |> Maybe.oneOf

inputFieldId = "commandInput"

findCommandInfo : Maybe CommandId -> List Command -> Maybe CommandInfo
findCommandInfo maybeCommandId commands =
  maybeCommandId `Maybe.andThen` (\commandId ->
    commands |> List.map (\command ->
      case command of
        Command info -> if info.id == commandId then Just info else Nothing
        Group { children } -> children |> findCommandInfo (Just commandId)
    ) |> Maybe.oneOf
  )

safeHead : List a -> Maybe a
safeHead list = if list |> List.isEmpty then Nothing else Just (list |> head)

attributes : EditorState -> Node -> List Html.Attribute
attributes editorState node =
  let selected = editorState.selection == Just node
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

viewCommandWithInput : EditorState -> Command -> Html
viewCommandWithInput editorState command =
  case command of
    Command { id, text, message } ->
      let selected = editorState.selectedCommandId == Just id
      in
        div [
          onClick message,
          classList [
            ("command", True),
            ("selected", selected)
          ]
        ] [Html.text text]
    Group { text, children } ->
      let caption = span [class "caption"] [text |> Html.text]
          childrenView = children |> List.map (viewCommandWithInput editorState)
      in div [class "group"] ([caption] ++ childrenView)

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
  } |
  SelectCommand (Maybe CommandId)

editorCommands : Channel EditorCommand
editorCommands = channel Nop

editorState : Signal EditorState
editorState = foldp updateEditorState initialEditorState (editorCommands |> subscribe)

updateEditorState : EditorCommand -> EditorState -> EditorState
updateEditorState editorCommand editorState =
  case editorCommand of
    Nop -> editorState
    Select newSelection -> { editorState | selection <- newSelection, inputText <- "" }
    Type { input, first } ->
      let newSelectedCommandId =
            if first then
              editorState.selection `Maybe.andThen` (\node -> input |> node.commandsWithInput |> firstCommandId)
            else editorState.selectedCommandId
      in
        { editorState |
          inputText <- input,
          selectedCommandId <- newSelectedCommandId
        }
    SelectCommand newSelectedCommandId -> { editorState | selectedCommandId <- newSelectedCommandId }

firstCommandId : List Command -> Maybe CommandId
firstCommandId commands =
  let getId command =
        case command of
          Command { id } -> Just id
          Group { children } -> children |> firstCommandId
  in (commands |> safeHead) `Maybe.andThen` getId

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    inputText = "",
    selectedCommandId = Nothing
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
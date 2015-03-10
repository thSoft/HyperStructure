module HyperStructure.View where
-- TODO split into modules

import Basics
import Array (..)
import Array
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
import Keyboard
import Char (..)
import String (..)
import String
import HyperStructure.Model (..)
import HyperStructure.Util (..)

viewNode : EditorState -> Node -> Html
viewNode editorState node =
  div [
    class "node",
    onClick (Select Nothing |> send editorCommandChannel)
  ] [
    node |> viewChildren editorState,
    node |> viewRelationships editorState
  ]

viewChildren : EditorState -> Node -> Html
viewChildren editorState node =
  let selected = editorState.selection == Just node
      children =
        node.children |> List.map (\child ->
          case child of
            ContentChild { content } -> content
            NodeChild nodeChild -> nodeChild.node |> viewChildren editorState
        )
      contextMenu = node |> viewContextMenu editorState
      keyboardMenu = node |> viewKeyboardMenu editorState
  in
    span [
      id node.id,
      classList [
        ("children", True),
        ("selected", selected)
      ],
      onClick (Select (Just node) |> send editorCommandChannel),
      attribute "contextmenu" (node |> menuId)
    ] (children ++ contextMenu ++ keyboardMenu)

viewRelationships : EditorState -> Node -> Html
viewRelationships editorState node =
  let relatedNodes = node |> getAllRelationships |> List.map (viewRelationship editorState)
  in div [class "relationships"] relatedNodes

getAllRelationships : Node -> List (Node, Relationship)
getAllRelationships node =
  let ownRelationships = node.relationships |> List.map ((,) node)
      childRelationships =
        node.children |> List.map (\child ->
          case child of
            ContentChild _ -> []
            NodeChild { node } -> node |> getAllRelationships
        )
  in childRelationships |> insertAtMiddle [ownRelationships] |> List.concat

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

-- Context menu

viewContextMenu : EditorState -> Node -> List Html
viewContextMenu editorState node =
  [
    menu [
      id (node |> menuId),
      attribute "type" "context"
    ] (node.commands |> List.map viewContextMenuItem)
  ]

menuId : Node -> String
menuId node = node.id ++ "menu"

viewContextMenuItem : Command -> Html
viewContextMenuItem command =
  case command of
    Command { text, message } ->
      menuitem [
        onClick message,
        attribute "label" text
      ] []
    Group { text, children } ->
      menu [
        attribute "label" text
      ] (children |> List.map viewContextMenuItem)

-- Keyboard menu

viewKeyboardMenu : EditorState -> Node -> List Html
viewKeyboardMenu editorState node =
  if (editorState.selection == Just node) && not (editorState.inputText |> String.isEmpty) then
    let allCommands = node |> getAllCommands editorState.inputText
        handleInput string = Type string |> send editorCommandChannel
        handleKeyDown keyCode =
          case keyCode of
            40 -> SelectCommand (moveCommandSelectionBy editorState allCommands 1) |> send editorCommandChannel
            38 -> SelectCommand (moveCommandSelectionBy editorState allCommands -1) |> send editorCommandChannel
            13 -> allCommands |> findCommandInfo editorState.selectedCommandId |> Maybe.map .message |> withDefault (Nop |> send editorCommandChannel) -- TODO also Type "" |> send editorCommandChannel
            _ -> Nop |> send editorCommandChannel
        handleKeyUp keyCode =
          case keyCode of
            27 -> Type "" |> send editorCommandChannel
            13 -> Type "" |> send editorCommandChannel
            _ -> Nop |> send editorCommandChannel
        keyboardMenuItems = allCommands |> List.map (viewKeyboardMenuItem editorState)
    in
      [
        Html.node "dialog" [
          attribute "open" "open",
          class "keyboardMenu"
        ] [
          input [
            id "commandInput",
            Attr.value editorState.inputText,
            autofocus True,
            on "input" targetValue handleInput,
            onKeyDown handleKeyDown,
            onKeyUp handleKeyUp
          ] [],
          span [
            class "commands"
          ] keyboardMenuItems
        ]
      ]
  else []

getAllCommands : String -> Node -> List Command
getAllCommands inputText node =
  let commandsWithInput = inputText |> node.commandsWithInput
      filteredCommands = node.commands |> filterCommands inputText
  in commandsWithInput ++ filteredCommands

moveCommandSelectionBy : EditorState -> List Command -> Int -> Maybe CommandId
moveCommandSelectionBy editorState commandsWithInput offset =
  let commandInfosWithIndex = commandsWithInput |> collectCommandInfos |> List.indexedMap (,)
      index = commandInfosWithIndex |> indexOfCommandId editorState.selectedCommandId
      newIndex = (index + offset) % (commandInfosWithIndex |> List.length)
  in commandInfosWithIndex |> findCommandIdByIndex newIndex

collectCommandInfos : List Command -> List CommandInfo
collectCommandInfos commands =
  commands |> concatMap (\command ->
    case command of
      Command info -> [info]
      Group { children } -> children |> collectCommandInfos
  )

indexOfCommandId : Maybe CommandId -> List (Int, CommandInfo) -> Int 
indexOfCommandId maybeCommandId commandInfosWithIndex =
  case maybeCommandId of
    Nothing -> 0
    Just commandId ->
      commandInfosWithIndex |> List.map (\indexed ->
        if (indexed |> snd).id == commandId then Just (indexed |> fst) else Nothing
      ) |> Maybe.oneOf |> withDefault 0

findCommandIdByIndex : Int -> List (Int, CommandInfo) -> Maybe CommandId
findCommandIdByIndex index commandInfosWithIndex =
  commandInfosWithIndex |> List.map (\indexed ->
    if (indexed |> fst) == index then Just (indexed |> snd).id else Nothing
  ) |> Maybe.oneOf

findCommandInfo : Maybe CommandId -> List Command -> Maybe CommandInfo
findCommandInfo maybeCommandId commands =
  maybeCommandId `Maybe.andThen` (\commandId ->
    commands |> List.map (\command ->
      case command of
        Command info -> if info.id == commandId then Just info else Nothing
        Group { children } -> children |> findCommandInfo (Just commandId)
    ) |> Maybe.oneOf
  )

filterCommands : String -> List Command -> List Command
filterCommands searchTerm commands =
  commands |> concatMap (\command ->
    case command of
      Command { text } -> if text `containsIgnoreCase` searchTerm then [command] else [] -- TODO fuzzy contains
      Group { text, children } -> 
        let filteredChildren = children |> filterCommands searchTerm
        in if filteredChildren |> List.isEmpty then [] else [Group { text = text, children = filteredChildren }]
  )

viewKeyboardMenuItem : EditorState -> Command -> Html
viewKeyboardMenuItem editorState command =
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
        ] [text |> Html.text]
    Group { text, children } ->
      let caption = span [class "caption"] [text |> Html.text]
          childrenView = children |> List.map (viewKeyboardMenuItem editorState)
      in div [class "group"] ([caption] ++ childrenView)

-- Editor state

type EditorCommand =
  Nop |
  Select (Maybe Node) |
  SelectFirstRelatedNode |
  SelectFirstChild |
  SelectParent |
  SelectNext |
  SelectPrevious |
  KeyPress Char |
  Type String |
  SelectCommand (Maybe CommandId)

editorState : Signal Int -> Signal Node -> Signal EditorState
editorState charCodes modelRoots =
  let updates = Signal.map2 (,) (editorCommands charCodes) modelRoots
  in foldp updateEditorState initialEditorState updates

updateEditorState : (EditorCommand, Node) -> EditorState -> EditorState
updateEditorState (editorCommand, modelRoot) editorState =
  case editorCommand of
    Nop -> editorState
    Select newSelection -> { editorState | selection <- newSelection, inputText <- "" }
    SelectFirstRelatedNode ->
      if editorState.selection |> isJust then editorState |> select editorState.selection 0 .relationships getRelationshipNode
      else { editorState | selection <- Just modelRoot }
    SelectFirstChild ->
      if editorState.selection |> isJust then editorState |> select editorState.selection 0 nodeChildren getChildNode
      else { editorState | selection <- Just modelRoot }
    SelectParent ->
      if editorState.inputText |> String.isEmpty then
        case editorState.selection of
          Just selectedNode -> { editorState | selection <- selectedNode |> findParent modelRoot }
          Nothing -> editorState
      else editorState
    SelectNext -> editorState |> moveSelectionBy 1 modelRoot
    SelectPrevious -> editorState |> moveSelectionBy -1 modelRoot
    KeyPress char ->
      if editorState.inputText |> String.isEmpty then
        let newInputText = char |> fromChar
        in
          { editorState |
            inputText <- newInputText,
            selectedCommandId <- editorState.selection `Maybe.andThen` (\selectedNode -> selectedNode |> getAllCommands newInputText |> firstCommandId)
          }
      else editorState
    Type input -> { editorState | inputText <- input }
    SelectCommand newSelectedCommandId -> { editorState | selectedCommandId <- newSelectedCommandId }

findParent : Node -> Node -> Maybe Node -- TODO optimize with zippers?
findParent potentialParent selectedNode =
  let found =
        potentialParent.children |> List.any (\child ->
          case child of
            NodeChild { node } -> node == selectedNode
            _ -> False
        )
  in
    if found then Just potentialParent
    else
      let childResults =
            potentialParent.children |> List.map (\child ->
              case child of
                NodeChild { node } -> selectedNode |> findParent node
                _ -> Nothing
            )
          relationshipResults =
            potentialParent.relationships |> List.map (\relationship ->
              case relationship of
                Relationship { node } -> selectedNode |> findParent node
            )
      in (childResults ++ relationshipResults) |> Maybe.oneOf

nodeChildren : Node -> List Child
nodeChildren node =
  node.children |> List.filter (\child ->
    case child of
      NodeChild _ -> True
      _ -> False
  )

getRelationshipNode : Relationship -> Node
getRelationshipNode relationship = case relationship of Relationship { node } -> node

getChildNode : Child -> Node
getChildNode child = case child of NodeChild { node } -> node

select : Maybe Node -> Int -> (Node -> List a) -> (a -> Node) -> EditorState -> EditorState
select maybeParent index getTargets getNode editorState =
  if editorState.inputText |> String.isEmpty then
    let newSelection =
          maybeParent `Maybe.andThen` (\selectedNode ->
            let targets = selectedNode |> getTargets
            in
              if targets |> List.isEmpty then Just selectedNode
              else targets |> Array.fromList |> get index |> Maybe.map getNode
          )
    in { editorState | selection <- newSelection }
  else editorState

indexOfNode : Node -> (a -> Node) -> List a -> Int 
indexOfNode node getNode nodes =
  nodes |> List.indexedMap (\index current ->
    if (current |> getNode) == node then Just index else Nothing
  ) |> Maybe.oneOf |> withDefault 0

moveSelectionBy : Int -> Node -> EditorState -> EditorState
moveSelectionBy offset modelRoot editorState =
  case editorState.selection of
  Just selectedNode ->
    let maybeParent = selectedNode |> findParent modelRoot
    in
      case maybeParent of
        Just parent ->
          let index = parent |> nodeChildren |> indexOfNode selectedNode getChildNode
              newIndex = index + offset |> Basics.max 0 |> Basics.min ((parent |> nodeChildren |> List.length) - 1)
          in editorState |> select maybeParent newIndex nodeChildren getChildNode
        Nothing -> editorState
  Nothing -> editorState

firstCommandId : List Command -> Maybe CommandId
firstCommandId commands = commands |> collectCommandInfos |> List.indexedMap (,) |> findCommandIdByIndex 0

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    inputText = "",
    selectedCommandId = Nothing
  }

editorCommands : Signal Int -> Signal EditorCommand
editorCommands charCodes =
  Signal.mergeMany [editorCommandChannel |> subscribe, printingCharacterCommands charCodes, controlCharacterCommands]

editorCommandChannel : Channel EditorCommand
editorCommandChannel = channel Nop

printingCharacterCommands : Signal Int -> Signal EditorCommand
printingCharacterCommands charCodes =
  charCodes |> Signal.map (\charCode ->
    if (charCode >= 32) then KeyPress (charCode |> fromCode) else Nop
  )

controlCharacterCommands : Signal EditorCommand
controlCharacterCommands =
  Keyboard.lastPressed |> Signal.map (\keyCode ->
    case keyCode of
      9 -> SelectFirstRelatedNode
      13 -> SelectFirstChild
      27 -> SelectParent
      39 -> SelectNext
      40 -> SelectNext
      37 -> SelectPrevious
      38 -> SelectPrevious
      _ -> Nop
  )
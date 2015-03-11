module HyperStructure.EditorState where

import Basics (..)
import Array (..)
import Array
import List (..)
import List
import Maybe (..)
import Maybe
import Signal (..)
import Signal
import Keyboard
import Char (..)
import String (..)
import String
import HyperStructure.Model (..)
import HyperStructure.Util (..)

editorState : Signal Int -> Signal Node -> Signal EditorState
editorState charCodes modelRoots =
  let updates = Signal.map2 (,) (editorCommands charCodes) modelRoots
  in foldp updateEditorState initialEditorState updates

updateEditorState : (EditorCommand, Node) -> EditorState -> EditorState
updateEditorState (editorCommand, modelRoot) editorState = editorCommand modelRoot editorState

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
editorCommandChannel = channel nop

printingCharacterCommands : Signal Int -> Signal EditorCommand
printingCharacterCommands charCodes =
  charCodes |> Signal.map (\charCode ->
    if (charCode >= 32) then enterCharacter (charCode |> fromCode) else nop
  )

controlCharacterCommands : Signal EditorCommand
controlCharacterCommands =
  Keyboard.lastPressed |> Signal.map (\keyCode ->
    case keyCode of
      9 -> selectFirstRelatedNode
      13 -> selectFirstChild
      27 -> selectParent
      39 -> selectNext
      40 -> selectNext
      37 -> selectPrevious
      38 -> selectPrevious
      _ -> nop
  )

type alias EditorCommand = Node -> EditorState -> EditorState

nop : EditorCommand
nop modelRoot editorState = editorState

select : Selection -> EditorCommand
select newSelection modelRoot editorState = { editorState | selection <- newSelection, inputText <- "" }

selectFirstRelatedNode : EditorCommand
selectFirstRelatedNode modelRoot editorState =
  editorState.selection |> Maybe.map (\selectedNode ->
    editorState |> selectNodeAt (selectedNode |> getRelationshipNodes) 0
  ) |> withDefault { editorState | selection <- Just modelRoot }

selectFirstChild : EditorCommand
selectFirstChild modelRoot editorState = 
  editorState.selection |> Maybe.map (\selectedNode ->
    editorState |> selectNodeAt (selectedNode |> getChildNodes) 0
  ) |> withDefault { editorState | selection <- Just modelRoot }

selectParent : EditorCommand
selectParent modelRoot editorState =
  if editorState.inputText |> String.isEmpty then
    case editorState.selection of
      Just selectedNode -> { editorState | selection <- selectedNode |> findParent modelRoot }
      Nothing -> editorState
  else editorState

selectNext : EditorCommand
selectNext modelRoot editorState = editorState |> moveSelectionBy 1 modelRoot

selectPrevious : EditorCommand
selectPrevious modelRoot editorState = editorState |> moveSelectionBy -1 modelRoot

enterCharacter : Char -> EditorCommand
enterCharacter char modelRoot editorState =
  if editorState.inputText |> String.isEmpty then
    let newInputText = char |> fromChar
    in
      { editorState |
        inputText <- newInputText,
        selectedCommandId <- editorState.selection `Maybe.andThen` (\selectedNode -> selectedNode |> getAllCommands newInputText |> firstCommandId)
      }
  else editorState

changeInputText : String -> EditorCommand
changeInputText newInputText modelRoot editorState = { editorState | inputText <- newInputText }

selectCommand : Maybe CommandId -> EditorCommand
selectCommand newSelectedCommandId modelRoot editorState = { editorState | selectedCommandId <- newSelectedCommandId }

getAllCommands : String -> Node -> List Command
getAllCommands inputText node =
  let commandsWithInput = inputText |> node.commandsWithInput
      filteredCommands = node.commands |> filterCommands inputText
  in commandsWithInput ++ filteredCommands

filterCommands : String -> List Command -> List Command
filterCommands searchTerm commands =
  commands |> concatMap (\command ->
    case command of
      Command { text } -> if text `containsIgnoreCase` searchTerm then [command] else [] -- TODO fuzzy contains
      Group { text, children } -> 
        let filteredChildren = children |> filterCommands searchTerm
        in if filteredChildren |> List.isEmpty then [] else [Group { text = text, children = filteredChildren }]
  )

findParent : Node -> Node -> Maybe Node -- TODO optimize with zippers?
findParent potentialParent selectedNode =
  let foundAsChild =
        potentialParent.children |> List.any (\child ->
          case child of
            NodeChild { node } -> node == selectedNode
            _ -> False
        )
      foundAsRelationship =
        potentialParent.relationships |> List.any (\relationship ->
          case relationship of
            Relationship { node } -> node == selectedNode
        )
  in
    if foundAsChild || foundAsRelationship then Just potentialParent
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

getRelationshipNodes : Node -> List Node
getRelationshipNodes parentNode =
  parentNode.relationships |> List.concatMap (\relationship ->
    case relationship of
      Relationship { node } -> [node]
      _ -> []
  )

getChildNodes : Node -> List Node
getChildNodes parentNode =
  parentNode.children |> List.concatMap (\child ->
    case child of
      NodeChild { node } -> [node]
      _ -> []
  )

selectNodeAt : List Node -> Int -> EditorState -> EditorState
selectNodeAt nodes index editorState =
  if editorState.inputText |> String.isEmpty then
    let newSelection =
          if nodes |> List.isEmpty then editorState.selection
          else nodes |> Array.fromList |> get index
    in { editorState | selection <- newSelection }
  else editorState

indexOfNode : List Node -> Node -> Int 
indexOfNode nodes node =
  nodes |> List.indexedMap (\index current ->
    if current == node then Just index else Nothing
  ) |> Maybe.oneOf |> withDefault 0

moveSelectionBy : Int -> Node -> EditorState -> EditorState
moveSelectionBy offset modelRoot editorState =
  case editorState.selection of
    Just selectedNode ->
      let maybeParent = selectedNode |> findParent modelRoot
      in
        case maybeParent of
          Just parent ->
            let nodesOnSameLevel = parent |> getChildNodes
                index = selectedNode |> indexOfNode nodesOnSameLevel
                newIndex = index + offset |> max 0 |> min ((nodesOnSameLevel |> List.length) - 1)
            in editorState |> selectNodeAt nodesOnSameLevel newIndex
          Nothing -> editorState
    Nothing -> editorState

firstCommandId : List Command -> Maybe CommandId
firstCommandId commands = commands |> collectCommandInfos |> List.indexedMap (,) |> findCommandIdByIndex 0

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
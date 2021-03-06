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

type alias EditorCommand = Node -> EditorState -> EditorState

initialEditorState : EditorState
initialEditorState =
  {
    selection = Nothing,
    inputText = "",
    selectedCommandId = Nothing
  }

editorCommands : Signal Int -> Signal EditorCommand
editorCommands charCodes =
  mergeMany [
    editorCommandChannel |> subscribe,
    printingCharacterCommands charCodes,
    controlCharacterCommands
  ]

editorCommandChannel : Channel EditorCommand
editorCommandChannel = channel nop

printingCharacterCommands : Signal Int -> Signal EditorCommand
printingCharacterCommands charCodes =
  charCodes |> Signal.map (\charCode ->
    if (charCode >= 32) then enterCharacter (charCode |> fromCode) else nop
  )

controlCharacterCommands : Signal EditorCommand
controlCharacterCommands =
   Keyboard.keysDown |> Signal.map (\keysDown ->
    case keysDown of
      [39] -> selectNextNode
      [40] -> selectNextNode
      [40, 18] -> selectFirstChildNode
      [37] -> selectPreviousNode
      [38] -> selectPreviousNode
      [38, 18] -> selectParentNode
      _ -> nop
  )

-- Editor commands

nop : EditorCommand
nop modelRoot editorState = editorState

selectNode : Selection -> EditorCommand
selectNode newSelection modelRoot editorState = { editorState | selection <- newSelection, inputText <- "" }

selectFirstChildNode : EditorCommand
selectFirstChildNode modelRoot editorState = 
  editorState.selection |> Maybe.map (\selectedNode ->
    editorState |> selectNodeAt (selectedNode |> getChildNodes) 0
  ) |> withDefault { editorState | selection <- Just modelRoot }

selectParentNode : EditorCommand
selectParentNode modelRoot editorState =
  if editorState.inputText |> String.isEmpty then
    editorState.selection |> Maybe.map (\selectedNode ->
      { editorState | selection <- selectedNode |> findParent modelRoot }
    ) |> withDefault editorState
  else editorState

selectNextNode : EditorCommand
selectNextNode modelRoot editorState = editorState |> moveSelectionBy 1 modelRoot
-- TODO if end of list then parent

selectPreviousNode : EditorCommand
selectPreviousNode modelRoot editorState = editorState |> moveSelectionBy -1 modelRoot

enterCharacter : Char -> EditorCommand
enterCharacter char modelRoot editorState =
  if editorState.inputText |> String.isEmpty then
    let newInputText = char |> fromChar
    in
      { editorState |
        inputText <- newInputText,
        selectedCommandId <- editorState.selection `andThen` (\selectedNode -> selectedNode |> getAllCommands newInputText |> firstCommandId)
      }
  else editorState

changeInputText : String -> EditorCommand
changeInputText newInputText modelRoot editorState = { editorState | inputText <- newInputText }

selectCommand : Maybe CommandId -> EditorCommand
selectCommand newSelectedCommandId modelRoot editorState = { editorState | selectedCommandId <- newSelectedCommandId }

-- Helper functions

getAllCommands : String -> Node -> List Command
getAllCommands inputText node =
  let commandsWithInput = inputText |> node.commandsWithInput
      filteredCommands = node.commands |> filterCommands inputText
  in commandsWithInput ++ filteredCommands

filterCommands : String -> List Command -> List Command
filterCommands searchTerm commands =
  commands |> concatMap (\command ->
    case command of
      Command { text } -> if text `fuzzyContains` searchTerm then [command] else []
      Group { text, children } -> 
        let filteredChildren = children |> filterCommands searchTerm
        in if filteredChildren |> List.isEmpty then [] else [Group { text = text, children = filteredChildren }]
  )

findParent : Node -> Node -> Maybe Node -- TODO optimize with zippers?
findParent potentialParent selectedNode =
  let childNodes = potentialParent |> getChildNodes
      foundAsChild = childNodes |> member selectedNode
  in
    if foundAsChild then Just potentialParent
    else childNodes |> List.map (\childNode -> selectedNode |> findParent childNode) |> Maybe.oneOf

getChildNodes : Node -> List Node
getChildNodes parentNode =
  case parentNode.content of
    ChildrenContent children -> children
    _ -> []

selectNodeAt : List Node -> Int -> EditorState -> EditorState
selectNodeAt nodes index editorState =
  if editorState.inputText |> String.isEmpty then
    let newSelection =
          if nodes |> List.isEmpty then editorState.selection
          else nodes |> Array.fromList |> get index
    in { editorState | selection <- newSelection }
  else editorState

moveSelectionBy : Int -> Node -> EditorState -> EditorState
moveSelectionBy offset modelRoot editorState =
  editorState.selection `andThen` (\selectedNode ->
    selectedNode |> findParent modelRoot |> Maybe.map (\parent ->
      let childNodes = parent |> getChildNodes
          index = childNodes |> indexOfNode selectedNode
          newIndex = index + offset |> clamp 0 ((childNodes |> List.length) - 1)
      in editorState |> selectNodeAt childNodes newIndex
    )
  ) |> withDefault editorState

indexOfNode : Node -> List Node -> Int 
indexOfNode node nodes =
  nodes |> List.indexedMap (\index current ->
    if current == node then Just index else Nothing
  ) |> Maybe.oneOf |> withDefault 0

firstCommandId : List Command -> Maybe CommandId
firstCommandId commands = commands |> collectCommandInfos |> List.indexedMap (,) |> findCommandIdByIndex 0

collectCommandInfos : List Command -> List CommandInfo
collectCommandInfos commands =
  commands |> concatMap (\command ->
    case command of
      Command info -> [info]
      Group { children } -> children |> collectCommandInfos
  )

findCommandIdByIndex : Int -> List (Int, CommandInfo) -> Maybe CommandId
findCommandIdByIndex index commandInfosWithIndex =
  commandInfosWithIndex |> List.map (\indexed ->
    if (indexed |> fst) == index then Just (indexed |> snd).id else Nothing
  ) |> Maybe.oneOf

findCommandInfo : Maybe CommandId -> List Command -> Maybe CommandInfo
findCommandInfo maybeCommandId commands =
  maybeCommandId `andThen` (\commandId ->
    commands |> List.map (\command ->
      case command of
        Command info -> if info.id == commandId then Just info else Nothing
        Group { children } -> children |> findCommandInfo (Just commandId)
    ) |> Maybe.oneOf
  )

moveCommandSelectionBy : EditorState -> List Command -> Int -> Maybe CommandId
moveCommandSelectionBy editorState commandsWithInput offset =
  let commandInfosWithIndex = commandsWithInput |> collectCommandInfos |> List.indexedMap (,)
      index = commandInfosWithIndex |> indexOfCommandId editorState.selectedCommandId
      newIndex = (index + offset) % (commandInfosWithIndex |> List.length)
  in commandInfosWithIndex |> findCommandIdByIndex newIndex

indexOfCommandId : Maybe CommandId -> List (Int, CommandInfo) -> Int 
indexOfCommandId maybeCommandId commandInfosWithIndex =
  case maybeCommandId of
    Nothing -> 0
    Just commandId ->
      commandInfosWithIndex |> List.map (\indexed ->
        if (indexed |> snd).id == commandId then Just (indexed |> fst) else Nothing
      ) |> Maybe.oneOf |> withDefault 0
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
import Signal (..)
import String (..)
import String
import HyperStructure.Model (..)
import HyperStructure.Util (..)
import HyperStructure.EditorState (..)
import HyperStructure.EditorState as EditorState

viewNode : EditorState -> Node -> Html
viewNode editorState node =
  let selected = editorState.selection == Just node
      attributes =
        [
          classList [
            ("node", True),
            ("selected", selected)
          ],
          attribute "contextmenu" (node |> contextMenuId),
          onClick (selectNode (Just node) |> send editorCommandChannel)
        ]
      contentView = node |> viewContent editorState
      contextMenu = node |> viewContextMenu editorState
      keyboardMenu = node |> viewKeyboardMenu editorState
  in
    div attributes (contentView ++ contextMenu ++ keyboardMenu)

viewContent : EditorState -> Node -> List Html
viewContent editorState node =
  case node.content of
    HtmlContent html -> [html]
    ChildrenContent children -> children |> List.map (viewNode editorState)

viewContextMenu : EditorState -> Node -> List Html
viewContextMenu editorState node =
  [
    menu [
      id (node |> contextMenuId),
      attribute "type" "context"
    ] (node.commands |> List.map viewContextMenuItem)
  ]

contextMenuId : Node -> String
contextMenuId node = node.id ++ "ContextMenu"

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

viewKeyboardMenu : EditorState -> Node -> List Html
viewKeyboardMenu editorState node =
  if (editorState.selection == Just node) && not (editorState.inputText |> String.isEmpty) then
    let allCommands = node |> getAllCommands editorState.inputText
        handleInput string = changeInputText string |> send editorCommandChannel
        handleKeyDown keyCode =
          case keyCode of
            40 -> selectCommand (moveCommandSelectionBy editorState allCommands 1) |> send editorCommandChannel
            38 -> selectCommand (moveCommandSelectionBy editorState allCommands -1) |> send editorCommandChannel
            13 -> allCommands |> findCommandInfo editorState.selectedCommandId |> Maybe.map .message |> withDefault (nop |> send editorCommandChannel)
            _ -> nop |> send editorCommandChannel
        handleKeyUp keyCode =
          case keyCode of
            27 -> changeInputText "" |> send editorCommandChannel
            13 -> changeInputText "" |> send editorCommandChannel
            _ -> nop |> send editorCommandChannel
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
        ] [text `highlightOccurencesOfWords` editorState.inputText]
    Group { text, children } ->
      let caption = span [class "caption"] [text |> Html.text]
          childrenView = children |> List.map (viewKeyboardMenuItem editorState)
      in div [class "group"] ([caption] ++ childrenView)
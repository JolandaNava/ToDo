module Chore exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput, onDoubleClick, onBlur)
import Html.Attributes exposing (style, type_, placeholder, value, class, hidden, classList)
import Json.Decode as Json
import Dom
import Task

-- The Chore file on the todomvc does not have a main function at all, I am following that

-- MODEL 

type alias Chore =
  { chore : Maybe String
  , id : Int
  , completed : Bool
  , changedchore : Maybe String
  }


init : String -> Int -> Chore
init chore id =
  Chore (Just chore) id False Nothing


-- UPDATE

type Msg
    = NoOp
    | RewriteChore
    | StoreChanges String
    | CommitChange
    | DeleteChore 
    | ToggleChore 

update : Msg -> Chore -> (Chore, Cmd Msg)
update msg chore =
  case msg of
    NoOp ->
        (chore, Cmd.none)
    
    DeleteChore ->
        ({ chore | chore = Nothing}, Cmd.none)
    
    RewriteChore ->
        ({ chore | changedchore = chore.chore }, Cmd.none ) 
        -- add comand

    StoreChanges text ->
        ({ chore | changedchore = Just text }, Cmd.none)

    CommitChange ->
        case chore.changedchore of
            Nothing ->
                (chore, Cmd.none)
            Just text ->
                case text of
                    "" ->  ({ chore | chore = Nothing}, Cmd.none)
                    _ ->  ({ chore | chore = Just text, changedchore = Nothing}, Cmd.none)
   
    ToggleChore ->
        ({ chore | completed = not chore.completed }, Cmd.none)
    
  


-- SUBSCRIPTIONS

subscriptions : Chore -> Sub Msg
subscriptions chore =
    Sub.none


-- VIEW

view : Chore -> Html Msg
view chore =
    let
        isbeingedited = case chore.changedchore of
            Nothing -> False
            Just text -> True
    in
    li 
    []
    [ div 
        [ classList [("chore", True), ("completed", chore.completed)]
        , hidden isbeingedited
        ]
        [ input 
            [ type_ "checkbox"
            , class "toggle"
            , Html.Attributes.checked chore.completed
            , onClick (ToggleChore)
            ] []
        , label 
            [ onDoubleClick RewriteChore ]
            [ text (description chore.chore)
            ]
        , button 
            [ onClick DeleteChore ] 
            [ text "×" ]
        ] 
    , div 
        [ class "editing-chore"
        , hidden (not isbeingedited)]
        [ input 
            [ Html.Attributes.id (makeId chore)
            , value (description chore.changedchore)
            , onInput StoreChanges
            , onBlur CommitChange
            , onKeyDown (enterKey CommitChange)
            ] []
        ]
    ]


description : Maybe String -> String
description chore =
    case chore of
            Nothing ->
                ""
            Just text -> text 

onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

enterKey :  Msg -> Int -> Msg
enterKey msg int = 
  if int == 13 then
    msg
  else
    NoOp

-- Creating the id that html will grab onto to focus on the chore that is being edited
makeId : Chore -> String
makeId chore = 
    "chore-" ++ (toString chore.id)

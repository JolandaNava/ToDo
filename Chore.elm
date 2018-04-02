module Chore exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput, onDoubleClick, onBlur)
import Html.Attributes exposing (style, type_, placeholder, value, class, hidden, classList)
import Json.Decode as Json

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

update : Msg -> Chore -> Chore
update msg chore =
  case msg of
    NoOp ->
        chore
    
    DeleteChore ->
        { chore | chore = Nothing}
    
    StoreChanges text ->
        { chore | changedchore = Just text }

    CommitChange ->
        case chore.changedchore of
            Nothing ->
                chore
            Just text ->
                case text of
                    "" ->  { chore | chore = Nothing}
                    _ -> { chore | chore = Just text, changedchore = Nothing}
    
    ToggleChore ->
        { chore | completed = not chore.completed }
    
    RewriteChore ->
        { chore | changedchore = chore.chore }
    
  


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
            [ text (description chore)
            ]
        , button 
            [ onClick DeleteChore ] 
            [ text "×" ]
        ] 
    , div 
        [ class "editing-chore"
        , hidden (not isbeingedited)]
        [ input 
            [ value (description chore)
            , onInput StoreChanges
            , onKeyDown (enterKey CommitChange)
            , onBlur CommitChange
            ] []
        ]
    ]


description : Chore -> String
description chore =
    case chore.changedchore of
            Nothing ->
                case chore.chore of
                    Nothing -> ""
                    Just text -> text
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


-- NEXT STEPS
-- onBlur CommitChange command seems to not be working

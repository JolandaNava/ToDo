module Chore exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput, onDoubleClick, onBlur)
import Html.Attributes exposing (style, type_, placeholder, value)
import Json.Decode as Json

-- The Chore file on the todomvc does not have a main function at all, I am following that

-- MODEL 

type alias Model =
  { chore : String
  , id : Int
  , completed : Bool
  , changedchore : Maybe String
  }


init : String -> Int -> (Model, Cmd Msg)
init chore id =
  (Model chore id False Nothing , Cmd.none)



-- UPDATE

type Msg
    = NoOp
    | RewriteChore
    | StoreChanges String
    | CommitChange
    | DeleteChore 
    | ToggleChore 

update : Msg -> Model -> Maybe Model
update msg model =
  case msg of
    NoOp ->
        Just model
    
    DeleteChore ->
        Nothing
    
    StoreChanges text ->
        Just { model | changedchore = Just text }

    CommitChange ->
        case model.changedchore of
            Nothing ->
                Nothing
            Just text ->
                Just { model | chore = text, changedchore = Nothing}
    
    ToggleChore ->
        Just { model | completed = not model.completed }
    
    RewriteChore ->
        Just { model | changedchore = Just model.chore }
    
  


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    li 
    []
    [ div 
        []
        [ input 
            [ type_ "checkbox"
            , Html.Attributes.checked model.completed
            , onClick (ToggleChore)
            ] []
        , label 
            [ onDoubleClick RewriteChore]
            [ text model.chore ]
        , button 
            [ onClick DeleteChore ] 
            [ text "X" ]
        ] 
    , input 
        [ onInput StoreChanges
        , onKeyDown (enterKey CommitChange)
        , onBlur CommitChange
        ] []
    ]

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
-- start re-writing todo to use the chore module instead.
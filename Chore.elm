-- Making a module for task 
-- I want to call this module quest instead of task, but I am unsure how changing the name of the file
-- would affect pushing the changes to the remote
module Task exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput, onDoubleClick, onBlur)
import Html.Attributes exposing (style, type_, placeholder, value)
import Json.Decode as Json

-- The Task file on the todomvc does not have a main function at all, I am following that

-- MODEL 

type alias Model =
  { task : String
  , id : Int
  , completed : Bool
  , changedtask : Maybe String
  }


init : String -> Int -> (Model, Cmd Msg)
init task id =
  (Model task id False Nothing , Cmd.none)



-- UPDATE

type Msg
    = NoOp
    | RewriteTask
    | StoreChanges String
    | CommitChange
    | DeleteTask 
    | ToggleTask 

update : Msg -> Model -> Maybe Model
update msg model =
  case msg of
    NoOp ->
        Just model
    
    DeleteTask ->
        Nothing
    
    StoreChanges text ->
        Just { model | changedtask = Just text }

    CommitChange ->
        case model.changedtask of
            Nothing ->
                Nothing
            Just text ->
                Just { model | task = text, changedtask = Nothing}
    
    ToggleTask ->
        Just { model | completed = not model.completed }
    
    RewriteTask ->
        Just { model | changedtask = Just model.task }
    
  


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
            , onClick (ToggleTask)
            ] []
        , label 
            [ onDoubleClick RewriteTask]
            [ text model.task ]
        , button 
            [ onClick DeleteTask ] 
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
-- find out how to rename file and push renamed version to remote
-- commit latest version of todo to master branch
-- start re-writing todo to use the task module instead.
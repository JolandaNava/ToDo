-- Making a module for task 
module Task exposing (..)

import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput, onDoubleClick, onBlur)
import Html.Attributes exposing (style, type_, placeholder, value)
import Json.Decode as Json

-- not sure how to handle the fact that a Task will only be initiated by being called in the Todo program. 
-- Right not I am initiating a task with an empty string and 0 as the id. It doesn't look like a great idea
-- Note: The Task file on the todomvc does not have a main function at all
main : Program Never Model Msg
main =
    Html.program
        { init = init "" 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

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
    | SelectTask Int
    | StoreChanges String
    | NewTask String Int
    | DeleteTask Int
    | CommitChange Int
    | ToggleTask Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
        (model, Cmd.none)
    
    NewTask task id ->
        (model, Cmd.none)
    
    DeleteTask id ->
        (model, Cmd.none)
    
    CommitChange id ->
        (model, Cmd.none)
    
    ToggleTask id ->
        (model, Cmd.none)
    
    SelectTask id ->
        (model, Cmd.none)
    
    StoreChanges text ->
        (model, Cmd.none)


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
            , onClick (ToggleTask model.id)
            ] []
        , label 
            [ onDoubleClick (SelectTask model.id)]
            [ text model.task ]
        , button 
            [ onClick (DeleteTask model.id) ] 
            [ text "X" ]
        ] 
    , input 
        [ onInput StoreChanges
        , onKeyDown (enterKey (CommitChange model.id))
        , onBlur (CommitChange model.id)
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

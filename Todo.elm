import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Html.Attributes exposing (style, type_, placeholder, value)
import Json.Decode as Json

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- TASKS (this might become its own module at some point, but we'll start here)

type alias Task =
  { task : String
  , completed : Bool
  , ide : Int
  }

buildtask : String -> Int -> Task
buildtask task ide =
  Task task False ide

-- this should come in handy to differentiate the different views we want
type Visibility = Completed | Active | All

-- MODEL
type alias Model =
  { alltasks : List Task
  , nextide : Int
  , view : Visibility
  , newtask : Maybe String
  }


init : (Model, Cmd Msg)
init =
 ( Model [] 0 All Nothing, Cmd.none)



-- UPDATE
-- helper functions to parse through task list and delete or update the selected task
deleteT : Task -> Model -> Model
deleteT task model =
  let 
    isTask1 i task = 
      if i == task.ide then 
        Nothing 
      else 
        Just task 
  in
  { model | alltasks = List.filterMap (isTask1 task.ide) model.alltasks }

updateT : Task -> Model -> Model 
updateT task model =
  let 
    isTask2 i task = 
      if i == task.ide then 
        { task | completed = not task.completed }
      else 
        task 
  in
  { model | alltasks = List.map (isTask2 task.ide) model.alltasks }

toggleall : Model -> List Task 
toggleall model =
  case model.alltasks of
    [] -> 
      []
    _ ->
      if List.length (completedT model) == List.length model.alltasks then
        List.map (\task -> {task | completed = False }) model.alltasks
      else 
        List.map (\task -> {task | completed = True }) model.alltasks

type Msg
  = NoOp
  | PreparingTask String
  | NewTask
  | Toggle Task
  | Delete Task
  | ChangeView Visibility
  | ToggleAll
  | ClearCompleted

-- actual update function 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    Toggle task ->
      ( updateT task model , Cmd.none)

    Delete task ->
      ( deleteT task model , Cmd.none)
    
    PreparingTask string ->
      ({ model | newtask = Just string } , Cmd.none)

    NewTask ->
      case model.newtask of
        Nothing ->
          (model, Cmd.none)
        Just string ->
          let 
            newt = buildtask string model.nextide
          in
          ({model | alltasks = model.alltasks ++ [newt], 
            newtask = Nothing,
            nextide = model.nextide + 1 } 
          , Cmd.none)

    ChangeView visibility ->
          ({ model | view = visibility } , Cmd.none)
    
    ToggleAll ->
      ({model | alltasks = toggleall model }, Cmd.none)

    ClearCompleted ->
      ({ model | alltasks = uncompletedT model }, Cmd.none)
        



-- VISIBILITY HELPER FUNCTIONS
-- only uncompleted tasks 
uncompletedT : Model -> List Task
uncompletedT model =
  let 
    isnotComp task = 
    case task.completed of
      True ->
        Nothing 
      False ->
        Just task 
  in
    List.filterMap isnotComp model.alltasks


-- only completed tasks 
completedT : Model -> List Task
completedT model =
  let 
    isComp task = 
      case task.completed of
        True ->
          Just task 
        False -> 
          Nothing 
  in
    List.filterMap isComp model.alltasks
  
-- create string to display number of items left
itemslfet : List a -> String
itemslfet alltasks =
  let 
    l = List.length alltasks 
  in
    if l == 1 then 
      "1 item left"
    else 
      (toString l) ++ (" items left")


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "todos" ]
    , input [ placeholder "What needs to be done?"
            , onKeyDown enterKey
            , onInput PreparingTask
            , value 
              (case model.newtask of 
                Nothing -> ""
                Just a -> a)] []
    , button [ onClick NewTask ] [ text "New Task" ]
    , button [ onClick (ChangeView All) ] [ text "All"]
    , button [ onClick (ChangeView Completed) ] [ text "Completed"]
    , button [ onClick (ChangeView Active) ] [ text "Active"]
    , button [ onClick ToggleAll ] [ text "v"]
    , button [ onClick ClearCompleted ] [ text ("Clear completed (" ++ (toString (List.length (completedT model))) ++ ")" ) ]
    , ul [] (
        case model.view of  
          Completed ->
            tasklist (completedT model)
          Active -> 
            tasklist (uncompletedT model)
          All -> 
            tasklist model.alltasks
            )
    , h6 [] [text (itemslfet (uncompletedT model))]
    ]


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

enterKey : Int -> Msg
enterKey int = 
  if int == 13 then
    NewTask
  else
    NoOp


taskview : Task -> Html Msg
taskview task =
  li []
    [ input [ type_ "checkbox", onClick (Toggle task)] []
    , text task.task
    , button [ onClick (Delete task) ] [ text "X" ]
    ]

tasklist : List Task -> List (Html Msg)
tasklist list =
  List.map taskview list


-- NEXT STEPS 
-- fix the checkbox issue by learning how to use Html.Keyed 
-- double-click on a to-do allows you to mofify it
-- Re-order buttons to be where they should be, kinda
-- Display buttons only when the actions are possible 
-- Learn how to point at css sheet from Elm
-- Learn how to link Elm file to css sheet through main Hthml file 
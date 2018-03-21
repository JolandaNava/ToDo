import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Html.Attributes exposing (style, type_, placeholder, value)
import Json.Decode as Json
-- Importing the Chore module
import Chore exposing (Chore)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- this should come in handy to differentiate the different views we want
type Visibility = Completed | Active | All

-- MODEL
type alias Model =
  { allchores : List Chore
  , nextid : Int
  , view : Visibility
  , newchore : Maybe String
  }


init : (Model, Cmd Msg)
init =
 ( Model [] 0 All Nothing, Cmd.none)



-- UPDATE
-- helper functions to parse through chore list and delete or update the selected chore
deleteT : Chore -> Model -> Model
deleteT chore model =
  let 
    isChore1 i chore = 
      if i == chore.id then 
        Nothing 
      else 
        Just chore 
  in
  { model | allchores = List.filterMap (isChore1 chore.id) model.allchores }

updateT : Chore -> Model -> Model 
updateT chore model =
  let 
    isChore2 i chore = 
      if i == chore.id then 
        (Chore.update Chore.ToggleChore chore)
      else 
        chore 
  in
  { model | allchores = List.map (isChore2 chore.id) model.allchores }

toggleall : Model -> List Chore 
toggleall model =
  case model.allchores of
    [] -> 
      []
    _ ->
      if List.length (completedT model) == List.length model.allchores then
        List.map (\chore -> {chore | completed = False }) model.allchores
      else 
        List.map (\chore -> {chore | completed = True }) model.allchores

type Msg
  = NoOp
  | PreparingChore String
  | NewChore
  | Toggle Chore
  | ChangeView Visibility
  | ToggleAll
  | ClearCompleted
  | ChoreMsg Chore.Msg

-- actual update function 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChoreMsg msg ->
      ({ model | allchores = (discardEmpty model.allchores)}, Cmd.none)

    NoOp ->
      (model, Cmd.none)

    Toggle chore ->
      ( updateT chore model , Cmd.none)
    
    PreparingChore string ->
      ({ model | newchore = Just string } , Cmd.none)

    NewChore ->
      case model.newchore of
        Nothing ->
          (model, Cmd.none)
        Just string ->
          let 
            newc = Chore.init string model.nextid
          in
          ({model | allchores = model.allchores ++ [newc], 
            newchore = Nothing,
            nextid = model.nextid + 1 } 
          , Cmd.none)

    ChangeView visibility ->
          ({ model | view = visibility } , Cmd.none)
    
    ToggleAll ->
      ({model | allchores = toggleall model }, Cmd.none)

    ClearCompleted ->
      ({ model | allchores = uncompletedT model }, Cmd.none)
        


-- VISIBILITY HELPER FUNCTIONS
-- helper function to continuously discard deleted chores 
discardEmpty : List Chore -> List Chore
discardEmpty clist = 
  let 
    isnothing chore = 
      case chore.chore of
        Nothing -> False
        _ -> True
  in
  List.filter isnothing clist

-- only uncompleted chores 
uncompletedT : Model -> List Chore
uncompletedT model =
  let 
    isnotComp chore = 
    case chore.completed of
      True ->
        Nothing 
      False ->
        Just chore 
  in
    List.filterMap isnotComp model.allchores


-- only completed chores 
completedT : Model -> List Chore
completedT model =
  let 
    isComp chore = 
      case chore.completed of
        True ->
          Just chore 
        False -> 
          Nothing 
  in
    List.filterMap isComp model.allchores
  
-- create string to display number of items left
itemslfet : List a -> String
itemslfet allchores =
  let 
    l = List.length allchores 
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
    , input 
      [ placeholder "What needs to be done?"
      , onKeyDown enterKey
      , onInput PreparingChore
      , value 
        (case model.newchore of 
          Nothing -> ""
          Just a -> a)
      ] []
    , button [ onClick NewChore ] [ text "New Chore" ]
    , button [ onClick (ChangeView All) ] [ text "All"]
    , button [ onClick (ChangeView Completed) ] [ text "Completed"]
    , button [ onClick (ChangeView Active) ] [ text "Active"]
    , button [ onClick ToggleAll ] [ text "v"]
    , button [ onClick ClearCompleted ] [ text ("Clear completed (" ++ (toString (List.length (completedT model))) ++ ")" ) ]
    , ul [] (
        case model.view of  
          Completed ->
            chorelist (completedT model)
          Active -> 
            chorelist (uncompletedT model)
          All -> 
            chorelist model.allchores
            )
    , h6 [] [text (itemslfet (uncompletedT model))]
    ]


-- onKeyDown and enterKey allow to register when "Enter" is pressed and attach a msg to that action
onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

enterKey : Int -> Msg
enterKey int = 
  if int == 13 then
    NewChore
  else
    NoOp

-- prepares the view for a list of chores
chorelist : List Chore -> List (Html Msg)
chorelist list =
  List.map (Html.map ChoreMsg) (List.map Chore.view list)
-- I am not sure I fully understand what the ChoreMsg wrapper is doing



-- NEXT STEPS 
-- understand how to keep list of chores up-to-date if the Delete command is only possible on the chore module
-- double-click on a to-do allows you to mofify it
-- Re-order buttons to be where they should be, kinda
-- Display buttons only when the actions are possible 
-- Learn how to point at css sheet from Elm
-- Learn how to link Elm file to css sheet through main Hthml file 
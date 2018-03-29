import Html exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Html.Attributes exposing (style, type_, placeholder, value, class)
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


-- UPDATE HELPERS
-- helper function to replace a chore with its updated version in a list of chores
updateC : Chore -> Model -> List Chore 
updateC newchore model =
  let 
    isChore2 id chore = 
      if id == chore.id then 
        newchore
      else 
        chore 
  in
    (List.map (isChore2 newchore.id) model.allchores )

-- if all tasks are completed, turns them all to uncomplete, otherwise marks all tasks as completed
toggleall : Model -> List Chore 
toggleall model =
  case model.allchores of
    [] -> 
      []
    _ ->
      if List.length (onlyCompleted True model) == List.length model.allchores then
        List.map (\chore -> {chore | completed = False }) model.allchores
      else 
        List.map (\chore -> {chore | completed = True }) model.allchores


type Msg
  = NoOp
  | PreparingChore String
  | NewChore
  | ChangeView Visibility
  | ToggleAll
  | ClearCompleted
  | ChoreMsg Chore Chore.Msg

-- UPDATE FUNCTION
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChoreMsg chore msg ->
      let 
        newc =  Chore.update msg chore
        newlist = updateC newc model
      in
        ({ model | allchores = (discardEmpty newlist)}, Cmd.none)

    NoOp ->
      (model, Cmd.none)
    
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
      ({ model | allchores = (onlyCompleted False model) }, Cmd.none)
        


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

-- function to select only completed (bool = true) or uncompleted (bpool = false) chores
onlyCompleted : Bool -> Model -> List Chore
onlyCompleted bool model = 
  let
    iscompl chore = 
      case chore.completed of
        True -> bool
        False -> (not bool)
  in
    List.filter iscompl model.allchores

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

-- translates visibility type to string for display
visibilitystring : Visibility -> String
visibilitystring vis =
  case vis of
    Completed -> "completed items"
    All -> "all items"
    Active -> "active items"


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "todos" ]
    , div [ class "header" ] 
        [ button [ class "toggle-all" , onClick ToggleAll ] []
        , input 
          [ class "newchore"
          , placeholder "What needs to be done?"
          , onKeyDown enterKey
          , onInput PreparingChore
          , value 
            (case model.newchore of 
              Nothing -> ""
              Just a -> a)
          ] []
        ]
    , ul [] (
        case model.view of  
          Completed ->
            chorelist (onlyCompleted True model)
          Active -> 
            chorelist (onlyCompleted False model)
          All -> 
            chorelist model.allchores
            )
    , div [ class "footer" ]
      [ p [class "items-left "] [text (itemslfet (onlyCompleted False model))]
      , button [ class "view", onClick (ChangeView All) ] [ text "All"]
      , button [ class "view", onClick (ChangeView Active) ] [ text "Active"]
      , button [ class "view", onClick (ChangeView Completed) ] [ text "Completed"]
      , button [ class "clear-completed", onClick ClearCompleted ] [ text ("Clear completed (" ++ (toString (List.length (onlyCompleted True model))) ++ ")" ) ]
      ]
    , p [] [text ("Currently viewing " ++ visibilitystring model.view )]
    ]


-- VIEW HELPER FUNCTIONS

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


-- from List.extra, mapping a list of functions onto a list of elements
-- used to prepare the chorelist
andMap : List a -> List (a -> b) -> List b
andMap l fl =
    List.map2 (<|) fl l

-- prepares the view for a list of chores using Chore.view
chorelist : List Chore -> List (Html Msg)
chorelist list =
  let
    listchoresviews = List.map Chore.view list
    listoffunctions = List.map Html.map (List.map ChoreMsg list)
  in 
    andMap listchoresviews listoffunctions



-- NEXT STEPS 
-- ensure user knows what view they are on (for now I just added a line of text, fix with css by highlighting button)
-- double-click on a to-do allows you to mofify it (able to modify - I think css needs to be set up to fully implement this)
-- Display buttons only when the actions are possible (css)
-- the actual todo website stores the tasks somwhere (refreshing the page does not delete previous tasks) - how do I implement that?
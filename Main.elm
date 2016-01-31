module Main where

import TodoItem
import Static exposing ( Email, Reminder )
import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (..)
import Html.Attributes as Attri
import String
import Signal exposing ( Mailbox, Address)
import Html.Events as E
import Debug
import Keyboard as Kb
import Char
import Time
import Date
import Task exposing (..)
import Json.Decode as Json exposing (Decoder, list, (:=))
import Http


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: 'Alt + v' toggles the visibility of 'done' items


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: 'Alt + r' toggles the visibility of 'add reminder'


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed
-- Summary: Current date is shown in date picker.
--          Current date is set by invoking the Action UpdateTime via a port.
--          Each Action is provided with a timestamp in the update function.
--          This timestamp is used to set the current date in "UpdateTime"


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed
-- Summary: When an item is overdue it is shown in red.
--          A check is done between the current date and date of the reminder.
--          If it is overdue a Boolean is passed to the view to indicate to show it's overdue.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed
-- Summary: Each item has a date picker and snooze button. Just click snooze to snooze to the date
--          The snooze date string is stored in the TodoItem model and a boolean if the item is snoozed
--          A check is done between the current date, snooze date, and boolean 'isSnoozed' if the item should be visible


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed
-- Summary: Ports and tasks are used for the implementation.
--          The json is retreived with Http and parsed to TodoItems objects which carry an email
--          If this fails, an Error action is invoked
--          If this succeeds, the "AddEmails" action is invoked which carries a list of TodoItems (containg the emails)
--          There is a check for duplicates done in the update function. This compares a few properties of the email (from,to,date,body,title)


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary: Every 10 seconds the Json is retreived. This is done in the port described in the previous extension.


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed
-- Summary: 'Alt + A' marks all unsnoozed ToDo Items as done


-- Start of program

{-- File is devided in
    1. Model
    2. View
    3. Key Shortcuts
    4. Actions
    5. Update
    6. HTTP request for json
    7. Sorting
    8. Helper Functins
    9. CSS attributes
--}

main : Signal Html
main = Signal.map (view mailActions.address) state

state : Signal Model
state = Signal.foldp update initModel (Time.timestamp <| Signal.mergeMany [mailActions.signal, keycomb])

mailActions : Mailbox Action
mailActions = Signal.mailbox NoAction

{-- 1. Model --}
type alias Model = { elements : List (ID, TodoItem.Model)
                   , nextId : Int
                   , inputRemBody : String
                   , inputRemDate : String
                   , currDate : String
                   , focusedItem  : Int
                   , isSortReversed : Bool
                   , isDoneVisible: Bool
                   , isReminderVisible: Bool
                   , test: String}

type alias ID = Int

initModel : Model
initModel =
  {elements =
    zip [0,1,2,3]
      (List.append
        (List.map (\x -> TodoItem.E x) Static.emails )
        (List.map (\x -> TodoItem.R x) Static.reminders ))
  , nextId = 4
  , inputRemBody = ""
  , inputRemDate = ""
  , currDate = ""
  , focusedItem = 0
  , isSortReversed = False
  , isDoneVisible = True
  , isReminderVisible = True
  , test = "Used to debug, messages get placed here"  }

{-- Kinda hacky way to init model with date --}
port initTime : Task.Task x ()
port initTime =
  Signal.send mailActions.address UpdateTime

{-- 2. View --}
view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [mainDivStyle] [
        viewTodo address model
      , cleanIf model.isDoneVisible (viewDone address model)
      , cleanIf model.isReminderVisible (viewAddReminder address model.inputRemBody model.inputRemDate)]

viewTodo : Signal.Address Action -> Model -> Html
viewTodo address model =
    let todoList = List.filter (\(_, (_,x)) -> (not <| TodoItem.getDone x) && not (((TodoItem.getSnoozeDate x > model.currDate) && (TodoItem.getIsSnoozed x))) ) (sortTodosWithIndex model.elements model.isSortReversed)
    in Html.div [] [
          cleanIf  (not <| List.isEmpty todoList) (Html.h1 [] [Html.text "To Do"])
        , Html.ul [ulStyleTodo] (List.map (viewTodoItem model address) todoList)]

viewDone : Signal.Address Action -> Model -> Html
viewDone address model =
  let doneList = List.filter (\(_, (_,x)) -> TodoItem.getDone x ) (sortTodosWithIndex model.elements model.isSortReversed)
  in  Html.div [] [
      cleanIf  (not <| List.isEmpty doneList ) (Html.h1 [] [Html.text "Done"])
      , Html.ul [ulStyleDone] (List.map (viewTodoItem model address) doneList)]

viewTodoItem : Model -> Address Action -> (Int, (ID, TodoItem.Model)) -> Html
viewTodoItem mainModel address (index, (id, model)) =
    let context = TodoItem.Context (Signal.forwardTo address (Modify id))
    in
      TodoItem.elementView context model
        (isFocused index mainModel.focusedItem)
        (isOverdue (TodoItem.getDate model) mainModel.currDate (TodoItem.getDone model))


viewAddReminder: Address Action -> String -> String -> Html
viewAddReminder address body date =
    Html.div [] [
    Html.h1 [] [Html.text "Reminder"]
    , Html.input
        [E.on "input" E.targetValue (Signal.message address << UpdateInputRemBody)
        , value body]
        []
    , Html.input
        [type' "date"
        ,value date
        ,E.on "input" E.targetValue (Signal.message address << UpdateInputRemDate)]
        []
    , Html.button [ E.onClick address Add ]
        [ Html.text "Add" ]
    ]



{-- 3. KeyCombs --}
{-- Two actions are passed.
    The first one is executed when the key combination is pressed
    The second one is executed when the key combination is released --}

keycomb : Signal Action
keycomb =  (Signal.mergeMany
                  [getComb IncreaseFocus NoAction 'J'
                  ,getComb DecreaseFocus NoAction 'K'
                  ,getComb (ShortcutAction TodoItem.Pin) NoAction  'P'
                  ,getComb (ShortcutAction TodoItem.Mark) NoAction 'X'
                  ,getComb (ShortcutAction TodoItem.More) NoAction 'O'
                  ,getComb ToggleDoneView NoAction     'V'
                  ,getComb ToggleReminderView NoAction 'R'
                  ,getComb SortReversed SortNormal     'S'
                  ,getComb MarkAllDone NoAction        'A'])


getComb : Action -> Action -> Char -> Signal Action
getComb ac1 ac2 char = Signal.map (actionChooser ac1 ac2) (Signal.map2 (&&) Kb.alt (Kb.isDown <| Char.toCode char))

actionChooser : Action -> Action -> Bool -> Action
actionChooser action1 action2 b = if | b -> action1
                                     | otherwise -> action2


{-- 4. Actions --}
type Action =
      Modify ID TodoItem.Action
    | Add
    | UpdateInputRemBody String
    | UpdateInputRemDate String
    | IncreaseFocus
    | DecreaseFocus
    | ShortcutAction TodoItem.Action
    | ToggleDoneView
    | ToggleReminderView
    | SortNormal
    | SortReversed
    | MarkAllDone
    | UpdateTime
    | AddEmails (List TodoItem.Model)
    | Error String
    | NoAction

{-- 5. Update --}
update : (Float,Action) -> Model -> Model
update (time,action) model =
      let getDay  = Date.day <| Date.fromTime time
          getMonth  = dateStringToNumber <| Date.month <| Date.fromTime time
          getYear  = Date.year <| Date.fromTime time
          getDateString  = ((toString) (getYear )) ++ "-" ++ getMonth ++ "-" ++ (toString (getDay ))
        in
  (case action of
    Modify id elementAction ->
      let updateTodoItem (elementID, elementModel) =
            if elementID == id
                then (elementID, TodoItem.update elementAction elementModel)
                else (elementID, elementModel)
      in
          { model | elements <- List.map updateTodoItem model.elements }
    Add ->
      { model |
          nextId <- model.nextId + 1
          ,inputRemBody <- ""
          ,elements <-
              if String.isEmpty model.inputRemBody
                then model.elements
                else model.elements ++ [(model.nextId,TodoItem.newReminder model.inputRemBody model.inputRemDate)]

      }
    UpdateInputRemBody string ->
      { model | inputRemBody <- string }
    UpdateInputRemDate string ->
      { model | inputRemDate <- string }
    IncreaseFocus ->
      { model | focusedItem <- if calcMaxFocusCounter model /= model.focusedItem then model.focusedItem + 1 else 0}
    DecreaseFocus ->
      { model | focusedItem <- if model.focusedItem /= 0  then model.focusedItem - 1 else calcMaxFocusCounter model }
    ShortcutAction elementAction ->
      let idToPin = findFocusedItemId model
          updateTodoItem (elementID, elementModel) =
              if elementID == idToPin
                  then (elementID, TodoItem.update elementAction elementModel)
                  else (elementID, elementModel)
      in { model | elements <- List.map updateTodoItem model.elements }
    ToggleDoneView -> if | model.isDoneVisible -> { model | isDoneVisible <- False}
                         | otherwise -> { model | isDoneVisible <- True}
    ToggleReminderView -> if | model.isReminderVisible -> { model | isReminderVisible <- False}
                             | otherwise -> { model | isReminderVisible <- True}
    SortNormal ->   {model | isSortReversed <- False}
    SortReversed -> {model | isSortReversed <- True}
    UpdateTime  -> {model | currDate <- getDateString,
                            inputRemDate <- getDateString}
    AddEmails newEmails -> List.foldr addNewEmail model newEmails
    Error msg -> {model | test <- msg} {-- this is only visible in debug mode in the model output --}
    MarkAllDone ->
      let updateTodoItem (elementID, elementModel) =
        if (not <|TodoItem.getDone elementModel) && (not <| TodoItem.getIsSnoozed elementModel)  then
            (elementID, TodoItem.update TodoItem.Mark elementModel)
        else (elementID, elementModel)
      in { model | elements <- List.map updateTodoItem model.elements }
    NoAction -> model)
    |> Debug.watch "model"

addNewEmail: TodoItem.Model -> Model -> Model
addNewEmail newEmail model =
  if (List.foldr (\(_,email) bool -> if bool then bool else TodoItem.compareModel newEmail email) False model.elements)
    then model
    else
    { model |
        nextId <- model.nextId + 1
        ,elements <- model.elements ++ [(model.nextId,newEmail)]}

{-- 6. HTTP request for json--}
getEmailData _ =
  let
    request =
      Http.get emailJsonDecoder "https://crossorigin.me/https://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"
        |> Task.map AddEmails
  in
    request
      `Task.onError` (\err -> Task.succeed (Error (toString err)))
      `Task.andThen` (\action -> Signal.send mailActions.address action )


port getEmails : Signal (Task.Task a ())
port getEmails =  Signal.map getEmailData (Time.every (Time.minute / 6.0) )

emailJsonDecoder: Decoder (List TodoItem.Model)
emailJsonDecoder =
    let makeEmail = Json.object4
            (\title from date body -> TodoItem.newEmail title from date body )
            ("title" := Json.string)
            ("from" := Json.string)
            ("date" := Json.string)
            ("body" := Json.string)
    in
        "emails" := Json.list makeEmail

{-- 7. Sorting --}
sortTodosWithIndex : List (Int, TodoItem.Model) -> Bool -> List (Int, (Int, TodoItem.Model))
sortTodosWithIndex list isReversed =
  let listLength = List.length list
  in
  if | isReversed -> zip [0..listLength] (List.sortWith sortElementsReversed list)
     | otherwise  -> zip [0..listLength] (List.sortWith sortElements list)


sortElements : (ID, TodoItem.Model) -> (ID, TodoItem.Model) -> Order
sortElements (_,a) (_,b) =
  TodoItem.normalSort a b

sortElementsReversed : (ID, TodoItem.Model) -> (ID, TodoItem.Model) -> Order
sortElementsReversed (_,a) (_,b) =
  TodoItem.reverseSort a b


{-- 8. Helper Functions --}
isFocused : Int -> Int -> Bool
isFocused focusedIndex index = focusedIndex == index

isOverdue : String -> String -> Bool -> Bool
isOverdue deadline date done = (deadline < date) && (not done)

findFocusedItemId : Model -> Int
findFocusedItemId model =
     List.foldl (\(index,(todoId,_)) id -> if model.focusedItem == index then todoId else id )
                0
                (sortTodosWithIndex model.elements model.isSortReversed)

{-- to remove ugly if conditions in html--}
cleanIf: Bool -> Html -> Html
cleanIf  bool html =
    if bool then
      html
    else
      Html.text ""

zip = List.map2 (,)

calcMaxFocusCounter: Model -> Int
calcMaxFocusCounter model =
  if | model.isDoneVisible ->  List.length model.elements - 1
     | otherwise -> List.length model.elements - (List.length (List.filter (\(_,x) -> TodoItem.getDone x ) model.elements )) - 1

dateStringToNumber: Date.Month -> String
dateStringToNumber month =
    case month of
      Date.Jan -> "01"
      Date.Feb -> "02"
      Date.Mar -> "03"
      Date.Apr -> "04"
      Date.May -> "05"
      Date.Jun -> "06"
      Date.Jul -> "07"
      Date.Aug -> "08"
      Date.Sep -> "09"
      Date.Oct -> "10"
      Date.Nov -> "11"
      Date.Dec -> "12"

{-- 9. CSS attributes --}
mainDivStyle : Html.Attribute
mainDivStyle =
  Attri.style
    [ ("width", "50%")
     ,("margin","auto")
     ]

ulStyleTodo : Html.Attribute
ulStyleTodo =
  Attri.style
    [ ("padding", "0")
      ,("list-style-type","none")
      ]

ulStyleDone : Html.Attribute
ulStyleDone =
  Attri.style
    [ ("padding", "0")
      ,("list-style-type","none")
      ,("opacity","0.7")]

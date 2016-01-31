module TodoItem where

import Static exposing ( Email, Reminder )
import Signal exposing ( Signal, Mailbox, Address )
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Attributes as Attri
import String
import Signal
import Html.Events as E
import Debug
import Date


{-- File is devided in
    1. Model
    2. View
    3. Actions
    4. Update
    5. Getters
    6. Sort functions
    7. CSS stuff
--}

{-- 1. Model --}
type Element r e
    = R r
    | E e

type alias Model = Element Reminder Email

type alias Context =
    { actions : Signal.Address Action }

newReminder : String -> String -> Element Reminder Email
newReminder body created =
   R { body = body
     ,created = created
     ,done = False
     ,pinned = False
     ,snoozeDate ="2016-03-01"
     ,isSnoozed = False }

newEmail : String -> String -> String -> String -> Element Reminder Email
newEmail title from date body =
   E { from = from
     , to = "goodbye@test.me"
     , title = title
     , body = body
     , date = date
     ,done = False, pinned = False, more = False , snoozeDate ="2016-03-01", isSnoozed = False}


{-- returns true if two emails are the same in their basic attributes
    Reminder is not implented because it is not needed atm --}
compareModel: Model -> Model -> Bool
compareModel model1 model2 =
    case model1 of
      E e1 -> (case model2 of
               E e2 -> (emailCompare e1 e2)
               R r  -> False)
      R r  -> False


emailCompare: Email -> Email -> Bool
emailCompare email1 email2 =
   if email1.from == email2.from &&
      email1.to == email2.to &&
      email1.title == email2.title &&
      email1.body == email2.body &&
      email1.date == email2.date
    then True
    else False


{-- 2. View --}
elementView : Context-> Model -> Bool -> Bool -> Html
elementView context element isFocused isOverdue =
    case element of
     R r -> Html.li [getCorrectCss isFocused r.pinned ] [
                Html.div [] [
                  Html.p []
                     [ Html.text r.body]
                , taskButton context.actions Mark (if r.done then "Undo" else "Mark as Done")
                , taskButton context.actions Pin  (if r.pinned then "Unpin" else "Pin")
                ,if not r.done then
                 Html.input
                  [type' "date"
                  ,value (getSnoozeDate element)
                  ,E.on "input" E.targetValue (Signal.message context.actions << UpdateSnoozeDate)]
                            []
                  else Html.text ""
                ,if not r.done then
                 Html.button [ E.onClick context.actions Snooze ]
                      [ Html.text "Snooze" ]
                  else Html.text ""
                , Html.p []
                       [ Html.text <| "Date: " ++ r.created ]
                , if isOverdue
                     then Html.p [ combineAttributes [overdueStyle]]
                          [ Html.text <| "Overdue: Yes" ]
                  else Html.text ""
               ]]

     E e -> Html.li [if isFocused then combineAttributes [focusedStyle,borderStyle] else combineAttributes [emptyStyle,borderStyle]] [
                Html.div [if e.pinned then combineAttributes [pinnedStyle] else combineAttributes [emptyStyle]] [
                 Html.p [] [Html.text <| e.title ++ " | " ++ e.from]
               , Html.p []
                     [ Html.text <|
                       if not e.more then
                       String.left 200 e.body
                       else e.body]
               ,
               if String.length e.body > 200 then
                 taskButton context.actions More <| if e.more then "Less" else "More"
               else
                 Html.text ""
               , taskButton context.actions Mark (if e.done then "Undo" else "Mark as Done")
               , taskButton context.actions Pin  (if e.pinned then "Unpin" else "Pin")
               , Html.p []
                     [ Html.text <| "Date: " ++ e.date]
               ]]

taskButton address action text =
  Html.button [ E.onClick address action ]
    [ Html.text text ]


{-- 3. Actions --}
type Action = More
            | Mark
            | Pin
            | UpdateSnoozeDate String
            | Snooze

{-- 4. Update --}
update : Action -> Model -> Model
update action model =
  case action of
    More ->
     let changeVisibility t =
          if t.more then
            {t | more <- False}
          else  {t | more <- True}
     in
      case model of
       E e -> E <| changeVisibility e
       R r -> R r
    Mark ->
       let changeMark t =
            if t.done then
              {t | done <- False}
            else  {t | done <- True}
       in
        case model of
         E e -> E <| changeMark e
         R r -> R <| changeMark r
    Pin  ->
       let changePin t =
            if t.pinned then
              {t | pinned <- False}
            else  {t | pinned <- True}
       in
        case model of
         E e -> E <| changePin e
         R r -> R <| changePin r
    Snooze ->
      let changeSnooze t =
            if t.isSnoozed  then
              {t | isSnoozed <- False}
            else  {t | isSnoozed <- True}
       in
            case model of
             E e -> E <| changeSnooze e
             R r -> R <| changeSnooze r
    UpdateSnoozeDate string ->
       let changeSnoozeDate t =
              {t | snoozeDate <- string}
       in
        case model of
         E e -> E <| changeSnoozeDate e
         R r -> R <| changeSnoozeDate r

{-- 5. getters --}
getDate model =
    case model of
      R r -> r.created
      E e -> e.date

getPinned model =
    case model of
      R r -> r.pinned
      E e -> e.pinned

getDone model =
    case model of
      R r -> r.done
      E e -> e.done

getSnoozeDate model =
    case model of
      R r -> r.snoozeDate
      E e -> e.snoozeDate

getIsSnoozed model =
    case model of
      R r -> r.isSnoozed
      E e -> e.isSnoozed

{-- 6. Sort functions --}
{--ugly code alert--}
normalSort: Model -> Model -> Order
normalSort a b =
  if getDone a && not (getDone b)
    then GT
    else
  if not (getDone a) && getDone b
    then LT
    else
  if getPinned a && not (getPinned b)
    then LT
    else
  if not (getPinned a) && getPinned b
    then GT
    else
  if (getPinned a && getPinned b) || (not (getPinned a) && not (getPinned b))
      then if getDate a > getDate b
        then GT
        else LT
  else EQ

reverseSort: Model -> Model -> Order
reverseSort a b =
  if getDone a && not (getDone b)
    then GT
    else
  if not (getDone a) && getDone b
    then LT
    else
 if getDate a > getDate b
        then LT
        else GT

{-- 7. CSS stuff --}
getCorrectCss : Bool -> Bool -> Html.Attribute
getCorrectCss isFocused isPinned  =
          if isFocused && isPinned
            then combineAttributes [pinnedStyle, focusedStyle,borderStyle]
          else
          if isFocused
            then combineAttributes [focusedStyle,borderStyle]
          else
          if isPinned
            then combineAttributes [pinnedStyle,borderStyle]
          else combineAttributes [emptyStyle,borderStyle]

combineAttributes : List (List (String, String)) -> Html.Attribute
combineAttributes list = Attri.style <| combineAttributesAcc list

combineAttributesAcc : List (List(String,String)) -> List (String, String)
combineAttributesAcc list =
    case list of
      [] -> []
      stringTupples :: rest ->
        stringTupples ++ combineAttributesAcc rest


pinnedStyle : List (String, String)
pinnedStyle  = [("background-color","rgb(218,223,224)")]

focusedStyle : List (String, String)
focusedStyle =
    [ ("border-left-width", "thick"),
      ("border-left-style","double"),
      ("border-color","rgb(170,255,255)"),
      ("padding","10px 10px 20px ")]

overdueStyle : List (String, String)
overdueStyle =
    [ ("color", "red")]

borderStyle : List (String, String)
borderStyle =
    [ ("border-bottom", "thin dotted #666666")]

snoozedStyle : List (String, String)
snoozedStyle =
    [ ("visibility", "hidden")
    , ("height","0")]

emptyStyle : List (String, String)
emptyStyle = [ ]

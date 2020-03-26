module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, bool, field, map4, maybe, string)
import Json.Encode as Encode exposing (Value)
import Result exposing (Result)



-- MAIN


main =
  Browser.element
  { init = init
  , subscriptions = \_ -> Sub.none
  , update = update
  , view = view }

-- MODEL

type alias Model =
  { book : Book
  , books : List Book
  }

type alias Book =
  { name : String
  , author : String
  , isPirated : Bool
  , publication : Maybe String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model (Book "" "" False Nothing) [], getBooks)

modelToList : Book -> List (String, Value)
modelToList model =
    let
        x = [ ( "name", Encode.string model.name )
            , ( "author", Encode.string model.author )
            , ( "isPirated", Encode.bool model.isPirated )
            ]
    in
    case model.publication of
        Nothing -> x
        Just p -> x ++ [( "publication", Encode.string p )]

liftString : String -> Maybe String
liftString x =
    if x == "" then
        Nothing
    else
        Just x

dropStringFn : (Maybe String -> msg) -> String  -> msg
dropStringFn mf = mf << liftString



-- UPDATE


type Msg
  = Name String
  | Author String
  | IsPirated Bool
  | Publication (Maybe String)
  | Submit
  | ToggleIsPirated
  | GotBook (Result Http.Error Book)
  | GotBooks (Result Http.Error (List Book))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let book = model.book
  in
  case msg of
    Name name ->
      ({ model | book = {book | name = name} }, Cmd.none)

    Author author ->
      ({ model | book = {book | author = author} }, Cmd.none)

    IsPirated isPirated ->
      ({ model | book = {book | isPirated = isPirated} }, Cmd.none)

    Publication publication ->
      ({ model | book = {book | publication = publication} }, Cmd.none)

    Submit ->
      (model, createBook model.book)

    GotBook _ ->
      (model, getBooks)

    GotBooks result ->
      case result of
          Err _ -> (model, Cmd.none)
          Ok response -> ({model | books = response}, Cmd.none)

    ToggleIsPirated ->
      ({model | book = {book |isPirated = not book.isPirated}}, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.book.name Name
    , viewInput "text" "Author" model.book.author Author
    , viewOptionalInput "text" "Publication" model.book.publication Publication
    , checkbox ToggleIsPirated "Is Pirated"
    --, viewValidation model
    , button [ onClick Submit ] [ text "Submit" ]
    , ul [] (viewBookList model.books)
    ]

viewBookList : List Book -> List (Html msg)
viewBookList = List.map viewBook

viewBook : Book -> Html msg
viewBook book =
    div []
      [ text book.name
      , text book.author
      ]


checkbox : msg -> String -> Html msg
checkbox msg name =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg, required True ] []

viewOptionalInput : String -> String -> Maybe String -> (Maybe String -> msg) -> Html msg
viewOptionalInput t p mv mtoMsg =
  let
      x = case mv of
             Just s -> s
             Nothing -> ""
  in
  input [ type_ t, placeholder p, value x, onInput (dropStringFn mtoMsg) ] []

{-
viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
-}



-- HTTP

getBooks : Cmd Msg
getBooks = Http.get
                 { url = "http://localhost:3000/books"
                 , expect = Http.expectJson GotBooks (Decode.list bookDecoder)
                 }

createBook : Book -> Cmd Msg
createBook model = Http.post
                           { url = "http://localhost:3000/books"
                           , expect = Http.expectJson GotBook bookDecoder
                           , body = jsonBody (Encode.object (modelToList model))
                           }

bookDecoder : Decoder Book
bookDecoder =
    map4 Book (field "name" string) (field "author" string) (field "isPirated" bool) (maybe (field "publication" string))
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (jsonBody)
import Json.Decode as Decode exposing (Decoder, bool, field, map4, maybe, string)
import Json.Encode as Encode exposing (Value)



-- MAIN


main =
  Browser.element
  { init = init
  , subscriptions = \_ -> Sub.none
  , update = update
  , view = view }



-- MODEL

type alias Model =
  { name : String
  , author : String
  , isPirated : Bool
  , publication : Maybe String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model "" "" False Nothing, Http.get
                                      { url = "http://localhost:3000/books"
                                      , expect = Http.expectJson GotBooks booksDecoder
                                      })

modelToList : Model -> List (String, Value)
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
  | GotBook (Result Http.Error Model)
  | GotBooks (Result Http.Error (List Model))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Name name ->
      ({ model | name = name }, Cmd.none)

    Author author ->
      ({ model | author = author }, Cmd.none)

    IsPirated isPirated ->
      ({ model | isPirated = isPirated }, Cmd.none)

    Publication publication ->
      ({ model | publication = publication }, Cmd.none)

    Submit ->
      (model, createBook model)

    GotBook _ ->
      (model, Cmd.none)

    GotBooks _ ->
      (model, Cmd.none)

    ToggleIsPirated ->
      ({model | isPirated = not model.isPirated}, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "text" "Author" model.author Author
    , viewOptionalInput "text" "Publication" model.publication Publication
    , checkbox ToggleIsPirated "Is Pirated"
    --, viewValidation model
    , button [ onClick Submit ] [ text "Submit" ]
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

createBook : Model -> Cmd Msg
createBook model = Http.post
                           { url = "http://localhost:3000/books"
                           , expect = Http.expectJson GotBook bookDecoder
                           , body = jsonBody (Encode.object (modelToList model))
                           }

bookDecoder : Decoder Model
bookDecoder =
    map4 Model (field "name" string) (field "author" string) (field "isPirated" bool) (maybe (field "publication" string))

booksDecoder : Decoder (List Model)
booksDecoder =
    Decode.list bookDecoder
module Login exposing (..)

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
  { username : String
  , password : String
  , token : Maybe String
  }

modelToList : Model -> List (String, Value)
modelToList model =
 [ ( "username", Encode.string model.username )
 , ( "password", Encode.string model.password )
 ]


init : () -> (Model, Cmd Msg)
init _ = (Model "" "" Nothing, Cmd.none)


-- UPDATE


type Msg
  = Username String
  | Password String
  | Submit
  | LoggedIn (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Username name -> ({ model | username = name }, Cmd.none)
    Password password -> ({ model | password = password }, Cmd.none)
    Submit -> (model, signIn model)
    LoggedIn (Ok token) -> ({ model | token = Just token }, Cmd.none)
    LoggedIn _ -> (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Username" model.username Username
    , viewInput "text" "Password" model.password Password
    , button [ onClick Submit ] [ text "Submit" ]
    , viewOptionalInput model.token
    --, viewValidation model
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg, required True ] []

viewOptionalInput : Maybe String -> Html msg
viewOptionalInput mv =
    case mv of
        Just s -> text s
        Nothing -> text ""

{-
viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
-}



-- HTTP

signIn : Model -> Cmd Msg
signIn model = Http.post
               { url = "http://localhost:3000/login"
               , expect = Http.expectJson LoggedIn (field "token" string)
               , body = jsonBody (Encode.object (modelToList model))
               }


module Views.Editor.Header.View
    exposing
        ( Config
        , SaveOption(..)
        , view
        )

import Extra.Html as Html
import Html exposing (Html, button, div, h1, header, span, text)
import Html.Attributes exposing (attribute, disabled, id)
import Html.Events exposing (onClick)
import Shared.Icons as Icons
import Views.Editor.Header.Classes exposing (..)


type SaveOption
    = Save
    | Update
    | Fork
    | Saving


type alias Config msg =
    { compileButtonEnabled : Bool
    , embedLinkButtonEnabled : Bool
    , saveButtonEnabled : Bool
    , saveButtonOption : SaveOption
    , buttonsVisible : Bool
    , onSave : msg
    , onCompile : msg
    , onFormat : msg
    , onAbout : msg
    , onEmbedLink : msg
    }


viewLogo : Html msg
viewLogo =
    div [ class [ Logo ] ]
        [ text "Ellie" ]


viewButton : msg -> Bool -> Html msg -> String -> Html msg
viewButton clickMsg isDisabled icon label =
    button
        [ class [ Button ]
        , onClick clickMsg
        , disabled isDisabled
        , id label
        ]
        [ div [ class [ ButtonInner ] ]
            [ span [ class [ ButtonIcon ] ]
                [ icon ]
            , span [ class [ ButtonText ] ]
                [ text label ]
            ]
        ]


viewSaveButton : Config msg -> Html msg
viewSaveButton config =
    case config.saveButtonOption of
        Fork ->
            viewButton
                config.onSave
                (not config.saveButtonEnabled)
                Icons.forkRepo
                "Fork"

        Update ->
            viewButton
                config.onSave
                (not config.saveButtonEnabled)
                Icons.cloudOutline
                "Update"

        Save ->
            viewButton
                config.onSave
                (not config.saveButtonEnabled)
                Icons.cloudOutline
                "Save"

        Saving ->
            viewButton
                config.onSave
                True
                Icons.loading
                "Saving..."


viewCompileButton : Config msg -> Html msg
viewCompileButton config =
    viewButton
        config.onCompile
        (not config.compileButtonEnabled)
        Icons.playOutline
        "Compile"


viewFormatButton : Config msg -> Html msg
viewFormatButton config =
    viewButton
        config.onFormat
        False
        Icons.format
        "Format"


viewAboutButton : Config msg -> Html msg
viewAboutButton config =
    viewButton
        config.onAbout
        False
        Icons.lightning
        "About"


viewEmbedLinkButton : Config msg -> Html msg
viewEmbedLinkButton config =
    viewButton
        config.onEmbedLink
        (not config.embedLinkButtonEnabled)
        Icons.share
        "Share"


viewCollaborateButton : Html msg
viewCollaborateButton =
    button
        [ class [ Button ]
        , attribute "onclick" "TogetherJS(this); return false;"
        , disabled False
        ]
        [ div [ class [ ButtonInner ] ]
            [ span [ class [ ButtonIcon ] ]
                [ Icons.collaborate ]
            , span [ class [ ButtonText ] ]
                [ text "Collaborate" ]
            ]
        ]


view : Config msg -> Html msg
view config =
    header [ class [ Header ] ]
        [ div [ class [ HeaderGroup ] ] <|
            if config.buttonsVisible then
                [ viewLogo
                , viewCompileButton config
                , viewSaveButton config
                , viewFormatButton config
                , viewEmbedLinkButton config
                , viewCollaborateButton
                ]
            else
                [ viewLogo ]
        , div [ class [ HeaderGroup ] ]
            [ viewAboutButton config
            ]
        ]

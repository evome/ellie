port module Pages.Editor.Subs exposing (subscriptions)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Data.Ellie.KeyCombo as KeyCombo exposing (KeyCombo)
import Json.Decode as Decode exposing (Decoder, Value)
import Keyboard
import Pages.Editor.Layout.Subscriptions as Layout
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.Update.Save as UpdateSave


port windowUnloadedIn : (() -> msg) -> Sub msg


port online : (Bool -> msg) -> Sub msg


port jsError : (String -> msg) -> Sub msg


port compilerMessagesIn : (Value -> msg) -> Sub msg


port compileForSaveIn : (Value -> msg) -> Sub msg


keyCombos : Model -> Sub Msg
keyCombos model =
    Sub.batch
        [ Keyboard.downs
            (\code ->
                if Model.canCompile model && KeyCombo.controlShift model.keyCombo && code == 13 then
                    CompileRequested
                else
                    NoOp
            )
        , Sub.map KeyComboMsg <| KeyCombo.subscriptions
        ]


compilerMessages : Sub Msg
compilerMessages =
    let
        parse value =
            Decode.decodeValue CompileStage.decoder value
                |> Result.map CompileStageChanged
                |> Result.withDefault NoOp
    in
    compilerMessagesIn parse


compileForSave : Sub Msg
compileForSave =
    let
        parse value =
            let
                _ =
                    Debug.log "v" value
            in
            Decode.decodeValue CompileStage.decoder value
                |> Result.map
                    (\stage ->
                        case Debug.log "stage" stage of
                            Compiling { total, complete } ->
                                if complete == 0 then
                                    SaveMsg <| UpdateSave.CompileStarted total
                                else
                                    NoOp

                            Success url ->
                                SaveMsg <| UpdateSave.CompileCompleted (Ok url)

                            FinishedWithErrors errors ->
                                SaveMsg <| UpdateSave.CompileCompleted (Err errors)

                            Failed message ->
                                SaveMsg <| UpdateSave.CompileAborted message

                            _ ->
                                NoOp
                    )
                |> Result.withDefault NoOp
    in
    compileForSaveIn parse


clearNotifications : Model -> Sub Msg
clearNotifications model =
    if not (List.isEmpty model.notifications) then
        Keyboard.ups
            (\keyCode ->
                if keyCode == 27 then
                    ClearAllNotifications
                else
                    NoOp
            )
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ online OnlineChanged
        , jsError IframeJsError
        , compilerMessages
        , compileForSave
        , keyCombos model
        , clearNotifications model
        , Sub.map LayoutMsg <| Layout.subscriptions model.layout
        ]

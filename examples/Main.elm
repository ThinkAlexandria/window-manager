module Main exposing (main)

import Css
import Drag
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)
import Window
import WindowManager exposing (WindowLocation, WindowLayout, initWindowLayout, updateWindowDeltaX, updateWindowDeltaY, resetWindowResizeFences, viewWindow, onMouseDownTranslateWindow)
import Css.ThinkAlexandria.WindowManager.Common exposing (styleWindow)
import Css.ThinkAlexandria.WindowManager.Default exposing (defaultStyleConfig)
import Css.ThinkAlexandria.WindowManager.Selectors.Classes as Classes exposing (CssClasses(..))


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }



-- MODEL


type InteractionLocation
    = TerminalWindow WindowLocation
    | WidgetWindow Int WindowLocation


type alias Model =
    { viewport : { width : Int, height : Int }
    , layout : Layout
    , dragState : Drag.State InteractionLocation
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { dragState = Drag.init
            , layout =
                { app = { width = 1200, height = 800, minWidth = 640, minHeight = 480 }
                , terminal =
                    initWindowLayout
                        { width = 320
                        , height = 44
                        , left = 160
                        , top = 390
                        , minWidth = 100
                        , minHeight = 44
                        }
                , widget1 =
                    initWindowLayout
                        { width = 320
                        , height = 100
                        , left = 100
                        , top = 100
                        , minWidth = 100
                        , minHeight = 100
                        }
                , widget2 =
                    initWindowLayout
                        { width = 320
                        , height = 100
                        , left = 200
                        , top = 100
                        , minWidth = 100
                        , minHeight = 100
                        }
                , widget3 =
                    initWindowLayout
                        { width = 320
                        , height = 100
                        , left = 300
                        , top = 100
                        , minWidth = 100
                        , minHeight = 100
                        }
                }
            , viewport = { width = 640, height = 480 }
            }
    in
        ( model, Task.attempt processWindowSize Window.size )


processWindowSize : Result x Window.Size -> Msg
processWindowSize result =
    case result of
        Ok size ->
            ResizeWindow size

        Err _ ->
            NoOp


type alias Layout =
    { app :
        { width : Int
        , height : Int
        , minWidth : Int
        , minHeight : Int
        }
    , terminal : WindowLayout
    , widget1 : WindowLayout
    , widget2 : WindowLayout
    , widget3 : WindowLayout
    }



-- UPDATE


type Msg
    = NoOp
    | ResizeWindow Window.Size
    | DragMsg (Drag.Msg InteractionLocation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DragMsg dragMsg ->
            ( updateDragMsg dragMsg model, Cmd.none )

        ResizeWindow windowSize ->
            let
                _ =
                    3
            in
                ( { model | viewport = windowSize }, Cmd.none )


updateDragMsg : Drag.Msg InteractionLocation -> Model -> Model
updateDragMsg dragMsg model =
    case dragMsg of
        Drag.Start location _ ->
            { model | dragState = Drag.update dragMsg model.dragState }

        Drag.Moved location currentMousePosition ->
            let
                ( dx, dy ) =
                    Drag.delta dragMsg model.dragState

                updatedModel =
                    { model | dragState = Drag.update dragMsg model.dragState }
            in
                -- Dirty hack because we can only drag the terminal, so there
                -- is no need to match on the location tag
                case location of
                    TerminalWindow windowLocation ->
                        let
                            newTerminal =
                                model.layout.terminal
                                    |> updateWindowDeltaX windowLocation currentMousePosition dx
                                    |> updateWindowDeltaY windowLocation currentMousePosition dy

                            modelLayout =
                                model.layout
                        in
                            { updatedModel
                                | layout = syncAppSize { modelLayout | terminal = newTerminal }
                            }

                    WidgetWindow i windowLocation ->
                        case i of
                            1 ->
                                let
                                    newWidget1 =
                                        model.layout.widget1
                                            |> updateWindowDeltaX windowLocation currentMousePosition dx
                                            |> updateWindowDeltaY windowLocation currentMousePosition dy

                                    modelLayout =
                                        model.layout
                                in
                                    { updatedModel
                                        | layout =
                                            syncAppSize
                                                { modelLayout
                                                    | widget1 = newWidget1
                                                }
                                    }

                            2 ->
                                let
                                    newWidget2 =
                                        model.layout.widget2
                                            |> updateWindowDeltaX windowLocation currentMousePosition dx
                                            |> updateWindowDeltaY windowLocation currentMousePosition dy

                                    modelLayout =
                                        model.layout
                                in
                                    { updatedModel
                                        | layout =
                                            syncAppSize
                                                { modelLayout
                                                    | widget2 = newWidget2
                                                }
                                    }

                            3 ->
                                let
                                    newWidget3 =
                                        model.layout.widget3
                                            |> updateWindowDeltaX windowLocation currentMousePosition dx
                                            |> updateWindowDeltaY windowLocation currentMousePosition dy

                                    modelLayout =
                                        model.layout
                                in
                                    { updatedModel
                                        | layout =
                                            syncAppSize
                                                { modelLayout
                                                    | widget3 = newWidget3
                                                }
                                    }

                            _ ->
                                updatedModel

        Drag.End _ _ ->
            let
                modelLayout =
                    model.layout
            in
                { model
                    | dragState = Drag.update dragMsg model.dragState
                    , layout =
                        { modelLayout
                            | terminal =
                                resetWindowResizeFences model.layout.terminal
                        }
                }

        Drag.Click _ _ ->
            { model | dragState = Drag.update dragMsg model.dragState }


{-| -}
syncAppSize : Layout -> Layout
syncAppSize layout =
    let
        layoutApp =
            layout.app

        candidateWidth =
            layout.terminal.width + layout.terminal.left

        candidateHeight =
            layout.terminal.height + layout.terminal.top
    in
        { layout
            | app =
                { layoutApp
                    | width =
                        if candidateWidth < layout.app.minWidth then
                            -- Prevent it from shrinking all the way
                            layout.app.minWidth
                        else
                            candidateWidth
                    , height =
                        if candidateHeight < layout.app.minHeight then
                            -- Prevent it from shrinking all the way
                            layout.app.minHeight
                        else
                            candidateHeight
                }
        }



-- VIEW


terminalWindowConfig =
    { toMsg = DragMsg
    , toInteractionLocation = TerminalWindow
    , windowContainerClass = Classes.toString WindowContainer
    , leftResizeHorizontallyHandleClass = Classes.toString LeftResizeHandle
    , rightResizeHorizontallyHandleClass = Classes.toString RightResizeHandle
    , upperLeftCornerResizeHandleClass = Classes.toString UpperLeftResizeHandle
    , upperRightCornerResizeHandleClass = Classes.toString UpperRightResizeHandle
    , lowerLeftCornerResizeHandleClass = Classes.toString LowerLeftResizeHandle
    , lowerRightCornerResizeHandleClass = Classes.toString LowerRightResizeHandle
    , topResizeVerticallyHandleClass = Classes.toString TopResizeHandle
    , bottomResizeVerticallyHandleClass = Classes.toString BottomResizeHandle
    }


widgetWindowConfig i =
    { terminalWindowConfig | toInteractionLocation = WidgetWindow i }


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "relative" )
            , ( "height", (toString model.layout.app.height) ++ "px" )
            , ( "width", (toString model.layout.app.width) ++ "px" )
            , ( "background-color", "#f59" )
            ]
        ]
        [ Html.node "style"
            []
            [ (styleWindow defaultStyleConfig)
                |> Css.stylesheet
                |> List.singleton
                |> Css.compile
                |> .css
                |> text
            ]
        , viewDummyAppPage
        , viewWindow
            terminalWindowConfig
            model.layout.terminal
            [ div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "background-color", "#fff" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "min-height", "10px" )
                        , ( "background-color", "#95f" )
                        , ( "display", "flex" )
                        , ( "flex-grow", "0" )
                        , ( "flex-shrink", "0" )
                        , ( "flex-basis", "initial" )
                        , ( "justify-content", "space-between" )
                        ]
                    , onMouseDownTranslateWindow terminalWindowConfig
                    ]
                    [ span [] [ text "Terminal" ]
                    , span [] [ text "X" ]
                    ]
                , div
                    [ style
                        [ ( "background-color", "#fff" )
                        , ( "flex-grow", "1" )
                        , ( "overflow-y", "scroll" )
                        , ( "overflow-x", "hidden" )
                        ]
                    ]
                    [ input
                        [ style
                            [ ( "border", "none" )
                            , ( "width", "100%" )
                            ]
                        ]
                        []
                    ]
                ]
            ]
        , viewWindow
            (widgetWindowConfig 1)
            model.layout.widget1
            [ div
                [ style
                    [ ( "background-color", "#fff" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "overflow", "hidden" )
                    ]
                ]
                [ text "widget1"
                , div
                    [ style
                        [ ( "background-color", "#95f" )
                        , ( "width", "100px" )
                        , ( "height", "50px" )
                        , ( "margin-top", "50px" )
                        , ( "margin-left", "50px" )
                        ]
                    , onMouseDownTranslateWindow (widgetWindowConfig 1)
                    ]
                    [ text "window drag handle" ]
                ]
            ]
        , viewWindow
            (widgetWindowConfig 2)
            model.layout.widget2
            [ div
                [ style
                    [ ( "background-color", "#fff" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "overflow", "hidden" )
                    ]
                ]
                [ text "widget2"
                , div
                    [ style
                        [ ( "background-color", "#95f" )
                        , ( "width", "100px" )
                        , ( "height", "50px" )
                        , ( "margin-top", "50px" )
                        , ( "margin-left", "50px" )
                        ]
                    , onMouseDownTranslateWindow (widgetWindowConfig 2)
                    ]
                    [ text "window drag handle" ]
                ]
            ]
        , viewWindow
            (widgetWindowConfig 3)
            model.layout.widget3
            [ div
                [ style
                    [ ( "background-color", "#fff" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "overflow", "hidden" )
                    ]
                ]
                [ text "widget3"
                , div
                    [ style
                        [ ( "background-color", "#95f" )
                        , ( "width", "100px" )
                        , ( "height", "50px" )
                        , ( "margin-top", "50px" )
                        , ( "margin-left", "50px" )
                        ]
                    , onMouseDownTranslateWindow (widgetWindowConfig 3)
                    ]
                    [ text "window drag handle" ]
                ]
            ]
        ]


viewDummyAppPage : Html msg
viewDummyAppPage =
    div
        [ style
            [ ( "background-color", "#f59" )
            , ( "height", "100%" )
            , ( "width", "100%" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "z-index", "-1" )
            ]
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes ResizeWindow
        , Sub.map DragMsg (Drag.subscriptions model.dragState)
        ]

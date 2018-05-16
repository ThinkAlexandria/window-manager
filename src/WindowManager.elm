module WindowManager
    exposing
        ( WindowLocation
        , WindowLayout
        , initWindowLayout
        , updateWindowDeltaX
        , updateWindowDeltaY
        , resetWindowResizeFences
        , Config
        , viewWindow
        , onMouseDownTranslateWindow
        )

{-| @docs WindowLayout, initWindowLayout

@docs updateWindowDeltaX, updateWindowDeltaY, resetWindowResizeFences

@docs Config, viewWindow, onMouseDownTranslateWindow

@docs WindowLocation

-}

import Drag
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)


-- MODEL


{-| -}
type WindowLocation
    = LeftResizeHorizontallyHandle
    | RightResizeHorizontallyHandle
    | UpperLeftCornerResizeHandle
    | UpperRightCornerResizeHandle
    | LowerLeftCornerResizeHandle
    | LowerRightCornerResizeHandle
    | TopResizeVerticallyHandle
    | BottomResizeVerticallyHandle
    | DragHandle


{-| -}
type alias WindowLayout =
    { width : Int
    , height : Int
    , top : Int
    , left : Int
    , minWidth : Int
    , minHeight : Int
    , resizeXFence : FenceRule
    , resizeYFence : FenceRule
    }


{-| -}
initWindowLayout :
    { width : Int, height : Int, top : Int, left : Int, minWidth : Int, minHeight : Int }
    -> WindowLayout
initWindowLayout config =
    { width = config.width
    , height = config.height
    , top = config.top
    , left = config.left
    , minWidth = config.minWidth
    , minHeight = config.minHeight
    , resizeXFence = Allow
    , resizeYFence = Allow
    }


{-| -}
type FenceRule
    = RejectGreaterThan Int
    | RejectLessThan Int
    | Allow


type alias Position =
    { x : Int, y : Int }



-- When we hit minimum size, then save the mouse position that corresponds to
-- that limit, and then modify the reported deltas, so we can discard any
-- changes until the mouse position has passed back through the fence.


type alias ResizeMouseFence =
    { x : FenceRule
    , y : FenceRule
    }


{-| -}
resetWindowResizeFences : WindowLayout -> WindowLayout
resetWindowResizeFences window =
    { window | resizeXFence = Allow, resizeYFence = Allow }


{-| -}
updateWindowDeltaX : WindowLocation -> Position -> Int -> WindowLayout -> WindowLayout
updateWindowDeltaX location currentMousePosition dragDeltaX window =
    case fencedWindowDeltaX dragDeltaX currentMousePosition window of
        Just dx ->
            case location of
                LeftResizeHorizontallyHandle ->
                    resizeWindowLeft dx currentMousePosition window

                RightResizeHorizontallyHandle ->
                    resizeWindowRight dx currentMousePosition window

                UpperLeftCornerResizeHandle ->
                    resizeWindowLeft dx currentMousePosition window

                UpperRightCornerResizeHandle ->
                    resizeWindowRight dx currentMousePosition window

                LowerLeftCornerResizeHandle ->
                    resizeWindowLeft dx currentMousePosition window

                LowerRightCornerResizeHandle ->
                    resizeWindowRight dx currentMousePosition window

                DragHandle ->
                    translateWindowX dx currentMousePosition window

                _ ->
                    window

        Nothing ->
            window


{-| -}
updateWindowDeltaY : WindowLocation -> Position -> Int -> WindowLayout -> WindowLayout
updateWindowDeltaY location currentMousePosition dragDeltaY window =
    case fencedWindowDeltaY dragDeltaY currentMousePosition window of
        Just dy ->
            case location of
                TopResizeVerticallyHandle ->
                    resizeWindowTop dy currentMousePosition window

                BottomResizeVerticallyHandle ->
                    resizeWindowBottom dy currentMousePosition window

                UpperLeftCornerResizeHandle ->
                    resizeWindowTop dy currentMousePosition window

                UpperRightCornerResizeHandle ->
                    resizeWindowTop dy currentMousePosition window

                LowerLeftCornerResizeHandle ->
                    resizeWindowBottom dy currentMousePosition window

                LowerRightCornerResizeHandle ->
                    resizeWindowBottom dy currentMousePosition window

                DragHandle ->
                    translateWindowY dy currentMousePosition window

                _ ->
                    window

        Nothing ->
            window


fencedWindowDeltaX : Int -> Position -> WindowLayout -> Maybe Int
fencedWindowDeltaX dx mousePosition window =
    case window.resizeXFence of
        Allow ->
            Just dx

        RejectLessThan x ->
            if mousePosition.x > x then
                Just (mousePosition.x - x)
            else
                Nothing

        RejectGreaterThan x ->
            if mousePosition.x < x then
                Just (mousePosition.x - x)
            else
                Nothing


fencedWindowDeltaY : Int -> Position -> WindowLayout -> Maybe Int
fencedWindowDeltaY dy mousePosition window =
    case window.resizeYFence of
        Allow ->
            Just dy

        RejectLessThan y ->
            if mousePosition.y > y then
                Just (mousePosition.y - y)
            else
                Nothing

        RejectGreaterThan y ->
            if mousePosition.y < y then
                Just (mousePosition.y - y)
            else
                Nothing


translateWindowX : Int -> Position -> WindowLayout -> WindowLayout
translateWindowX dx currentMousePosition window =
    -- Clamp left at zero to prevent resize handle from being trapped
    -- outside of browser viewport
    if window.left + dx < 0 then
        { window
            | left = 0
            , resizeXFence = RejectLessThan (currentMousePosition.x - window.left - dx)
        }
    else
        { window
            | left = window.left + dx
            , resizeXFence = Allow
        }


translateWindowY : Int -> Position -> WindowLayout -> WindowLayout
translateWindowY dy currentMousePosition window =
    if window.top + dy < 0 then
        { window
            | top = 0
            , resizeYFence = RejectLessThan (currentMousePosition.y - window.top - dy)
        }
    else
        { window
            | top = window.top + dy
            , resizeYFence = Allow
        }


resizeWindowLeft : Int -> Position -> WindowLayout -> WindowLayout
resizeWindowLeft dx currentMousePosition window =
    -- Clamp left at zero to prevent resize handle from being trapped
    -- outside of browser viewport
    if (window.width - dx) < window.minWidth then
        -- compute the minimal update
        let
            -- Prevent decreasing width if for some
            -- reason the minWidth is larger than
            -- the actual width
            validDeltaX =
                (window.width - window.minWidth)
        in
            if validDeltaX < 0 then
                window
            else
                { window
                    | left = window.left + validDeltaX
                    , width = window.width - validDeltaX
                    , resizeXFence = RejectGreaterThan (currentMousePosition.x - dx + validDeltaX)
                }
    else if window.left + dx < 0 then
        { window
            | left = 0
            , width = window.width + (window.left - 0)
            , resizeXFence = RejectLessThan (currentMousePosition.x - window.left - dx)
        }
    else
        { window
            | left = window.left + dx
            , width = window.width - dx
            , resizeXFence = Allow
        }


resizeWindowRight : Int -> Position -> WindowLayout -> WindowLayout
resizeWindowRight dx currentMousePosition window =
    -- Allow terminal to resize the parent container to fit to prevent
    -- resize handle from being trapped outside of browser viewport
    if (window.width + dx) < window.minWidth then
        { window
            | width = window.minWidth
            , resizeXFence = RejectLessThan (currentMousePosition.x - ((window.width + dx) - window.minWidth))
        }
    else
        { window
            | width = window.width + dx
            , resizeXFence = Allow
        }


resizeWindowTop : Int -> Position -> WindowLayout -> WindowLayout
resizeWindowTop dy currentMousePosition window =
    -- Clamp top at zero to prevent resize handle from being trapped
    -- outside of browser viewport
    if (window.height - dy) < window.minHeight then
        -- compute the minimal update
        let
            -- Prevent decreasing height if for some
            -- reason the minHeight is larger than
            -- the actual height
            validDeltaY =
                (window.height - window.minHeight)
        in
            if validDeltaY < 0 then
                window
            else
                { window
                    | top = window.top + validDeltaY
                    , height = window.height - validDeltaY
                    , resizeYFence = RejectGreaterThan (currentMousePosition.y - dy + validDeltaY)
                }
    else if window.top + dy < 0 then
        { window
            | top = 0
            , height = window.height + (window.top - 0)
            , resizeYFence = RejectLessThan (currentMousePosition.y - window.top - dy)
        }
    else
        { window
            | top = window.top + dy
            , height = window.height - dy
            , resizeYFence = Allow
        }


resizeWindowBottom : Int -> Position -> WindowLayout -> WindowLayout
resizeWindowBottom dy currentMousePosition window =
    if (window.height + dy) < window.minHeight then
        { window
            | height = window.minHeight
            , resizeYFence = RejectLessThan (currentMousePosition.y - ((window.height + dy) - window.minHeight))
        }
    else
        { window
            | height = window.height + dy
            , resizeYFence = Allow
        }



-- VIEW


{-| -}
type alias Config msg interactionLocation =
    { toMsg : Drag.Msg interactionLocation -> msg
    , toInteractionLocation : WindowLocation -> interactionLocation

    -- Styling
    , windowContainerClass : String
    , leftResizeHorizontallyHandleClass : String
    , rightResizeHorizontallyHandleClass : String
    , upperLeftCornerResizeHandleClass : String
    , upperRightCornerResizeHandleClass : String
    , lowerLeftCornerResizeHandleClass : String
    , lowerRightCornerResizeHandleClass : String
    , topResizeVerticallyHandleClass : String
    , bottomResizeVerticallyHandleClass : String
    }


{-| -}
onMouseDownTranslateWindow : Config msg interactionLocation -> Attribute msg
onMouseDownTranslateWindow config =
    Html.Attributes.map
        config.toMsg
        (Drag.onMouseDown
            { stopPropagation = True, preventDefault = True }
            (config.toInteractionLocation DragHandle)
        )


{-| -}
viewWindow :
    Config msg interactionLocation
    -> WindowLayout
    -> List (Html msg)
    -> Html msg
viewWindow config window contents =
    let
        handleOnMouseDown : WindowLocation -> Attribute msg
        handleOnMouseDown location =
            Html.Attributes.map
                config.toMsg
                (Drag.onMouseDown
                    { stopPropagation = True, preventDefault = True }
                    (config.toInteractionLocation location)
                )
    in
        div
            [ style
                [ ( "height", (toString window.height) ++ "px" )
                , ( "width", (toString window.width) ++ "px" )
                , ( "top", (toString window.top) ++ "px" )
                , ( "left", (toString window.left) ++ "px" )
                ]
            , class config.windowContainerClass
            ]
            ([ div
                [ class config.upperLeftCornerResizeHandleClass
                , handleOnMouseDown UpperLeftCornerResizeHandle
                ]
                []
             , div
                [ class config.topResizeVerticallyHandleClass
                , handleOnMouseDown TopResizeVerticallyHandle
                ]
                []
             , div
                [ class config.upperRightCornerResizeHandleClass
                , handleOnMouseDown UpperRightCornerResizeHandle
                ]
                []
             , div
                [ class config.leftResizeHorizontallyHandleClass
                , handleOnMouseDown LeftResizeHorizontallyHandle
                ]
                []
             , div
                [ class config.rightResizeHorizontallyHandleClass
                , handleOnMouseDown RightResizeHorizontallyHandle
                ]
                []
             , div
                [ class config.lowerLeftCornerResizeHandleClass
                , handleOnMouseDown LowerLeftCornerResizeHandle
                ]
                []
             , div
                [ class config.bottomResizeVerticallyHandleClass
                , handleOnMouseDown BottomResizeVerticallyHandle
                ]
                []
             , div
                [ class config.lowerRightCornerResizeHandleClass
                , handleOnMouseDown LowerRightCornerResizeHandle
                ]
                []
             ]
                ++ contents
            )

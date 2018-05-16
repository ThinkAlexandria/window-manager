module Css.ThinkAlexandria.WindowManager.Common exposing (WindowStyleConfig, WindowSelectorConfig, styleWindow)

import Css exposing (..)


type alias WindowStyleConfig =
    { borderRightWidth : Float
    , borderLeftWidth : Float
    , borderTopWidth : Float
    , borderBottomWidth : Float

    -- Colors
    , borderRightColor : String
    , borderLeftColor : String
    , borderTopColor : String
    , borderBottomColor : String

    -- Radius
    , borderTopLeftRadius : Float
    , borderTopRightRadius : Float
    , borderBottomRightRadius : Float
    , borderBottomLeftRadius : Float

    -- Padding
    , paddingRight : Float
    , paddingLeft : Float
    , paddingTop : Float
    , paddingBottom : Float
    , cornerHandleHeight : Float
    , windowBackgroundColor : String
    , selectors : WindowSelectorConfig
    }


type alias WindowSelectorConfig =
    { windowContainerClass : String
    , leftResizeHorizontallyHandleClass : String
    , rightResizeHorizontallyHandleClass : String
    , upperLeftCornerResizeHandleClass : String
    , upperRightCornerResizeHandleClass : String
    , lowerLeftCornerResizeHandleClass : String
    , lowerRightCornerResizeHandleClass : String
    , topResizeVerticallyHandleClass : String
    , bottomResizeVerticallyHandleClass : String
    }


styleWindow : WindowStyleConfig -> List Snippet
styleWindow config =
    [ class config.selectors.windowContainerClass
        [ position absolute
        , boxSizing borderBox
        , padding4
            (px (config.paddingTop + config.borderTopWidth))
            (px (config.paddingRight + config.borderRightWidth))
            (px (config.paddingBottom + config.borderBottomWidth))
            (px (config.paddingLeft + config.borderLeftWidth))
        , borderRadius4
            (px config.borderTopLeftRadius)
            (px config.borderTopRightRadius)
            (px config.borderBottomRightRadius)
            (px config.borderBottomLeftRadius)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.upperLeftCornerResizeHandleClass
        [ position absolute
        , boxSizing borderBox
        , top zero
        , left zero
        , width (px (config.paddingLeft + config.borderLeftWidth))
        , height (px (config.cornerHandleHeight))
        , borderLeft3 (px config.borderLeftWidth) solid (hex config.borderLeftColor)
        , borderTop3 (px config.borderTopWidth) solid (hex config.borderTopColor)
        , borderTopLeftRadius (px config.borderTopLeftRadius)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.topResizeVerticallyHandleClass
        [ position absolute
        , boxSizing borderBox
        , top zero
        , left (px (config.paddingLeft + config.borderLeftWidth))
        , right (px (config.paddingRight + config.borderRightWidth))
        , borderTop3 (px config.borderTopWidth) solid (hex config.borderTopColor)
        , height (px (config.paddingTop + config.borderTopWidth))
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.upperRightCornerResizeHandleClass
        [ position absolute
        , boxSizing borderBox
        , top zero
        , right zero
        , width (px (config.paddingRight + config.borderRightWidth))
        , height (px config.cornerHandleHeight)
        , borderRight3 (px config.borderRightWidth) solid (hex config.borderRightColor)
        , borderTop3 (px config.borderTopWidth) solid (hex config.borderTopColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.leftResizeHorizontallyHandleClass
        [ position absolute
        , boxSizing borderBox
        , top (px config.cornerHandleHeight)
        , bottom (px config.cornerHandleHeight)
        , left zero
        , width (px (config.paddingLeft + config.borderLeftWidth))
        , borderLeft3 (px config.borderLeftWidth) solid (hex config.borderLeftColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.rightResizeHorizontallyHandleClass
        [ position absolute
        , boxSizing borderBox
        , top (px config.cornerHandleHeight)
        , bottom (px config.cornerHandleHeight)
        , right zero
        , width (px (config.paddingRight + config.borderRightWidth))
        , borderRight3 (px config.borderRightWidth) solid (hex config.borderRightColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.lowerLeftCornerResizeHandleClass
        [ position absolute
        , boxSizing borderBox
        , bottom zero
        , left zero
        , width (px (config.paddingLeft + config.borderLeftWidth))
        , height (px config.cornerHandleHeight)
        , borderLeft3 (px config.borderLeftWidth) solid (hex config.borderLeftColor)
        , borderBottom3 (px config.borderBottomWidth) solid (hex config.borderBottomColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.bottomResizeVerticallyHandleClass
        [ position absolute
        , boxSizing borderBox
        , bottom zero
        , left (px (config.paddingLeft + config.borderLeftWidth))
        , right (px (config.paddingRight + config.borderRightWidth))
        , height (px (config.paddingBottom + config.borderBottomWidth))
        , borderBottom3 (px config.borderBottomWidth) solid (hex config.borderBottomColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    , class config.selectors.lowerRightCornerResizeHandleClass
        [ position absolute
        , boxSizing borderBox
        , bottom zero
        , right zero
        , width (px (config.paddingRight + config.borderRightWidth))
        , height (px config.cornerHandleHeight)
        , borderRight3 (px config.borderRightWidth) solid (hex config.borderRightColor)
        , borderBottom3 (px config.borderBottomWidth) solid (hex config.borderBottomColor)
        , backgroundColor (hex config.windowBackgroundColor)
        ]
    ]

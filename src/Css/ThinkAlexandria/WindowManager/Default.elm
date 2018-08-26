module Css.ThinkAlexandria.WindowManager.Default exposing (defaultSelectorConfig, defaultStyleConfig)

{-|

@docs defaultSelectorConfig, defaultStyleConfig

-}

import Css.ThinkAlexandria.WindowManager.Common exposing (WindowSelectorConfig, WindowStyleConfig)
import Css.ThinkAlexandria.WindowManager.Selectors.Classes as Classes exposing (CssClasses(..))


{-| Garishly colored Window configuration best used for debugging.
-}
defaultStyleConfig : WindowStyleConfig
defaultStyleConfig =
    { borderRightWidth = 2
    , borderLeftWidth = 2
    , borderTopWidth = 2
    , borderBottomWidth = 2

    -- Colors
    , borderRightColor = "#707"
    , borderLeftColor = "#707"
    , borderTopColor = "#707"
    , borderBottomColor = "#707"

    -- Radius
    , borderTopLeftRadius = 2
    , borderTopRightRadius = 2
    , borderBottomRightRadius = 2
    , borderBottomLeftRadius = 2

    -- Padding
    , paddingRight = 5
    , paddingLeft = 5
    , paddingTop = 5
    , paddingBottom = 5
    , cornerHandleHeight = 15
    , windowBackgroundColor = "#0f0"
    , selectors = defaultSelectorConfig
    }


{-| Default css selectors.
-}
defaultSelectorConfig : WindowSelectorConfig
defaultSelectorConfig =
    { windowContainerClass = Classes.toString WindowContainer
    , leftResizeHorizontallyHandleClass = Classes.toString LeftResizeHandle
    , rightResizeHorizontallyHandleClass = Classes.toString RightResizeHandle
    , upperLeftCornerResizeHandleClass = Classes.toString UpperLeftResizeHandle
    , upperRightCornerResizeHandleClass = Classes.toString UpperRightResizeHandle
    , lowerLeftCornerResizeHandleClass = Classes.toString LowerLeftResizeHandle
    , lowerRightCornerResizeHandleClass = Classes.toString LowerRightResizeHandle
    , topResizeVerticallyHandleClass = Classes.toString TopResizeHandle
    , bottomResizeVerticallyHandleClass = Classes.toString BottomResizeHandle
    }

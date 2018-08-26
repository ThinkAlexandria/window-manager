module Css.ThinkAlexandria.WindowManager.Selectors.Classes exposing (toString, CssClasses(..))

{-|

@docs toString, CssClasses

-}


{-| Debug implementation of toString for CssClasses. Converts the type name into a string
-}
toString : CssClasses -> String
toString a =
    case a of
        WindowContainer ->
            "WindowContainer"

        LeftResizeHandle ->
            "LeftResizeHandle"

        RightResizeHandle ->
            "RightResizeHandle"

        UpperLeftResizeHandle ->
            "UpperLeftResizeHandle"

        UpperRightResizeHandle ->
            "UpperRightResizeHandle"

        LowerLeftResizeHandle ->
            "LowerLeftResizeHandle"

        LowerRightResizeHandle ->
            "LowerRightResizeHandle"

        TopResizeHandle ->
            "TopResizeHandle"

        BottomResizeHandle ->
            "BottomResizeHandle"

        TranslateHandle ->
            "TranslateHandle"


{-| The CSS classes for styling the window
-}
type CssClasses
    = WindowContainer
    | LeftResizeHandle
    | RightResizeHandle
    | UpperLeftResizeHandle
    | UpperRightResizeHandle
    | LowerLeftResizeHandle
    | LowerRightResizeHandle
    | TopResizeHandle
    | BottomResizeHandle
    | TranslateHandle

module Css.ThinkAlexandria.WindowManager.Selectors.Classes exposing (CssClasses(..), toString)


toString : CssClasses -> String
toString a =
    Basics.toString a


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

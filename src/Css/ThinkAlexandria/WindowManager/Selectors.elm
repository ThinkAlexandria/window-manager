module Css.ThinkAlexandria.WindowManager.Selectors exposing (CssClasses, class, classList)

{-| This elm module provides strongly typed selectors for

@docs CssClasses, class, classList

-}

import Css.ThinkAlexandria.WindowManager.Selectors.Classes as Classes
import Html
import Html.Attributes


{-| The CSS classes for styling the window chrome
-}
type alias CssClasses =
    Classes.CssClasses


{-| Helper lets you keep type checking of selectors all the way to call site.
-}
class : CssClasses -> Html.Attribute msg
class =
    Classes.toString >> Html.Attributes.class


{-| Helper lets you keep type checking of selectors all the way to call site.
-}
classList : List ( CssClasses, Bool ) -> Html.Attribute msg
classList names =
    names
        |> List.map (\( c, b ) -> ( Classes.toString c, b ))
        |> Html.Attributes.classList


pub enum CssClasses {
    WindowContainer,
    LeftResizeHandle,
    RightResizeHandle,
    UpperLeftResizeHandle,
    UpperRightResizeHandle,
    LowerLeftResizeHandle,
    LowerRightResizeHandle,
    TopResizeHandle,
    BottomResizeHandle,
    TranslateHandle,
}

pub enum CssIds {}

include!(concat!(
    env!("OUT_DIR"),
    "/elm_window_manager_selectors_classes_impl.rs"
));

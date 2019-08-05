/// Known issue: if multiple colored pieces of text are stacked they interfer with each other
use std::fmt::{Debug, Display};

pub trait AnsiCode {
    /// maps self to the corresponding ansi code
    fn to_ansi(&self) -> String;
}

/// Text colors
pub enum Color {
    Black,
    DarkRed,
    DarkGreen,
    Grey,
    White,
    Green,
    Red,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    Rgb(u8, u8, u8),
    Default,
}

impl AnsiCode for Color {
    fn to_ansi(&self) -> String {
        match self {
            Color::Black => "30".to_string(),
            Color::DarkRed => "31".to_string(),
            Color::DarkGreen => "32".to_string(),
            Color::Grey => "37".to_string(),
            Color::Rgb(r, g, b) => format!("38;2;{};{};{}", r, g, b),
            Color::Default => "39".to_string(),
            Color::Red => "91".to_string(),
            Color::Green => "92".to_string(),
            Color::Yellow => "93".to_string(),
            Color::Blue => "94".to_string(),
            Color::Magenta => "95".to_string(),
            Color::Cyan => "96".to_string(),
            Color::White => "97".to_string(),
        }
    }
}

/// Various ansi code text modifiers
pub enum Modifier {
    Reset,
    Bold,
    Faint,
    Italic,
    Underline,
    SlowBlink,
    RapidBlink,
    Reverse,
    CrossedOut,
    DoubleUnderline,
}

impl AnsiCode for Modifier {
    fn to_ansi(&self) -> String {
        match self {
            Modifier::Reset => "0",
            Modifier::Bold => "1",
            Modifier::Faint => "2",
            Modifier::Italic => "3",
            Modifier::Underline => "4",
            Modifier::SlowBlink => "5",
            Modifier::RapidBlink => "6",
            Modifier::Reverse => "7",
            Modifier::CrossedOut => "9",
            Modifier::DoubleUnderline => "21",
        }
        .to_string()
    }
}

/// Background color
pub enum BgColor {
    Black,
    DarkRed,
    DarkGreen,
    Grey,
    White,
    Green,
    Red,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    Rgb(u8, u8, u8),
    Default,
}

impl AnsiCode for BgColor {
    fn to_ansi(&self) -> String {
        match self {
            BgColor::Black => "40".to_string(),
            BgColor::DarkRed => "41".to_string(),
            BgColor::DarkGreen => "42".to_string(),
            BgColor::Grey => "47".to_string(),
            BgColor::Rgb(r, g, b) => format!("48;2;{};{};{}", r, g, b),
            BgColor::Default => "49".to_string(),
            BgColor::Red => "101".to_string(),
            BgColor::Green => "102".to_string(),
            BgColor::Yellow => "103".to_string(),
            BgColor::Blue => "104".to_string(),
            BgColor::Magenta => "105".to_string(),
            BgColor::Cyan => "106".to_string(),
            BgColor::White => "107".to_string(),
        }
    }
}

/// formats the items provided via the format_str and applies the modifiers from modifier_vec
/// or alternatively just applies modifiers to a provided string
#[macro_export]
macro_rules! colored {
    ( $string:expr, $modifier_vec:expr) => {
        format!(
            "\x1b[{}m{}\x1b[0m",
            $modifier_vec.into_iter().map(|elem| elem.to_ansi())
                .collect::<Vec<String>>().join(";"),
            $string
        )
    };
    ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
        format!(
            "\x1b[{}m{}\x1b[0m",
            $modifier_vec.into_iter().map(|elem| elem.to_ansi())
                .collect::<Vec<String>>().join(";"),
            format!($format_str, $( $item )*)
        )
    };
}

#[macro_export]
macro_rules! print_colored {
    ( $format_str:expr, $modifier_vec:expr) => {
        print!("{}", colored!($format_str, $modifier_vec));
    };
    ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
        print!("{}", colored!($format_str, $modifier_vec, $( $item )*));
    };
}

#[macro_export]
macro_rules! println_colored {
    ( $format_str:expr, $modifier_vec:expr) => {
        println!("{}", colored!($format_str, $modifier_vec));
    };
    ( $format_str:expr, $modifier_vec:expr, $($item:expr),* ) => {
        println!("{}", colored!($format_str, $modifier_vec, $( $item )*));
    };
}

/// Create a vector of AnsiCode Trait objects
#[macro_export]
macro_rules! params {
    ( $($item:expr),* ) => {
        {
            #[allow(unused_mut)]
            let mut v: Vec<&dyn AnsiCode> = Vec::new();
            $(
                v.push(&$item);
            )*
            v
        }
    }
}

/// Log the success of some event
pub fn success<T: Debug, S: Display>(message: S, element: Option<T>) {
    if let Some(element) = element {
        println!(
            "{} {} {}",
            colored!("{}", vec!(Color::Green), '✔'),
            message,
            colored!("{:?}", params!(Color::Green, Modifier::Italic), element)
        );
    } else {
        println!("{} {}", colored!("{}", vec!(Color::Green), '✔'), message);
    }
}

/// Log an error of some kind
pub fn error<T: Debug, S: Display>(message: S, element: Option<T>) {
    if let Some(element) = element {
        println!(
            "{} {} {}",
            colored!("{}", vec!(&Color::Red), '✗'),
            message,
            colored!("{:?}", params!(Color::Red, Modifier::Italic), element)
        );
    } else {
        println!("{} {}", colored!("{}", vec!(&Color::Red), '✗'), message);
    }
}

/// Clear the terminal
pub fn clear_screen() {
    println!("\x1b2J");
}

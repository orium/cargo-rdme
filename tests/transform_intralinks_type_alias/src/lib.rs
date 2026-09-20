//! * [Alignment](Alignment)
//! * [Alignment::Left](Alignment::Left)
//! * [HorizontalAlignment](HorizontalAlignment)
//! * [HorizontalAlignment::Left](HorizontalAlignment::Left)
//! * [Shape](Shape)
//! * [Shape::width](Shape::width)
//! * [Shape::area](Shape::area)
//! * [Rectangle::width](Rectangle::width)
//! * [Rectangle::area](Rectangle::area)
//! * [Color::Red](Color::Red)

pub type Alignment = HorizontalAlignment;

pub enum HorizontalAlignment {
    Left,
    Center,
    Right,
}

pub type Shape = Rectangle;

pub struct Rectangle {
    pub width: u32,
}

impl Rectangle {
    pub fn area(&self) -> u32 {
        self.width * self.width
    }
}

type PrivateAlias = Color;

pub enum Color {
    Red,
}

fn paint(_: PrivateAlias) {}

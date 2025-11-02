//! [a MyStruct](crate::a::MyStruct)
//! [skip](crate::a::Skip)
//! [b MyStruct](crate::b::MyStruct)
//! [skip](crate::b::MyStructTest)
//! [c MyStruct](crate::c::MyStruct)
//! [skip](crate::c::MyStructTest)
//! [d Same is a trait](crate::d::Same)

#[cfg(not(foo))]
pub mod a {
    pub struct MyStruct {}
}

#[cfg(foo)]
pub mod a {
    pub struct Skip {}
}

#[cfg(not(test))]
pub mod b {
    pub struct MyStruct {}
}

#[cfg(test)]
pub mod b {
    pub struct MyStructTest {}
}

#[cfg(test)]
pub mod c {
    pub struct MyStructTest {}
}

#[cfg(not(test))]
pub mod c {
    pub struct MyStruct {}
}

#[cfg(test)]
pub mod d {
    pub struct Same {}
}

#[cfg(not(test))]
pub mod d {
    pub trait Same {}
}

fn main() {}

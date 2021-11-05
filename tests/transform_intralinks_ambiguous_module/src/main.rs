//! [a MyStruct](crate::a::MyStruct)
//! [skip](crate::a::Skip)
//! [b MyStruct](crate::b::MyStruct)
//! [skip](crate::b::MyStructTest)
//! [c MyStruct](crate::c::MyStruct)
//! [skip](crate::c::MyStructTest)
//! [d Same is a trait](crate::d::Same)

#[cfg(not(foo))]
mod a {
  struct MyStruct {}
}

#[cfg(foo)]
mod a {
  struct Skip {}
}

#[cfg(not(test))]
mod b {
  struct MyStruct {}
}

#[cfg(test)]
mod b {
  struct MyStructTest {}
}

#[cfg(test)]
mod c {
  struct MyStructTest {}
}

#[cfg(not(test))]
mod c {
  struct MyStruct {}
}

#[cfg(test)]
mod d {
  struct Same {}
}

#[cfg(not(test))]
mod d {
  trait Same {}
}

fn main() {}

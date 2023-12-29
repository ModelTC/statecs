use statecs::*;
#[system]
fn example_system(a: i32, b: u32) -> String {
    std::format!("({a}, {b})")
}
fn main() {
    let (res, _): (String, _) = example_system((233i32, 666u32)).take();
    println!("{res}");
}

use crate::galois_field::GaloisField;

mod number_tools;
mod galois_field;

fn main() {
    let a: GaloisField<1234577> = GaloisField::new(110).unwrap();
    let b: GaloisField<1234577> = GaloisField::new(145).unwrap().inverse().unwrap();
    println!("{}", b.characteristic());
    println!("{}", a > b);
    println!("{}", a / b);
}

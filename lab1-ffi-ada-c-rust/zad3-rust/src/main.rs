use utils::common::{DiophantineEq, DiophantineSol};
use lib::rec::rec_dio_solve;

mod utils;
mod lib;

fn main() {
    let eq = DiophantineEq { x_k: 87, y_k: -64, c: 3 };
    let mut sol = DiophantineSol { ..Default::default() };
    rec_dio_solve(&eq, &mut sol);
    println!("{}", sol.x_k);
    println!("{}", sol.x_a);
    println!("{}", sol.y_k);
    println!("{}", sol.y_a);
}

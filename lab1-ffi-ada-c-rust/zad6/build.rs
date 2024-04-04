extern crate cc;

fn main() {
    let lin_path = "src/c/lib/iter.c";
    let rec_path = "src/c/lib/rec.c";
    cc::Build::new()
        .file(lin_path)
        .shared_flag(true)
        .compile("iter.a");
    cc::Build::new()
        .file(rec_path)
        .shared_flag(true)
        .compile("rec.a");
}
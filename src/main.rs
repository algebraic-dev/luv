use std::fs::File;

use firefly::{
    change,
    env::Env,
    parser::parse,
    prettytree::PrettyPrint,
    span::{Edit, Point, Span},
};

fn main() {
    let mut env = Env::new();
    let file_id = env.add_file("".to_owned());

    let edits = [
        Edit::new(
            "(def c a)(def d (+ e a))(def e (+ c d))".to_string(),
            Span::new(Point::new(0, 0), Point::new(0, 0)),
        ),
        Edit::new(
            "(def g (+ e a))(def g (+ e a))".to_string(),
            Span::new(Point::new(0, 9), Point::new(0, 24)),
        ),
        Edit::new(
            "".to_string(),
            Span::new(Point::new(0, 9), Point::new(0, 24)),
        ),
    ];

    for edit in edits {
        let edits = &[edit];

        println!("\n---");
        env.modify_file(file_id, edits);

        let file = env.file_storage.get(&file_id).unwrap().source.clone();
        println!("input: {}", file);

        let res = env.compare_file(file_id).unwrap();


        println!("changes: {:?}", res);
        for change in res {
            if let firefly::id::AnId::Definition(def) = change {
                env.recompute(def);
            }
        }

        let errs = env.errors(file_id).unwrap();

        println!("\nerrors: {}", errs.len());
        for err in errs {
            println!("err: {}", err.message);
        }


    }
}

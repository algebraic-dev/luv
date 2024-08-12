use firefly::{
    change::ChangeMap, parser::parse, span::{Point, Span}, syntax::Change
};

fn main() {
    let (syn1, _errs1) = parse(
        "
        (defn a (x y) (+ x y))
        (defn b (x y) (+ x y))
        (defn c (x y) (+ x y))
    ",
    );

    let (syn2, _errs1) = parse(
        "
        (defn a (x y) (+ z x y))
        (defn d (x y) (+ x y))
        (defn b (x y) (+ x y))
    ",
    );

    let diff = syn1.compare_hashes(&syn2, &[Span::new(Point::new(2, 0), Point::new(2, 20))]);

    let mut change_map = ChangeMap::default();

    for change in diff {
        match change {
            Change::Added(node) => change_map.add(node.toplevel_info(), node),
            Change::Removed(node) =>change_map.remove(node.toplevel_info(), node),
        }
    }

    println!("changed {}", change_map.iter_changed().count());
    println!("added {}", change_map.iter_unchanged_added().count());
    println!("removed {}", change_map.iter_unchanged_removed().count());
}

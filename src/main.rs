use firefly::{r#abstract::TopLevel, analyze, parser::parse, prettytree::PrettyPrint};

fn main() {
    let (syn, _errs) = parse("(defn ata (x y z) (if a (block (let d 3) (let e 3)) c))");

    let nodes = syn.nodes();

    let mut ctx = analyze::Context::default();

    for node in nodes {
        let top_level = TopLevel::from_node(node).unwrap();
        top_level.visit(&mut ctx);
    }

    println!("{}", ctx.scopes.finish().to_tree());
}

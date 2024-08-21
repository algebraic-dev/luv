use std::collections::HashSet;

use crate::{r#abstract::TopLevel, env::Env, id::{self, Id}, ssa::FunctionBuilder};

pub struct Compiler<'a> {
    env: &'a mut Env,
    builder: FunctionBuilder,
}

impl<'a> Compiler<'a> {
    pub fn accumulate_defs(&mut self, queue: &[(Id<id::File>, String)]) -> Vec<(id::Id<id::File>, &'a TopLevel)> {
        let mut visited_functions = HashSet::new();
        let mut functions = Vec::new();
        let mut queue = queue.to_vec();

        while let Some(id) = queue.pop() {
            if visited_functions.contains(&id) {
                continue
            }

            visited_functions.insert(id);

            let file = self.env.files.get(k);
        }

        functions
    }
}
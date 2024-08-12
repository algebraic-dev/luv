//! The manager of graphs and entities during a coding session.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    mem,
};

use crate::{
    analyze,
    change::ChangeMap,
    definition::Definition,
    errors::Error,
    file::File,
    hierarchy::Hierarchy,
    id::{self, AnId, Id},
    parser::parse,
    r#abstract::Name,
    relation::Relations,
    span::Edit,
    storage::Storage,
};

pub type LocalName = (id::Id<id::File>, Name);

/// The environment.
#[derive(Debug)]
pub struct Env {
    pub file_storage: Storage<id::File, File>,
    pub def_storage: Storage<id::Definition, Definition>,

    pub created: Relations<AnId>,
    pub used: Relations<AnId>,
    pub conflicted: HashMap<LocalName, HashSet<Id<id::Definition>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            file_storage: Storage::default(),
            def_storage: Storage::default(),
            created: Relations::default(),
            used: Relations::default(),
            conflicted: Default::default(),
        }
    }

    pub fn add_file(&mut self, source: String) -> Id<id::File> {
        let (new_tree, syntax_errors) = parse(&source);

        let file = File::new(new_tree, source, syntax_errors);
        let id = self.file_storage.add(file);
        self.created.add_node(id::AnId::File(id));

        id
    }

    pub fn modify_file(&mut self, id: Id<id::File>, edits: &[Edit]) -> Option<()> {
        let file = self.file_storage.get_mut(&id)?;

        for edit in edits {
            let start = edit.span.start.to_offset(&file.source);
            let end = edit.span.end.to_offset(&file.source);

            file.source.replace_range(start..end, &edit.data);
        }

        let (new_syntax, new_syntax_errors) = parse(&file.source);

        mem::swap(&mut file.old_tree, &mut file.new_tree);
        file.new_tree = new_syntax;
        file.syntax_errors = new_syntax_errors;

        Some(())
    }

    pub fn errors(&mut self, id: Id<id::File>) -> Option<Vec<Error>> {
        let file = self.file_storage.get_mut(&id)?;
        let mut errs = file.syntax_errors.clone();

        for (id, _) in self.created.get_dependencies(AnId::File(id)) {
            if let AnId::Definition(node) = id {
                let def = self.def_storage.get(&node).unwrap();
                errs.append(&mut def.errors.clone())
            }
        }

        Some(errs)
    }

    pub fn recompute(&mut self, def: Id<id::Definition>) {
        let node = self.def_storage.get_mut(&def).unwrap();
        println!("recomputing {}", node.name);

        let mut analysis = analyze::analyze(&node.syn);

        node.scope = analysis.scopes.finish();
        node.errors = analysis.errors;

        let file = self.file_storage.get_mut(&node.file).unwrap();

        if let Some(conflicts) = self
            .conflicted
            .get_mut(&(node.file, Name::Def(node.name.clone())))
        {
            if conflicts.len() > 1 {
                node.conflicted = true;
                node.errors = vec![Error::new("conflicting definition", node.syn.span.clone())];
                return;
            }
        }

        for unbound in analysis.unbound {
            if let Some(id) = file.names.get(&Name::Def(unbound.data.to_owned())) {
                if id.len() == 1 {
                    self.used.connect(def, *id.iter().next().unwrap());
                    continue;
                }
            }
            node.errors.push(Error::new(
                format!("cannot find `{}`", unbound.data),
                unbound.span,
            ))
        }
    }

    pub fn compare_file(&mut self, file_id: Id<id::File>) -> Option<HashSet<AnId>> {
        let file = self.file_storage.get_mut(&file_id)?;
        let syn_changes = file.old_tree.compare_hashes(&file.new_tree);

        let map_by_name = ChangeMap::of_changes(syn_changes);

        let mut removed = HashSet::new();
        let mut to_check = HashSet::new();

        for (name, _) in map_by_name.iter_unchanged_removed() {
            println!("removing {:?}", name);
            if let Name::Def(_) = &name {
                if let Some(opts) = file.names.get_mut(&name) {
                    if let Some(opt) = opts.iter().next().cloned() {
                        opts.remove(&opt);
                        if let AnId::Definition(def_id) = opt {
                            self.created
                                .remove_edge(AnId::File(file_id), AnId::Definition(def_id));

                            if let Some(conflicts) =
                                self.conflicted.get_mut(&(file_id, name.clone()))
                            {
                                println!("conflicts {}", conflicts.len());
                                conflicts.remove(&def_id);
                                if conflicts.len() == 1 {
                                    let unconflicted = conflicts.drain().next().unwrap();
                                    to_check.insert(AnId::Definition(unconflicted));
                                }
                            }

                            for (used_affected, _) in
                                self.used.get_dependents(AnId::Definition(def_id))
                            {
                                to_check.insert(used_affected);
                            }

                            self.used.remove_node(AnId::Definition(def_id));
                            removed.insert(AnId::Definition(def_id));

                            let set = file.names.get_mut(&name).unwrap();
                            set.remove(&AnId::Definition(def_id));

                            if set.is_empty() {
                                file.names.remove(&name);
                            }
                        }
                    }
                }
            }
        }

        let to_add = map_by_name
            .iter_unchanged_added()
            .filter_map(|x| match x.0 {
                Name::Def(name) => Some(name),
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut counts: HashMap<String, usize> = HashMap::new();

        for name in to_add.clone() {
            *counts.entry(name).or_insert(0) += 1;
        }

        let mut to_connect_new: HashMap<_, Vec<_>> = HashMap::new();
        let mut name_assign = HashMap::new();

        for (name, syn) in map_by_name.iter_unchanged_added() {
            println!("adding {:?}", name);
            if let Name::Def(name_str) = name.clone() {
                let entry = self.conflicted.entry((file_id, name.clone())).or_default();

                if entry.is_empty() && *counts.get(&name_str).unwrap_or(&0) == 1 {
                    let mut analysis = analyze::analyze(syn);

                    let def_id = self.def_storage.add(Definition {
                        name: name_str.to_owned(),
                        syn: syn.clone(),
                        scope: analysis.scopes.finish(),
                        errors: mem::take(&mut analysis.errors),
                        conflicted: false,
                        file: file_id.clone(),
                    });

                    let def_ref = self.def_storage.get_mut(&def_id).unwrap();
                    file.names
                        .entry(name)
                        .or_default()
                        .insert(id::AnId::Definition(def_id));
                    name_assign.insert(name_str.clone(), def_id);

                    for unbound in analysis.unbound {
                        if to_add.contains(&unbound.data) {
                            println!("connect {name_str} with {}", unbound.data);
                            to_connect_new
                                .entry(def_id)
                                .or_default()
                                .push(unbound.data.clone());
                            continue;
                        } else if let Some(id) = file.names.get(&Name::Def(unbound.data.to_owned()))
                        {
                            if id.len() == 1 {
                                self.used.connect(def_id, *id.iter().next().unwrap());
                                continue;
                            } else {
                                def_ref.errors.push(Error::new(
                                    format!("cannot find `{}`", unbound.data),
                                    unbound.span,
                                ))
                            }
                        } else {
                            def_ref.errors.push(Error::new(
                                format!("cannot find `{}`", unbound.data),
                                unbound.span,
                            ))
                        }
                    }

                    self.created.connect(file_id, def_id);
                    entry.insert(def_id);

                    continue;
                } else if entry.len() > 0 {
                    // Can be redundant
                    let entry = *entry.iter().next().unwrap();
                    let data = self.def_storage.get_mut(&entry).unwrap();

                    data.conflicted = true;
                    data.errors = vec![Error::new("conflicting definition", syn.span.clone())];
                }

                let def_id = self.def_storage.add(Definition {
                    name: name_str.to_owned(),
                    syn: syn.clone(),
                    scope: Hierarchy::new(syn.span.clone(), Default::default()),
                    errors: vec![Error::new("conflicting definition", syn.span.clone())],
                    conflicted: true,
                    file: file_id.clone(),
                });

                file.names
                    .entry(name)
                    .or_default()
                    .insert(id::AnId::Definition(def_id));

                self.created.connect(file_id, def_id);
                self.used.add_node(id::AnId::Definition(def_id));
                entry.insert(def_id);
            }
        }

        for (f, t) in to_connect_new {
            for t in t {
                let name = name_assign.get(&t).unwrap();
                self.used.connect(f, *name);
            }
        }

        Some(to_check.difference(&removed).cloned().collect())
    }
}

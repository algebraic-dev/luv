use std::fmt;

/// Represents the type of a value in the simple Lisp.
pub enum Typ {
    Integer,
    Float,
    String,
    Array,
}

impl fmt::Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Typ::Integer => "Integer",
                Typ::Float => "Float",
                Typ::String => "String",
                Typ::Array => "Array",
            }
        )
    }
}

/// Represents a value in the simple Lisp.
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::Float(fl) => write!(f, "Float({})", fl),
            Value::String(s) => write!(f, "String(\"{}\")", s),
            Value::Array(arr) => {
                let elements: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
                write!(f, "Array([{}])", elements.join(", "))
            }
        }
    }
}

/// Represents an instruction in the SSA form.
pub enum Instruction {
    Add { dest: usize, lhs: usize, rhs: usize },
    Subtract { dest: usize, lhs: usize, rhs: usize },
    Multiply { dest: usize, lhs: usize, rhs: usize },
    Divide { dest: usize, lhs: usize, rhs: usize },
    And { dest: usize, lhs: usize, rhs: usize },
    Or { dest: usize, lhs: usize, rhs: usize },
    Equal { dest: usize, lhs: usize, rhs: usize },
    NotEqual { dest: usize, lhs: usize, rhs: usize },
    Not { dest: usize, lhs: usize },
    Incref { dest: usize },
    Decref { dest: usize },
    Check { dest: usize, src: usize, typ: Typ },
    Call { name: String, args: usize },
    Const { dest: usize, value: usize },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Add { dest, lhs, rhs } => {
                write!(f, "Add(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Subtract { dest, lhs, rhs } => {
                write!(f, "Subtract(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Multiply { dest, lhs, rhs } => {
                write!(f, "Multiply(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Divide { dest, lhs, rhs } => {
                write!(f, "Divide(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::And { dest, lhs, rhs } => {
                write!(f, "And(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Or { dest, lhs, rhs } => {
                write!(f, "Or(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Equal { dest, lhs, rhs } => {
                write!(f, "Equal(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::NotEqual { dest, lhs, rhs } => {
                write!(f, "NotEqual(dest: {}, lhs: {}, rhs: {})", dest, lhs, rhs)
            }
            Instruction::Not { dest, lhs } => write!(f, "Not(dest: {}, lhs: {})", dest, lhs),
            Instruction::Incref { dest } => write!(f, "Incref(dest: {})", dest),
            Instruction::Decref { dest } => write!(f, "Decref(dest: {})", dest),
            Instruction::Check { dest, src, typ } => {
                write!(f, "Check(dest: {}, src: {}, typ: {})", dest, src, typ)
            }
            Instruction::Call { name, args } => write!(f, "Call(name: {}, args: {})", name, args),
            Instruction::Const { dest, value } => {
                write!(f, "Const(dest: {}, value: {})", dest, value)
            }
        }
    }
}

/// Represents a terminator that ends a basic block.
pub enum Terminator {
    Return(Option<usize>),
    Jump(usize),
    ConditionalJump {
        condition: usize,
        then_block: usize,
        else_block: usize,
    },
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(None) => write!(f, "Return()"),
            Terminator::Return(Some(value)) => write!(f, "Return(value: {})", value),
            Terminator::Jump(block) => write!(f, "Jump(block: {})", block),
            Terminator::ConditionalJump {
                condition,
                then_block,
                else_block,
            } => write!(
                f,
                "ConditionalJump(condition: {}, then_block: {}, else_block: {})",
                condition, then_block, else_block
            ),
        }
    }
}

/// Represents a basic block containing instructions and a terminator.
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instructions: Vec<String> = self.instructions.iter().map(|i| i.to_string()).collect();
        let terminator = match &self.terminator {
            Some(t) => t.to_string(),
            None => "None".to_string(),
        };
        write!(
            f,
            "BasicBlock(instructions: [{}], terminator: {})",
            instructions.join(", "),
            terminator
        )
    }
}

/// Represents a function containing arguments, blocks, and constants.
pub struct Function {
    pub arguments: usize,
    pub blocks: Vec<BasicBlock>,
    pub constants: Vec<Value>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let blocks: Vec<String> = self.blocks.iter().map(|b| b.to_string()).collect();
        let constants: Vec<String> = self.constants.iter().map(|c| c.to_string()).collect();
        write!(
            f,
            "Function(arguments: {}, blocks: [{}], constants: [{}])",
            self.arguments,
            blocks.join(", "),
            constants.join(", ")
        )
    }
}

/// Builder for creating functions, blocks, and instructions.
pub struct FunctionBuilder {
    function: Function,
    current_block: usize,
    var_num: usize,
}

impl FunctionBuilder {
    /// Creates a new `FunctionBuilder` with the specified number of arguments.
    pub fn new(arguments: usize) -> Self {
        let mut builder = Self {
            function: Function {
                arguments,
                blocks: Vec::new(),
                constants: Vec::new(),
            },
            current_block: 0,
            var_num: 0,
        };
        builder.current_block = builder.create_block(); // Create the initial block
        builder
    }

    /// Adds a constant to the function and returns its index.
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.function.constants.push(value);
        self.function.constants.len() - 1
    }

    /// Creates a new basic block and sets it as the current block.
    pub fn create_block(&mut self) -> usize {
        let block = BasicBlock {
            instructions: Vec::new(),
            terminator: None,
        };
        self.function.blocks.push(block);
        self.function.blocks.len() - 1
    }

    /// Switches to an existing block.
    pub fn switch_block(&mut self, block: usize) {
        self.current_block = block;
    }

    /// Adds an instruction to the current block.
    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.function.blocks[self.current_block]
            .instructions
            .push(instruction);
    }

    /// Sets the terminator for the current block.
    pub fn set_terminator(&mut self, terminator: Terminator) {
        self.function.blocks[self.current_block].terminator = Some(terminator);
    }

    /// Builds the final function.
    pub fn build(self) -> Function {
        self.function
    }

    pub fn new_var(&mut self) -> usize {
        let n = self.var_num;
        self.var_num += 1;
        n
    }

    // Functions for creating instructions

    pub fn add(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Add { dest, lhs, rhs });
        dest
    }

    pub fn subtract(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Subtract { dest, lhs, rhs });
        dest
    }

    pub fn multiply(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Multiply { dest, lhs, rhs });
        dest
    }

    pub fn divide(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Divide { dest, lhs, rhs });
        dest
    }

    pub fn and(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::And { dest, lhs, rhs });
        dest
    }

    pub fn or(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Or { dest, lhs, rhs });
        dest
    }

    pub fn equal(&mut self, lhs: usize, rhs: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Equal { dest, lhs, rhs });
        dest
    }

    pub fn incref(&mut self) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Incref { dest });
        dest
    }

    pub fn decref(&mut self) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Decref { dest });
        dest
    }

    pub fn check(&mut self, src: usize, typ: Typ) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Check { dest, src, typ });
        dest
    }

    pub fn push(&mut self, value: usize) -> usize {
        let dest = self.new_var();
        self.add_instruction(Instruction::Const { dest, value });
        dest
    }
}

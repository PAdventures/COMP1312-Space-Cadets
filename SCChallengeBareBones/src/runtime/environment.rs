// Import necessary modules
use super::values;
use std::collections::HashMap;

// Define the Environment struct which holds all the variables at runtime
#[derive(Clone)]
pub struct Environment {
    // Internally, any RuntimeValue can be stored, but only the Number value can be used in the program
    pub variables: HashMap<String, values::RuntimeValue>,
}

impl Environment {
    pub fn create() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    // Function that creates or updates a variable in the environment
    pub fn assign_variable(&mut self, name: String, value: values::RuntimeValue) -> () {
        self.variables.insert(name, value);
    }

    // Function that finds the contents of a variable in the environment
    pub fn lookup_variable(&mut self, name: String) -> Option<values::RuntimeValue> {
        match self.variables.get(&name) {
            Some(v) => Some(v.to_owned()),
            None => None,
        }
    }
}

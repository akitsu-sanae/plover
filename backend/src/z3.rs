use std::collections::HashMap;

#[derive(Deserialize, Clone)]
enum Format {
    #[serde(rename = "smtlib2")]
    Smtlib2,
    #[serde(rename = "datalog")]
    Datalog,
    #[serde(rename = "DIMACS")]
    Dimacs,
    #[serde(rename = "Weighted CNF DIMACS")]
    WeightedCnfDimacs,
    #[serde(rename = "PB optimization")]
    PbOptimization,
    #[serde(rename = "CPLEX LP")]
    CplexLp,
    #[serde(rename = "Z3 log")]
    Z3Log,
}

#[derive(Deserialize, Clone)]
struct Display {
    #[serde(rename = "global-parameters")]
    global_parameters: bool,
    #[serde(rename = "global-parameter-descriptions")]
    global_parameter_descriptions: bool,
    #[serde(rename = "statistics")]
    statistics: bool,
    #[serde(rename = "warnings")]
    warnings: bool,
}

#[derive(Deserialize, Clone)]
struct Limit {
    #[serde(rename = "timeout")]
    timeout: Option<i32>,
    #[serde(rename = "soft-timeout")]
    soft_timeout: Option<i32>,
    #[serde(rename = "memory-limit")]
    memory: Option<i32>,
}

#[derive(Deserialize, Clone)]
pub struct Argments {
    #[serde(rename = "format")]
    format: Format,

    #[serde(rename = "display")]
    display: Display,

    #[serde(rename = "limit")]
    limit: Limit,

    #[serde(rename = "global-parameters")]
    global_parameters: HashMap<String, String>,
    #[serde(rename = "module-parameters")]
    module_parameters: HashMap<(String, String), String>,

    others: Vec<String>,
}

impl Argments {
    pub fn to_commandline(mut self) -> Vec<String> {
        let mut result = vec![];
        match self.format {
            Format::Smtlib2 => result.push("-smt2".to_string()),
            Format::Datalog => result.push("-dl".to_string()),
            Format::Dimacs => result.push("-dimacs".to_string()),
            Format::WeightedCnfDimacs => result.push("-wcnf".to_string()),
            Format::PbOptimization => result.push("-opb".to_string()),
            Format::CplexLp => result.push("-lp".to_string()),
            Format::Z3Log => result.push("-log".to_string()),
        }

        if self.display.global_parameters {
            result.push("-p".to_string());
        }
        if self.display.global_parameter_descriptions {
            result.push("-pd".to_string());
        }
        if self.display.statistics {
            result.push("-st".to_string());
        }
        if self.display.warnings {
            result.push("-nw".to_string());
        }
        if let Some(n) = self.limit.timeout {
            result.push("-T".to_string());
            result.push(n.to_string());
        }
        if let Some(n) = self.limit.soft_timeout {
            result.push("-t".to_string());
            result.push(n.to_string());
        }
        if let Some(n) = self.limit.memory {
            result.push("-memory".to_string());
            result.push(n.to_string());
        }

        for (ref param_name, ref value) in self.global_parameters.iter() {
            result.push(format!("{}={}", param_name, value));
        }
        for (&(ref module_name, ref param_name), ref value) in self.module_parameters.iter() {
            result.push(format!("{}.{}={}", module_name, param_name, value));
        }

        result.append(&mut self.others);

        result
    }
}

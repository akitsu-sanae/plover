#[derive(Deserialize, Clone)]
pub enum Lang {
    #[serde(rename = "auto")]
    Auto,

    #[serde(rename = "cvc4")]
    Cvc4,

    #[serde(rename = "smtlib1")]
    Smtlib1,

    #[serde(rename = "smtlib2.0")]
    Smtlib2,

    #[serde(rename = "smtlib2.5")]
    Smtlib25,

    #[serde(rename = "smtlib2.6")]
    Smtlib26,

    #[serde(rename = "smtlib2.6.1")]
    Smtlib261,

    #[serde(rename = "tptp")]
    Tptp,

    #[serde(rename = "sygus")]
    Sygus,
}

#[derive(Deserialize, Clone)]
pub enum OutputLang {
    #[serde(rename = "auto")]
    Auto,

    #[serde(rename = "cvc4")]
    Cvc4,

    #[serde(rename = "cvc3")]
    Cvc3,

    #[serde(rename = "smtlib2")]
    Smtlib2,

    #[serde(rename = "smtlib2.5")]
    Smtlib25,

    #[serde(rename = "smtlib2.6")]
    Smtlib26,

    #[serde(rename = "smtlib2.6.1")]
    Smtlib261,

    #[serde(rename = "tptp")]
    Tptp,

    #[serde(rename = "z3str")]
    Z3str,

    #[serde(rename = "ast")]
    Ast,
}

#[derive(Deserialize, Clone)]
pub struct Argments {
    #[serde(rename = "lang")]
    lang: Lang,

    #[serde(rename = "output-lang")]
    output_lang: OutputLang,

    #[serde(rename = "verbosity")]
    verbosity: i32,

    #[serde(rename = "seed")]
    seed: Option<i32>,

    #[serde(rename = "cpu-time")]
    cpu_time: bool,

    #[serde(rename = "incremental")]
    incremental: bool,

    #[serde(rename = "resource-limit-per")]
    resource_limit_per: Option<i32>,
    #[serde(rename = "resource-limit")]
    resource_limit: Option<i32>,

    #[serde(rename = "time-limit-per")]
    time_limit_per: Option<i32>,
    #[serde(rename = "time-limit")]
    time_limit: Option<i32>,

    #[serde(rename = "approx-branch-depth")]
    approx_branch_depth: Option<i32>,
    #[serde(rename = "arith-no-partial-fun")]
    arith_no_partial_fun: bool,
}

impl Argments {
    pub fn to_commandline(&self) -> Vec<String> {
        let mut result = vec![];
        result.push(format!(
            "--lang={}",
            match self.lang {
                Lang::Auto => "auto",
                Lang::Cvc4 => "cvc4",
                Lang::Smtlib1 => "smtlib1",
                Lang::Smtlib2 => "smtlib2.0",
                Lang::Smtlib25 => "smtlib2.5",
                Lang::Smtlib26 => "smtlib2.6",
                Lang::Smtlib261 => "smtlib2.6.1",
                Lang::Tptp => "tptp",
                Lang::Sygus => "sygus",
            }
        ));
        result.push(format!(
            "--output-lang={}",
            match self.output_lang {
                OutputLang::Auto => "auto",
                OutputLang::Cvc4 => "cvc4",
                OutputLang::Cvc3 => "cvc3",
                OutputLang::Smtlib2 => "smtlib2.0",
                OutputLang::Smtlib25 => "smtlib2.5",
                OutputLang::Smtlib26 => "smtlib2.6",
                OutputLang::Smtlib261 => "smtlib2.6.1",
                OutputLang::Tptp => "tptp",
                OutputLang::Z3str => "z3str",
                OutputLang::Ast => "ast",
            }
        ));

        for _ in 0..self.verbosity.abs() {
            result.push(if self.verbosity >= 0 {
                "--verbose".to_string()
            } else {
                "--quiet".to_string()
            });
        }

        if let Some(n) = self.seed {
            result.push(format!("--seed={}", n));
        }

        if self.cpu_time {
            result.push("--cpu-time".to_string());
        }
        if self.incremental {
            result.push("--incremental".to_string());
        }

        if let Some(n) = self.resource_limit_per {
            result.push(format!("--rlimit-per={}", n));
        }
        if let Some(n) = self.resource_limit {
            result.push(format!("--rlimit={}", n));
        }

        if let Some(n) = self.time_limit_per {
            result.push(format!("--tlimit-per={}", n));
        }
        if let Some(n) = self.time_limit {
            result.push(format!("--tlimit={}", n));
        }

        if let Some(n) = self.approx_branch_depth {
            result.push(format!("--approx-branch-depth={}", n));
        }
        if self.arith_no_partial_fun {
            result.push("--arith-no-partial-fun".to_string());
        }
        result
    }
}

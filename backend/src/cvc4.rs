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
    lang: Lang,
    #[serde(rename = "output-lang")]
    output_lang: OutputLang,

    verbosity: i32, // --quiet/--verbose
    stats: bool,
    seed: Option<i32>,
    #[serde(rename = "strict-parsing")]
    strict_parsing: bool,
    #[serde(rename = "cpu-time")]
    cpu_time: bool,
    #[serde(rename = "hard-limit")]
    hard_limit: bool,
    incremental: bool,
    #[serde(rename = "produce-assertions")]
    produce_assertions: bool,
    #[serde(rename = "produce-models")]
    produce_models: bool,
    #[serde(rename = "rlimit-per")]
    rlimit_per: Option<i32>,
    rlimit: Option<i32>,
    #[serde(rename = "tlimit-per")]
    tlimit_per: Option<i32>,
    tlimit: Option<i32>,

    others: Vec<String>,
}

impl Argments {
    pub fn to_commandline(self) -> Vec<String> {
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

        result.push(if self.stats {
            "--stats".to_string()
        } else {
            "--no-stats".to_string()
        });

        if let Some(n) = self.seed {
            result.push(format!("--seed={}", n));
        }

        result.push(if self.strict_parsing {
            "--strict-parsing".to_string()
        } else {
            "--no-strict-parsing".to_string()
        });

        result.push(if self.cpu_time {
            "--cpu-time".to_string()
        } else {
            "--no-cpu-time".to_string()
        });

        result.push(if self.hard_limit {
            "--hard-limit".to_string()
        } else {
            "--no-hard-limit".to_string()
        });

        result.push(if self.incremental {
            "--incremental".to_string()
        } else {
            "--no-incremental".to_string()
        });

        result.push(if self.produce_assertions {
            "--produce-assertions".to_string()
        } else {
            "--no-produce-assertions".to_string()
        });

        result.push(if self.produce_models {
            "--produce-models".to_string()
        } else {
            "--no-produce-models".to_string()
        });

        if let Some(n) = self.rlimit_per {
            result.push(format!("--rlimit-per={}", n));
        }
        if let Some(n) = self.rlimit {
            result.push(format!("--rlimit={}", n));
        }

        if let Some(n) = self.tlimit_per {
            result.push(format!("--tlimit-per={}", n));
        }
        if let Some(n) = self.tlimit {
            result.push(format!("--tlimit={}", n));
        }

        let mut others: Vec<String> = self
            .others
            .into_iter()
            .map(|opt| {
                format!("\"{}\"", opt.replace("\"", "\\\"")) // escape double-quote
            })
            .collect();
        result.append(&mut others);

        result
    }
}

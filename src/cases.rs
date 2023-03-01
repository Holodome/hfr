use clap::ValueEnum;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Default)]
pub enum Case {
    /// snake_case
    #[default]
    Snake,
    /// kebab-case
    Kebab,
    /// PascalCase
    Pascal,
    /// SCREAMING_SNAKE_CASE
    ScreamingSnake,
    /// camelCase
    Camel,
    /// SCREAMING-KEBAB-CASE
    ScreamingKebab,
    /// singleword
    SingleWord,
    /// SCREAMINGSINGLEWORD
    ScreamingSingleWord,
}

pub fn split(case: Case, name: &str) -> Vec<String> {
    let functor = match case {
        Case::Snake => SnakeCase::split_name,
        Case::Kebab => KebabCase::split_name,
        Case::Pascal => PascalCase::split_name,
        Case::ScreamingSnake => ScreamingSnakeCase::split_name,
        Case::Camel => CamelCase::split_name,
        Case::ScreamingKebab => ScreamingKebabCase::split_name,
        Case::SingleWord => SingleWord::split_name,
        Case::ScreamingSingleWord => ScreamingSingleWord::split_name,
    };
    functor(name)
}

/// Detect case assuming priority
/// 1. If contains '-' - kebab case
/// 2. If contains '_' - snake case
/// 3. If contains uppercase letters - PascalCase or camelCase or SCREAMINGSINGLEWORD
/// 4. Single word without delimiters otherwise
pub fn detect_case(name: &str) -> Case {
    if name.contains('-') {
        return if name.chars().all(|it| it == '-' || it.is_uppercase()) {
            Case::ScreamingKebab
        } else {
            Case::Kebab
        };
    }

    if name.contains('_') {
        return if name.chars().all(|it| it == '_' || it.is_uppercase()) {
            Case::ScreamingSnake
        } else {
            Case::Snake
        };
    }

    if name.chars().any(|it| it.is_uppercase()) {
        if name.chars().all(|it| it.is_uppercase()) {
            return Case::ScreamingSingleWord;
        }
        if let Some(first) = name.chars().next() {
            return if first.is_uppercase() {
                Case::Pascal
            } else {
                Case::Camel
            };
        }
    }

    Case::SingleWord
}

trait CaseOps {
    fn split_name(name: &str) -> Vec<String>;
    fn construct(parts: &[String]) -> String;
}

struct KebabCase;
impl CaseOps for KebabCase {
    fn split_name(name: &str) -> Vec<String> {
        name.split('-').map(|str| str.to_owned()).collect()
    }

    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .map(|it| it.to_lowercase())
            .collect::<Vec<String>>()
            .join("-")
    }
}

struct SnakeCase;
impl CaseOps for SnakeCase {
    fn split_name(name: &str) -> Vec<String> {
        name.split('_').map(str::to_owned).collect()
    }

    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .map(|it| it.to_lowercase())
            .collect::<Vec<String>>()
            .join("-")
    }
}

struct PascalCase;
impl CaseOps for PascalCase {
    fn split_name(name: &str) -> Vec<String> {
        let upper_case_idxs = name
            .chars()
            .enumerate()
            .filter(|(_, it)| it.is_uppercase())
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        upper_case_idxs[..upper_case_idxs.len() - 1]
            .iter()
            .copied()
            .zip(upper_case_idxs[1..].iter().copied())
            .map(|(a, b)| name[a..b].to_owned().to_lowercase())
            .collect()
    }

    fn construct(parts: &[String]) -> String {
        parts.iter().map(|it| capitalize(it)).collect()
    }
}

struct ScreamingSnakeCase;
impl CaseOps for ScreamingSnakeCase {
    fn split_name(name: &str) -> Vec<String> {
        name.split('_')
            .map(|str| str.to_owned().to_lowercase())
            .collect()
    }

    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .map(|it| it.to_uppercase())
            .collect::<Vec<String>>()
            .join("_")
    }
}

struct ScreamingKebabCase;
impl CaseOps for ScreamingKebabCase {
    fn split_name(name: &str) -> Vec<String> {
        name.split('-')
            .map(|str| str.to_owned().to_lowercase())
            .collect()
    }

    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .map(|it| it.to_uppercase())
            .collect::<Vec<String>>()
            .join("-")
    }
}

struct CamelCase;
impl CaseOps for CamelCase {
    fn split_name(name: &str) -> Vec<String> {
        let upper_case_idxs = name
            .chars()
            .enumerate()
            .filter(|(i, it)| it.is_uppercase() || *i == 0)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        upper_case_idxs[..upper_case_idxs.len() - 1]
            .iter()
            .copied()
            .zip(upper_case_idxs[1..].iter().copied())
            .map(|(a, b)| name[a..b].to_owned().to_lowercase())
            .collect()
    }

    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .enumerate()
            .map(|(i, it)| if i == 0 { it.clone() } else { capitalize(it) })
            .collect()
    }
}

struct SingleWord;
impl CaseOps for SingleWord {
    fn split_name(name: &str) -> Vec<String> {
        vec![name.to_owned()]
    }
    fn construct(parts: &[String]) -> String {
        parts.join("")
    }
}

struct ScreamingSingleWord;
impl CaseOps for ScreamingSingleWord {
    fn split_name(name: &str) -> Vec<String> {
        vec![name.to_owned()]
    }
    fn construct(parts: &[String]) -> String {
        parts
            .iter()
            .map(|it| it.to_uppercase())
            .collect::<Vec<_>>()
            .join("")
    }
}

pub fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

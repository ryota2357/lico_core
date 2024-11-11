use crate::{
    codegen::{ensure_file_contents, rustfmt},
    project_root,
};
use anyhow::Result;
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Punct, Spacing, TokenStream};
use quote::{format_ident, quote};
use std::{collections::HashMap, fs};
use ungrammar::{Grammar, Node, NodeData, Rule, Token, TokenData};
use xshell::Shell;

pub(crate) fn generate(sh: &Shell) -> Result<()> {
    let source = fs::read_to_string(project_root().join("crates/syntax/lico.ungram"))?;
    let grammar = source.parse::<Grammar>()?;

    let ast_nodes = grammar.iter().map(|node| convert_node(&node, &grammar)).collect::<Box<_>>();
    let syntax_kinds = {
        let defaults =
            DEFAULT_SYNTAX_KIND.iter().map(|name| format_ident!("{}", name)).collect::<Box<_>>();
        let literals =
            LITERAL_SYNTAX_KIND.iter().map(|name| format_ident!("{}", name)).collect::<Box<_>>();
        let mut puncts = grammar
            .tokens()
            .filter_map(|token| convert_token(&token, &grammar))
            .filter(|it| !it.to_string().ends_with("_KW"))
            .collect::<Box<_>>();
        puncts.sort_unstable();
        let mut keywords = grammar
            .tokens()
            .filter_map(|token| convert_token(&token, &grammar))
            .filter(|it| it.to_string().ends_with("_KW"))
            .collect::<Box<_>>();
        keywords.sort_unstable();
        let mut nodes = ast_nodes
            .iter()
            .filter_map(|(_, kind)| kind.clone())
            .map(|kind| format_ident!("{}", kind))
            .collect::<Box<_>>();
        nodes.sort_unstable();
        let size = defaults.len() + literals.len() + keywords.len() + puncts.len() + nodes.len();
        if size > u8::MAX.into() {
            panic!("too many syntax kinds");
        }
        let t_macro_variants = {
            let mut symbol_map = grammar
                .tokens()
                .map(|token| {
                    let token = &grammar[token];
                    let syntax_kind = format_ident!("{}", get_token_syntax_kind_name(token));
                    match token.name.as_str() {
                        punct @ ("(" | ")" | "{" | "}" | "[" | "]") => {
                            let c = punct.chars().next().unwrap();
                            (quote! { #c }, syntax_kind)
                        }
                        "_" => (quote! { _ }, syntax_kind),
                        punct if punct.chars().all(|c| c.is_ascii_punctuation()) => {
                            let cs = punct.chars().map(|c| Punct::new(c, Spacing::Joint));
                            (quote! { #(#cs)* }, syntax_kind)
                        }
                        name => {
                            let name = name.strip_prefix('$').unwrap_or(name);
                            let ident = format_ident!("{}", name);
                            (quote! { #ident }, syntax_kind)
                        }
                    }
                })
                .collect::<Vec<_>>();
            symbol_map.sort_unstable_by_key(|(symbol, _)| symbol.to_string());
            let symbols = symbol_map.iter().map(|(p, _)| p).collect::<Box<_>>();
            let names = symbol_map.iter().map(|(_, n)| n).collect::<Box<_>>();
            quote! {
                #[macro_export]
                #[doc(hidden)]
                macro_rules! __token_kind_fast_accsess {
                    #((#symbols) => { $crate::SyntaxKind::#names });*
                }
                pub use __token_kind_fast_accsess as T;
            }
        };
        quote! {
            #[allow(bad_style)]
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            #[repr(u8)]
            pub enum SyntaxKind {
                #( #defaults ),*,
                #( #literals ),*,
                #( #keywords ),*,
                #( #puncts ),*,
                #( #nodes ),*,
            }
            const fn _static_assert_size() { const { assert!(size_of::<SyntaxKind>() == 1) } }
            impl From<SyntaxKind> for rowan::SyntaxKind {
                fn from(kind: SyntaxKind) -> Self { Self(kind as u16) }
            }
            impl From<rowan::SyntaxKind> for SyntaxKind {
                fn from(kind: rowan::SyntaxKind) -> Self {
                    assert!(kind.0 <= (#size as u16), "bad SyntaxKind: {:?}", kind);
                    // SAFETY: Ensured by the assert above.
                    unsafe { ::core::mem::transmute(kind.0 as u8) }
                }
            }
            impl SyntaxKind {
                pub fn is_literal(self) -> bool { matches!(self, #( SyntaxKind::#literals )|*) }
                pub fn is_keyword(self) -> bool { matches!(self, #( SyntaxKind::#keywords )|*) }
                pub fn is_punctuation(self) -> bool { matches!(self, #( SyntaxKind::#puncts )|*) }
            }
            #t_macro_variants
        }
    };

    let ast_code = rustfmt(sh, {
        std::iter::once(
            quote! {
                use super::{LicoLanguage, SyntaxKind, SyntaxNode, SyntaxToken};
                pub use rowan::ast::{AstNode, AstChildren};
                use rowan::ast::support;
            }
            .to_string(),
        )
        .chain(ast_nodes.iter().map(|(node, _)| node.to_string()))
        .collect::<Vec<_>>()
        .join("\n\n")
    })?;
    let syntax_kind_code = rustfmt(sh, syntax_kinds.to_string())?;

    fn add_header(source: &str) -> String {
        [
            "//! This file is generated by `cargo codegen syntax`, do not edit by hand.",
            "#![cfg_attr(rustfmt, rustfmt::skip)]",
            "",
            source,
        ]
        .join("\n")
    }
    ensure_file_contents(
        &project_root().join("crates/syntax/src/syntax_kind.rs"),
        &add_header(&syntax_kind_code),
    )?;
    ensure_file_contents(&project_root().join("crates/syntax/src/ast.rs"), &add_header(&ast_code))?;

    Ok(())
}

const DEFAULT_SYNTAX_KIND: &[&str] = &["ERROR", "COMMENT", "WHITESPACE", "IDENT", "SHEBANG"];

const LITERAL_SYNTAX_KIND: &[&str] = &["INT", "FLOAT", "STRING", "TRUE", "FALSE", "NIL"];

const PUNCT_MAP: &[(&str, &str)] = &[
    ("+", "plus"),
    ("-", "minus"),
    ("*", "star"),
    ("/", "slash"),
    ("%", "percent"),
    ("^", "caret"),
    ("&", "amp"),
    ("|", "pipe"),
    ("~", "tilde"),
    ("!", "bang"),
    ("=", "eq"),
    ("<", "lt"),
    (">", "gt"),
    (".", "dot"),
    ("@", "at"),
    (",", "comma"),
    (":", "colon"),
    ("_", "underscore"),
    ("(", "open_paren"),
    (")", "close_paren"),
    ("{", "open_brace"),
    ("}", "close_brace"),
    ("[", "open_bracket"),
    ("]", "close_bracket"),
    ("->", "arrow"),
    ("!=", "bang_eq"),
    ("..", "dot2"),
    ("==", "eq2"),
    ("<<", "lt2"),
    ("<=", "le"),
    (">>", "gt2"),
    (">=", "ge"),
];

/// return (AstNode, SyntaxKind)
fn convert_node(node: &Node, grammar: &Grammar) -> (TokenStream, Option<String>) {
    let node = &grammar[*node];
    if node.name.as_str() == "Literal" {
        return (convert_literal_node_to_struct(node, grammar), Some("LITERAL".to_string()));
    }
    match &node.rule {
        Rule::Labeled { .. } => todo!(),
        Rule::Node(_) | Rule::Token(_) | Rule::Seq(_) => {
            (convert_node_to_struct(node, grammar), Some(node.name.to_case(Case::UpperSnake)))
        }
        Rule::Alt(_) => (convert_node_to_enum(node, grammar), None),
        Rule::Opt(_) => todo!(),
        Rule::Rep(_) => todo!(),
    }
}

/// return SyntaxKind
fn convert_token(token: &Token, grammar: &Grammar) -> Option<Ident> {
    let token = &grammar[*token];
    match token.name.as_str() {
        name if name.starts_with('$') => {
            let kind: &str = &name.strip_prefix('$').unwrap().to_case(Case::UpperSnake);
            if DEFAULT_SYNTAX_KIND.contains(&kind) || LITERAL_SYNTAX_KIND.contains(&kind) {
                None
            } else {
                panic!("unknown token special token: {}", kind);
            }
        }
        _ => {
            let syntax_kind = get_token_syntax_kind_name(token);
            Some(format_ident!("{}", syntax_kind))
        }
    }
}

fn convert_node_to_enum(node: &NodeData, grammar: &Grammar) -> TokenStream {
    assert!(matches!(node.rule, Rule::Alt(_)));

    let name = node.name.to_case(Case::Pascal);
    let mut variant_names = Vec::new();
    let mut variant_ty_names = Vec::new();
    let Rule::Alt(variants) = &node.rule else { unreachable!() };
    for variant in variants {
        match variant {
            Rule::Labeled { label, rule } => match rule as &Rule {
                Rule::Node(node) => {
                    let name = label.to_case(Case::UpperCamel);
                    let ty_name = grammar[*node].name.to_case(Case::UpperCamel);
                    variant_names.push(format_ident!("{}", name));
                    variant_ty_names.push(format_ident!("{}", ty_name));
                }
                _ => panic!("complex rule {:?} in {}", rule, name),
            },
            Rule::Node(node) => {
                let node_name = &grammar[*node].name;
                let name = match node_name.strip_suffix(&name) {
                    Some(name) => name.to_case(Case::UpperCamel),
                    None => node_name.to_case(Case::UpperCamel),
                };
                let ty_name = node_name.to_case(Case::UpperCamel);
                variant_names.push(format_ident!("{}", name));
                variant_ty_names.push(format_ident!("{}", ty_name));
            }
            _ => panic!("complex rule {:?} in {}", variant, name),
        }
    }

    let name = format_ident!("{}", name);
    quote! {
        pub enum #name {
            #(#variant_names(#variant_ty_names)),*
        }
        impl AstNode for #name {
            type Language = LicoLanguage;
            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool {
                #(<#variant_ty_names as AstNode>::can_cast(kind))||*
            }
            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
                let kind = node.kind();
                #(
                    if <#variant_ty_names as AstNode>::can_cast(kind) {
                        let casted = <#variant_ty_names as AstNode>::cast(node).expect("Invalid `can_cast` implementation");
                        return Some(Self::#variant_names(casted));
                    }
                )*
                None
            }
            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                match self {
                    #(Self::#variant_names(x) => x.syntax()),*
                }
            }
        }
    }
}

fn convert_node_to_struct(node: &NodeData, grammar: &Grammar) -> TokenStream {
    assert!(matches!(node.rule, Rule::Seq(_) | Rule::Node(_) | Rule::Token(_)));

    let name = node.name.to_case(Case::Pascal);
    let fields = match &node.rule {
        node @ Rule::Node(_) => vec![node],
        tok @ Rule::Token(_) => vec![tok],
        Rule::Seq(vec) => vec.iter().collect(),
        _ => unreachable!(),
    };
    struct FieldTokenStreams(Vec<TokenStream>, HashMap<String, usize>);
    impl FieldTokenStreams {
        fn push_child(&mut self, field_name: &str, field_ty: &str) {
            let field_name = format_ident!("{}", field_name);
            let field_ty = format_ident!("{}", field_ty);
            self.1.entry(field_ty.to_string()).and_modify(|count| *count += 1).or_insert(0);
            let count = self.1[&field_ty.to_string()];
            if count == 0 {
                self.0.push(quote! {
                    pub fn #field_name(&self) -> Option<#field_ty> {
                        support::child(AstNode::syntax(self))
                    }
                });
            } else {
                self.0.push(quote! {
                    pub fn #field_name(&self) -> Option<#field_ty> {
                        support::children(AstNode::syntax(self)).nth(#count)
                    }
                });
            }
        }
        fn push_token(&mut self, field_name: &str, syntax_kind: &str) {
            let field_name = format_ident!("{}", field_name);
            let syntax_kind = format_ident!("{}", syntax_kind);
            self.0.push(quote! {
                pub fn #field_name(&self) -> Option<SyntaxToken> {
                    support::token(AstNode::syntax(self), SyntaxKind::#syntax_kind)
                }
            });
        }
        fn push_children(&mut self, field_name: &str, field_ty: &str) {
            let field_name = format_ident!("{}", field_name);
            let field_ty = format_ident!("{}", field_ty);
            self.0.push(quote! {
                pub fn #field_name(&self) -> AstChildren<#field_ty> {
                    support::children(AstNode::syntax(self))
                }
            });
        }
    }
    let mut field_token_streams = FieldTokenStreams(Vec::new(), HashMap::new());
    let mut extra_token_streams = Vec::new();
    for mut field in fields {
        while let Rule::Opt(next) = field {
            field = next;
        }
        match field {
            Rule::Labeled { label, rule } => {
                let mut rule = rule as &Rule;
                while let Rule::Opt(next) = rule {
                    rule = next;
                }
                let field_name = label.to_case(Case::Snake);
                match rule {
                    Rule::Node(node) => {
                        let field_ty = grammar[*node].name.to_case(Case::UpperCamel);
                        field_token_streams.push_child(&field_name, &field_ty);
                    }
                    Rule::Token(token) => {
                        let syntax_kind = get_token_syntax_kind_name(&grammar[*token]);
                        field_token_streams.push_token(&field_name, &syntax_kind);
                    }
                    Rule::Rep(rule) => {
                        let Rule::Node(node) = rule as &Rule else {
                            panic!("complex rule {:?} in {}", rule, name)
                        };
                        let field_ty = grammar[*node].name.to_case(Case::UpperCamel);
                        field_token_streams.push_children(&field_name, &field_ty);
                    }
                    Rule::Seq(seq) => match try_simp_seq_to_node_name(seq, grammar) {
                        Some(node_name) => {
                            let field_ty = node_name.to_case(Case::UpperCamel);
                            field_token_streams.push_child(&field_name, &field_ty);
                        }
                        None => panic!("complex rule {:?} in {}", field, name),
                    },
                    Rule::Alt(rules) => {
                        let tokens = rules
                            .iter()
                            .map(|rule| match rule {
                                Rule::Token(token) => &grammar[*token],
                                _ => panic!("complex rule {:?} in {}", rules, name),
                            })
                            .collect::<Box<_>>();
                        let (fields, op_enum) = match (name.as_str(), field_name.as_str()) {
                            ("BinaryExpr", "op") => binary_expr_op_field_and_op_enum(&tokens),
                            ("PrefixExpr", "op") => prefix_expr_op_field_and_op_enum(&tokens),
                            _ => panic!("unknown"),
                        };
                        field_token_streams.0.push(fields);
                        extra_token_streams.push(op_enum);
                    }
                    Rule::Labeled { .. } => panic!("complex rule {:?} in {}", rule, name),
                    Rule::Opt(_) => unreachable!(),
                }
            }
            Rule::Node(node) => {
                let field_name = grammar[*node].name.to_case(Case::Snake);
                let field_ty = grammar[*node].name.to_case(Case::UpperCamel);
                field_token_streams.push_child(&field_name, &field_ty);
            }
            Rule::Token(token) => {
                let field_name = {
                    let name = &grammar[*token].name;
                    if name.starts_with('$') {
                        name.strip_prefix('$').unwrap().to_case(Case::Snake)
                    } else {
                        let name = match PUNCT_MAP.iter().find(|(p, _)| p == name) {
                            Some((_, pname)) => pname,
                            None => name.as_str(),
                        };
                        format!("{}_token", name.to_case(Case::Snake))
                    }
                };
                let syntax_kind = get_token_syntax_kind_name(&grammar[*token]);
                field_token_streams.push_token(&field_name, &syntax_kind);
            }
            Rule::Rep(rule) => {
                let Rule::Node(node) = rule as &Rule else {
                    panic!("complex rule {:?} in {}", rule, name)
                };
                let field_name = grammar[*node].name.to_case(Case::Snake);
                let field_ty = grammar[*node].name.to_case(Case::UpperCamel);
                field_token_streams.push_children(&field_name, &field_ty);
            }
            Rule::Seq(seq) => match try_simp_seq_to_node_name(seq, grammar) {
                Some(node_name) => {
                    let field_name = node_name.to_case(Case::Snake);
                    let field_ty = node_name.to_case(Case::UpperCamel);
                    field_token_streams.push_child(&field_name, &field_ty);
                }
                None => panic!("complex rule {:?} in {}", field, name),
            },
            Rule::Alt(_) => panic!("complex rule {:?} in {}", field, name),
            Rule::Opt(_) => unreachable!(),
        }
    }

    let fields = field_token_streams.0;
    let syntax_kind = format_ident!("{}", name.to_case(Case::UpperSnake));
    let name = format_ident!("{}", name);
    quote! {
        pub struct #name(SyntaxNode);
        impl AstNode for #name {
            type Language = LicoLanguage;
            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool {
                kind == SyntaxKind::#syntax_kind
            }
            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }
            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> { &self.0 }
        }
        impl #name {
            #(#fields)*
        }
        #(#extra_token_streams)*
    }
}

fn convert_literal_node_to_struct(node: &NodeData, grammar: &Grammar) -> TokenStream {
    assert!(matches!(node.rule, Rule::Alt(_)));
    assert!(node.name.as_str() == "Literal");

    // check variants
    let Rule::Alt(variants) = &node.rule else { unreachable!() };
    if variants.len() != LITERAL_SYNTAX_KIND.len() {
        panic!("unexpected number of literal variants: {:?}", variants);
    }
    for variant in variants {
        match variant {
            Rule::Token(token) => {
                let Some(token_name) = grammar[*token].name.strip_prefix('$') else {
                    panic!("unexpected token name: {:?}", grammar[*token].name)
                };
                if !LITERAL_SYNTAX_KIND.contains(&token_name.to_case(Case::UpperSnake).as_str()) {
                    panic!("unexpected literal token: {:?}", token_name);
                }
            }
            _ => panic!("unexpected Literal variant: {:?}", variant),
        }
    }

    let struct_name = format_ident!("{}", node.name.to_case(Case::Pascal));
    let enum_name = format_ident!("{}Kind", struct_name);
    let syntax_kind = format_ident!("{}", node.name.to_case(Case::UpperSnake));
    quote! {
        pub struct #struct_name(SyntaxNode);
        impl AstNode for #struct_name {
            type Language = LicoLanguage;
            fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool {
                kind == SyntaxKind::#syntax_kind
            }
            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }
            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> { &self.0 }
        }
        impl #struct_name {
            pub fn token(&self) -> Option<SyntaxToken> {
                self.syntax().children_with_tokens()
                    .filter_map(|it| it.into_token()).find(|it| it.kind().is_literal())
            }
            pub fn kind(&self) -> Option<#enum_name> {
                let token = self.token()?;
                let kind = match token.kind() {
                    SyntaxKind::INT    => #enum_name::Int(token),
                    SyntaxKind::FLOAT  => #enum_name::Float(token),
                    SyntaxKind::STRING => #enum_name::String(token),
                    SyntaxKind::TRUE   => #enum_name::Bool(true),
                    SyntaxKind::FALSE  => #enum_name::Bool(false),
                    SyntaxKind::NIL    => #enum_name::Nil,
                    _ => return None,
                };
                Some(kind)
            }
        }
        pub enum #enum_name {
            Int(SyntaxToken),
            Float(SyntaxToken),
            String(SyntaxToken),
            Bool(bool),
            Nil,
        }
    }
}

// (op fields, enum BinaryOp)
fn binary_expr_op_field_and_op_enum(tokens: &[&TokenData]) -> (TokenStream, TokenStream) {
    const BINARY_OPS: &[(&str, &str)] = &[
        ("+", "Add"),
        ("-", "Sub"),
        ("*", "Mul"),
        ("/", "Div"),
        ("%", "Mod"),
        ("<<", "Shl"),
        (">>", "Shr"),
        ("..", "Concat"),
        ("==", "Eq"),
        ("!=", "Ne"),
        ("<", "Lt"),
        (">", "Gt"),
        ("<=", "Le"),
        (">=", "Ge"),
        ("and", "And"),
        ("or", "Or"),
        ("^", "BitXor"),
        ("&", "BitAnd"),
        ("|", "BitOr"),
        ("=", "Assign"),
    ];
    let binary_ops = tokens.iter().map(|token| {
        let Some((symbol, name)) = BINARY_OPS.iter().find(|(p, _)| &token.name == p) else {
            panic!("unknown binary op: {}", token.name)
        };
        let kind = get_token_syntax_kind_name(token);
        (symbol, format_ident!("{}", kind), format_ident!("{}", name))
    });
    let op_fields = {
        let op_match_arms = binary_ops.clone().map(|(_, kind, variant)| {
            quote! { SyntaxKind::#kind => BinaryOp::#variant }
        });
        quote! {
            pub fn op(&self) -> Option<(SyntaxToken, BinaryOp)> {
                self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
                    let op = match token.kind() {
                        #(#op_match_arms),*,
                        _ => return None,
                    };
                    Some((token, op))
                })
            }
            pub fn op_token(&self) -> Option<SyntaxToken> { self.op().map(|(token, _)| token) }
            pub fn op_kind(&self) -> Option<BinaryOp> { self.op().map(|(_, kind)| kind) }
        }
    };
    let op_enum = {
        let op_variants = binary_ops.clone().map(|(_, _, v)| quote! { #v, });
        let op_match_arms = binary_ops.map(|(symbol, _, variant)| {
            let symbol = symbol.to_string();
            quote! { BinaryOp::#variant => #symbol, }
        });
        quote! {
            #[derive(Clone, Copy, PartialEq, Eq)]
            pub enum BinaryOp {
                #(#op_variants)*
            }
            impl BinaryOp {
                pub fn sign_text(&self) -> &'static str {
                    match self {
                        #(#op_match_arms)*
                    }
                }
            }
        }
    };
    (op_fields, op_enum)
}

// (op fields, enum PrefixOp)
fn prefix_expr_op_field_and_op_enum(tokens: &[&TokenData]) -> (TokenStream, TokenStream) {
    const PREFIX_OPS: &[(&str, &str)] =
        &[("+", "Plus"), ("-", "Minus"), ("~", "BitNot"), ("not", "Not"), ("typeof", "TypeOf")];
    let prefix_ops = tokens.iter().map(|token| {
        let Some((symbol, name)) = PREFIX_OPS.iter().find(|(p, _)| &token.name == p) else {
            panic!("unknown prefix op: {}", token.name)
        };
        let kind = get_token_syntax_kind_name(token);
        (symbol, format_ident!("{}", kind), format_ident!("{}", name))
    });
    let op_fields = {
        let op_match_arms = prefix_ops.clone().map(|(_, kind, variant)| {
            quote! { SyntaxKind::#kind => PrefixOp::#variant, }
        });
        quote! {
            pub fn op(&self) -> Option<(SyntaxToken, PrefixOp)> {
                self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map(|token| {
                    let op = match token.kind() {
                        #(#op_match_arms)*
                        _ => return None,
                    };
                    Some((token, op))
                })
            }
            pub fn op_token(&self) -> Option<SyntaxToken> { self.op().map(|(token, _)| token) }
            pub fn op_kind(&self) -> Option<PrefixOp> { self.op().map(|(_, kind)| kind) }
        }
    };
    let op_enum = {
        let op_variants = prefix_ops.clone().map(|(_, _, v)| quote! { #v });
        let op_match_arms = prefix_ops.map(|(symbol, _, variant)| {
            let symbol = symbol.to_string();
            quote! { PrefixOp::#variant => #symbol }
        });
        quote! {
            #[derive(Clone, Copy, PartialEq, Eq)]
            pub enum PrefixOp {
                #(#op_variants),*
            }
            impl PrefixOp {
                pub fn sign_text(&self) -> &'static str {
                    match self {
                        #(#op_match_arms),*
                    }
                }
            }
        }
    };
    (op_fields, op_enum)
}

fn try_simp_seq_to_node_name(rules: &[Rule], grammar: &Grammar) -> Option<String> {
    let mut name = String::new();
    for mut rule in rules {
        while let Rule::Opt(next) = rule {
            rule = next;
        }
        let next = match rule {
            Rule::Node(node) => grammar[*node].name.clone(),
            Rule::Token(token)
                if grammar[*token].name.chars().all(|c| c.is_ascii_punctuation()) =>
            {
                continue;
            }
            Rule::Rep(rule) => match rule as &Rule {
                Rule::Node(node) => grammar[*node].name.clone(),
                Rule::Token(token)
                    if grammar[*token].name.chars().all(|c| c.is_ascii_punctuation()) =>
                {
                    continue;
                }
                Rule::Seq(seq) => try_simp_seq_to_node_name(seq, grammar)?,
                _ => return None,
            },
            _ => return None,
        };
        if name.is_empty() || name == next {
            name = next;
        } else {
            return None;
        }
    }
    Some(name)
}

fn get_token_syntax_kind_name(token: &TokenData) -> String {
    let name = &token.name;
    if let Some(&(_, kind)) = PUNCT_MAP.iter().find(|(punct, _)| *punct == name) {
        return kind.to_case(Case::UpperSnake);
    }
    if let Some(kind) = name.strip_prefix('$') {
        return kind.to_case(Case::UpperSnake);
    }
    if name.chars().all(|c| c.is_ascii_alphabetic()) {
        return format!("{}_KW", name.to_case(Case::UpperSnake));
    }
    panic!("unknown token name: {}", name);
}

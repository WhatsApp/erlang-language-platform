/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::path::Path;

use anyhow::Context;
use anyhow::Result;
use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use serde::Deserialize;
use xshell::Shell;

use crate::project_root;
use crate::reformat;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mode {
    Overwrite,
    Verify,
}

pub struct CodegenCmd {
    pub mode: Mode,
}

impl CodegenCmd {
    pub fn run(self) -> Result<()> {
        let node_types = read_node_types()?;

        let syntax_kinds_file = project_root().join("crates/syntax/src/syntax_kind/generated.rs");
        let syntax_kinds = generate_syntax_kinds(&node_types)?;
        update(&syntax_kinds_file, &syntax_kinds, self.mode)?;

        let ast_file = project_root().join("crates/syntax/src/ast/generated/nodes.rs");
        let ast = generate_ast(&node_types)?;
        update(&ast_file, &ast, self.mode)?;

        Ok(())
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct SymbolId(u16);

#[derive(Debug)]
enum NodeType {
    Enum(SymbolId, Enum),      // sum type
    Node(SymbolId, Node),      // product type
    Punct(SymbolId, String),   // leaf type, not named
    Keyword(SymbolId, String), // leaf type, not named
    Literal(SymbolId, String), // leaf type, always named
}

#[derive(Debug)]
struct Enum {
    nodetype: String,
    named: bool,
    variants: Vec<RawType>,
}

impl Enum {
    fn kinds(&self, enums: &HashMap<String, &Enum>) -> Vec<TokenStream> {
        self.variants
            .iter()
            .map(|var| {
                if let Some(en) = enums.get(&var.nodetype) {
                    let kinds = en.kinds(enums);
                    quote! { #(#kinds)|* }
                } else {
                    let var = var.kind();
                    quote! { #var }
                }
            })
            .collect()
    }
}

#[derive(Debug)]
struct Node {
    nodetype: String,
    named: bool,
    fields: Vec<Field>,
}

#[derive(Debug, Clone)]
enum Field {
    Node {
        name: Ident,
        ty: Ident,
        nth: usize,
    },
    Multiple {
        name: Ident,
        ty: Ident,
    },
    Token {
        name: Ident,
        kind: Ident,
        nth: usize,
    },
}

impl NodeType {
    fn id(&self) -> SymbolId {
        match self {
            NodeType::Punct(id, _) => *id,
            NodeType::Keyword(id, _) => *id,
            NodeType::Literal(id, _) => *id,
            NodeType::Enum(id, _) => *id,
            NodeType::Node(id, _) => *id,
        }
    }

    fn nodetype(&self) -> &str {
        match self {
            NodeType::Enum(_, Enum { nodetype, .. }) => nodetype,
            NodeType::Node(_, Node { nodetype, .. }) => nodetype,
            NodeType::Punct(_, name) => name,
            NodeType::Keyword(_, name) => name,
            NodeType::Literal(_, name) => name,
        }
    }
    fn named(&self) -> bool {
        match self {
            NodeType::Enum(_, Enum { named, .. }) => *named,
            NodeType::Node(_, Node { named, .. }) => *named,
            NodeType::Punct(_, _) => false,
            NodeType::Keyword(_, _) => false,
            NodeType::Literal(_, _) => true,
        }
    }
    fn mapped_name(&self) -> String {
        match map_name(self.nodetype()) {
            Ok(NameType::Punctuation(n)) => n,
            Ok(NameType::Identifier(n)) => n,
            Err(_) => self.nodetype().to_string(),
        }
    }

    fn ty(&self) -> Ident {
        let name = self.mapped_name();
        format_ident!("{}", name)
    }

    fn kind(&self) -> Option<Ident> {
        match self {
            NodeType::Enum(_, _) => None,
            _ => {
                let name = if self.named() {
                    to_upper_snake_case(&self.mapped_name())
                } else {
                    format!("ANON_{}", to_upper_snake_case(&self.mapped_name()))
                };
                Some(format_ident!("{}", name))
            }
        }
    }
}
impl RawType {
    fn mapped_nodetype(&self) -> String {
        match map_name(&self.nodetype) {
            Ok(NameType::Punctuation(n)) => n,
            Ok(NameType::Identifier(n)) => n,
            Err(_) => self.nodetype.to_string(),
        }
    }

    fn ty(&self) -> Ident {
        format_ident!("{}", self.mapped_nodetype())
    }

    fn kind(&self) -> Ident {
        format_ident!("{}", to_upper_snake_case(&self.mapped_nodetype()))
    }
}

fn generate_syntax_kinds(node_types: &[NodeType]) -> Result<String> {
    let all_kinds = node_types.iter().filter_map(|node_type| {
        let id = node_type.id().0;
        let kind = node_type.kind()?;
        Some(quote! { #kind = #id })
    });

    let all_keywords = node_types.iter().filter_map(|node_type| match node_type {
        NodeType::Keyword(_id, _name) => node_type.kind(),
        _ => None,
    });

    let all_puncts = node_types.iter().filter_map(|node_type| match node_type {
        NodeType::Punct(_id, _name) => node_type.kind(),
        _ => None,
    });

    let all_literals = node_types.iter().filter_map(|node_type| match node_type {
        NodeType::Literal(_id, _name) => node_type.kind(),
        _ => None,
    });

    // ---------------------------------

    let max_id = node_types.iter().map(|node| node.id()).max().unwrap().0 + 1;

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]

        use num_derive::{FromPrimitive, ToPrimitive};

        /// The kind of syntax node, e.g. `ATOM`, `IF_KW`, or `DOT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, FromPrimitive, ToPrimitive)]
        #[repr(u16)]
        pub enum SyntaxKind {
            #(#all_kinds,)*
            WHITESPACE = #max_id,
            ERROR = u16::MAX
        }

        use self::SyntaxKind::*;

        impl SyntaxKind {
            #[allow(clippy::match_like_matches_macro)]
            pub fn is_keyword(&self) -> bool {
                match self {
                    #(#all_keywords)|* => true,
                    _ => false
                }
            }

            #[allow(clippy::match_like_matches_macro)]
            pub fn is_punct(&self) -> bool {
                match self {
                    #(#all_puncts)|* => true,
                    _ => false
                }
            }

            #[allow(clippy::match_like_matches_macro)]
            pub fn is_literal(&self) -> bool {
                match self {
                    #(#all_literals)|* => true,
                    _ => false
                }
            }

            pub fn is_token(&self) -> bool {
                self.is_keyword() || self.is_punct() || self.is_literal()
            }
        }

        /// Tell emacs to automatically reload this file if it changes
        /// Local Variables:
        /// auto-revert-mode: 1
        /// End:
        fn _dummy() -> bool { false }
    };

    reformat(&ast.to_string())
}

// ---------------------------------------------------------------------

/// Generate an actual AST, modelled on the way rust-analyzer does it,
/// but using the tree-sitter node info.
fn generate_ast(node_types: &[NodeType]) -> Result<String> {
    let enums: HashMap<String, &Enum> = node_types
        .iter()
        .filter_map(|node| match node {
            NodeType::Enum(_, en) => Some((en.nodetype.clone(), en)),
            _ => None,
        })
        .collect();

    let defs = node_types.iter().filter_map(|node| match node {
        NodeType::Keyword(_, _) => None,
        NodeType::Punct(_, _) => None,
        NodeType::Literal(_id, _name) => {
            let kind = node.kind()?;
            let name = node.ty();

            Some(quote! {
                /// Via NodeType::Literal 2
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #name {
                    pub(crate) syntax: SyntaxNode,
                }

                /// Via NodeType::Literal 2
                impl #name {
                    pub fn self_token(&self) -> Option<SyntaxToken> { support::token(&self.syntax, #kind, 0) }

                }

                /// Via NodeType::Literal 2
                impl AstNode for #name {
                    fn can_cast(kind: SyntaxKind) -> bool {
                        kind == #kind
                    }
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        if Self::can_cast(syntax.kind()) {
                            Some(Self { syntax })
                        } else {
                            None
                        }
                    }
                    fn syntax(&self) -> &SyntaxNode { &self.syntax }
                }

            })
        }
        NodeType::Enum(_id, en) => {
            let variants: Vec<_> = en.variants.iter().map(|var| var.ty()).collect();
            let name = node.ty();
            let kinds = en.kinds(&enums);
            let cast_body = en
                .variants
                .iter()
                .map(|variant| {
                    let var = variant.ty();
                    if enums.contains_key(&variant.nodetype) {
                        // This cast should always succeed
                        quote! {
                            Some(#name::#var(#var::cast(syntax).unwrap()))
                        }
                    } else {
                        quote! {
                            Some(#name::#var(#var { syntax }))
                        }
                    }
                });

            Some(quote! {
                /// Via NodeType::Enum 2
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub enum #name {
                    #(#variants(#variants),)*
                }

                impl AstNode for #name {
                    #[allow(clippy::match_like_matches_macro)]
                    fn can_cast(kind: SyntaxKind) -> bool {
                        match kind {
                            #(#kinds)|* => true,
                            _ => false,
                        }
                    }
                    #[allow(clippy::match_like_matches_macro)]
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        match syntax.kind() {
                            #(
                                #kinds => #cast_body,
                            )*
                            _ => None,
                        }
                    }
                    fn syntax(&self) -> &SyntaxNode {
                        match self {
                            #(
                                #name::#variants(it) => it.syntax(),
                            )*
                        }
                    }
                }

                /// Via NodeType::Enum 2 forms
                #(
                    impl From<#variants> for #name {
                        fn from(node: #variants) -> #name {
                            #name::#variants(node)
                        }
                    }
                )*

                /// Via NodeType::Enum 2 display
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            })
        }
        NodeType::Node(_id, node1) => {
            let kind = node.kind()?;
            let name = node.ty();

            let methods = node1.fields.iter().map(|field| {
                match field {
                    Field::Multiple { name, ty } => {
                        quote!{
                            pub fn #name(&self) -> AstChildren<#ty> {
                                support::children(&self.syntax)
                            }
                        }
                    },
                    Field::Node { name, ty, nth } => {
                        quote! {
                            pub fn #name(&self) -> Option<#ty> {
                                support::child(&self.syntax, #nth)
                            }
                        }
                    }
                    Field::Token { name, kind, nth } => {
                        quote! {
                            pub fn #name(&self) -> Option<SyntaxToken> {
                                support::token(&self.syntax, #kind, #nth)
                            }
                        }
                    },
                }
            });

            Some(quote!{
                /// Via NodeType::Node 2 struct inner
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #name {
                    pub(crate) syntax: SyntaxNode,
                }

                impl #name {
                    #(#methods)*
                }

                /// Via NodeType::Node 2 struct
                impl AstNode for #name {
                    fn can_cast(kind: SyntaxKind) -> bool {
                        kind == #kind
                    }

                    /// Via field_casts
                    fn cast(syntax: SyntaxNode) -> Option<Self> {
                        if Self::can_cast(syntax.kind()) {
                            Some(Self { syntax })
                        } else {
                            None
                        }
                    }
                    fn syntax(&self) -> &SyntaxNode { &self.syntax }

                }

                /// Via NodeType::Node 2 display
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            })
        }
    });

    let ast = quote! {

        #![allow(dead_code)]
        /// TODO: remove this pragma
        use crate::{
            ast::{support, AstChildren, AstNode},
            SyntaxKind::{self, *},
            SyntaxNode, SyntaxToken,
        };

        #(#defs)*

        /// Tell emacs to automatically reload this file if it changes
        /// Local Variables:
        /// auto-revert-mode: 1
        /// End:
        fn _dummy() -> bool {
            false
        }
    };

    reformat(&ast.to_string())
}

// ---------------------------------------------------------------------

fn update(path: &Path, contents: &str, mode: Mode) -> Result<()> {
    let sh = Shell::new()?;
    match sh.read_file(path) {
        Ok(old_contents) if old_contents == contents => {
            return Ok(());
        }
        _ => {}
    }
    if mode == Mode::Verify {
        anyhow::bail!("`{}` is not up-to-date", path.display());
    }
    eprintln!("updating {}", path.display());
    sh.write_file(path, contents)?;
    Ok(())
}

#[derive(Debug)]
enum NameType {
    Punctuation(String),
    Identifier(String),
}

// ---------------------------------------------------------------------

/// See
/// https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types
/// for what these mean

#[derive(Deserialize, Debug)]
struct RawNodeType {
    #[serde(rename = "type")]
    #[serde(default)]
    nodetype: String,
    #[serde(default)]
    named: bool,
    subtypes: Option<Vec<RawType>>,
    fields: Option<BTreeMap<String, RawField>>,
    children: Option<RawField>,
}

#[derive(Deserialize, Debug, Clone)]
struct RawField {
    multiple: bool,
    types: Vec<RawType>,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
struct RawType {
    // A syntax node's type is a string that indicates which grammar
    // rule the node represents.  Every node has a type
    #[serde(rename = "type")]
    nodetype: String,
    named: bool,
}

// ---------------------------------------------------------------------

fn read_node_types() -> Result<Vec<NodeType>> {
    let node_types_string = tree_sitter_erlang::NODE_TYPES;
    let node_types: Vec<RawNodeType> = serde_json::from_str(node_types_string)
        .with_context(|| "parsing node types from tree_sitter_erlang::NODE_TYPES".to_string())?;
    let language = tree_sitter_erlang::language();

    println!("--------------------------------");
    println!(
        "node_types {:?} {:?} {:?}",
        language,
        language.version(),
        language.node_kind_count()
    );

    println!("--------------------------------");
    // Note: language.node_kind_is_visible is not trustworthy, as per
    // https://github.com/tree-sitter/tree-sitter/issues/997.  If we
    // care, for now, we need that flag true AND no leading underscore
    // on the name.
    // 2021-03-31: using (unpublished) 0.19.4, which has a correct
    // implementation
    for i in 0..language.node_kind_count() as u16 {
        println!(
            "node_kind {:?} {:?} named:{:?} visible:{:?}",
            i,
            language.node_kind_for_id(i).unwrap(),
            language.node_kind_is_named(i),
            language.node_kind_is_visible(i)
        );
    }
    println!("--------------------------------");
    for id in 1..language.field_count() as u16 {
        let name = language.field_name_for_id(id).unwrap();
        println!("all_field_kinds {:?} {:?} {:?}", id, name, map_name(name));
    }
    println!("--------------------------------");
    // for i in 1..language.symb() {
    //     println!("node_kind {:?} {:?}", i, language.node_kind_for_id(i as u16));
    // }
    println!("--------------------------------");
    // Node categorisation
    // ~~~~~~~~~~~~~~~~~~~
    // subtypes | fields   | children
    // ---------+----------+---------
    //  present | x        | x        => ProperEnum
    //  empty   | empty    | nonempty => Product/Node or Enum, based on note 3
    //  empty   | nonempty | empty    => Product/Node
    //  empty   | empty    | nonempty => Product/Node
    //  empty   | empty    | empty    => Leaf
    //
    // Note: 1) The nodetype is the name of the rule that generated the node.
    //       2) If the rule is not reachable from the top level rule in the
    //          grammar, it will not be assigned a SymbolId, which is indicated
    //          by the `id_for_node_kind` call returning zero
    //       3) should be Enum: children: multiple type, multiple:False, required True
    let mut node_types = node_types
        .iter()
        .filter_map::<Result<NodeType, anyhow::Error>, _>(|node| {
            let id = SymbolId(language.id_for_node_kind(&node.nodetype, node.named));
            println!("processing {:?}", node.nodetype);
            if node.nodetype == "expression" {
                println!("processing {:?}", node);
            }
            if id.0 == 65535 {
                None
            } else {
                match node {
                    RawNodeType {
                        nodetype,
                        children: Some(_),
                        ..
                    } => {
                        panic!(
                            "raw children without fields assigned are not supported, in {}",
                            nodetype
                        );
                    }
                    RawNodeType {
                        nodetype,
                        named,
                        subtypes: Some(subtys),
                        children,
                        fields,
                    } => {
                        assert!(children.is_none());
                        assert!(fields.is_none());
                        let variants = subtys.clone();
                        let nodetype = nodetype.clone();
                        let named = *named;
                        Some(Ok(NodeType::Enum(
                            id,
                            Enum {
                                nodetype,
                                named,
                                variants,
                            },
                        )))
                    }
                    // Start of leaf types
                    RawNodeType {
                        nodetype,
                        named: true,
                        subtypes: None,
                        fields: None,
                        children: None,
                    } => {
                        println!("got leaf node Literal: {:?}", to_camel_case(nodetype));
                        Some(Ok(NodeType::Literal(id, to_camel_case(nodetype))))
                    }
                    RawNodeType {
                        nodetype,
                        named: false,
                        subtypes: None,
                        fields: None,
                        children: None,
                    } => match map_name(nodetype).ok()? {
                        NameType::Punctuation(name) => Some(Ok(NodeType::Punct(id, name))),
                        NameType::Identifier(name) => Some(Ok(NodeType::Keyword(id, name))),
                    },

                    // End of leaf types
                    RawNodeType {
                        nodetype,
                        named,
                        subtypes: None,
                        fields,
                        children: None,
                    } => {
                        let fields = match fields {
                            Some(fields) => build_fields(fields),
                            None => Vec::new(),
                        };
                        let nodetype = nodetype.clone();
                        let named = *named;
                        Some(Ok(NodeType::Node(
                            id,
                            Node {
                                nodetype,
                                named,
                                fields,
                            },
                        )))
                    }
                }
            }
        })
        .collect::<Result<Vec<_>>>()?;

    node_types.sort_by(|a, b| a.mapped_name().cmp(&b.mapped_name()));
    Ok(node_types)
}

fn map_name(name: &str) -> Result<NameType> {
    use NameType::*;

    // AZ updated from parser.c ts_symbol_names
    match name {
        "-" => Ok(Punctuation("Dash".into())),
        "(" => Ok(Punctuation("Lparen".into())),
        ")" => Ok(Punctuation("Rparen".into())),
        "." => Ok(Punctuation("Dot".into())),
        // module
        // export
        // export_type
        "[" => Ok(Punctuation("Lbrack".into())),
        "/" => Ok(Punctuation("Slash".into())),
        "," => Ok(Punctuation("Comma".into())),
        "]" => Ok(Punctuation("Rbrack".into())),
        ";" => Ok(Punctuation("Semi".into())),
        // comment
        // type
        // opaque
        "::" => Ok(Punctuation("ColonColon".into())),
        // spec
        // callback
        "->" => Ok(Punctuation("DashGt".into())),
        "|" => Ok(Punctuation("Pipe".into())),
        ":" => Ok(Punctuation("Colon".into())),
        "<<" => Ok(Punctuation("LtLt".into())),
        "_" => Ok(Punctuation("Underscore".into())), // changed name
        "*" => Ok(Punctuation("Star".into())),
        ">>" => Ok(Punctuation("GtGt".into())),
        // fun
        "..." => Ok(Punctuation("DotDotDot".into())),
        ".." => Ok(Punctuation("DotDot".into())),
        "{" => Ok(Punctuation("Lbrace".into())),
        "}" => Ok(Punctuation("Rrace".into())),
        "#" => Ok(Punctuation("Pound".into())),
        "=>" => Ok(Punctuation("EqGt".into())),
        ":=" => Ok(Punctuation("ColonEq".into())),
        // try
        // of
        // end
        // catch
        // after
        // throw
        // begin
        "||" => Ok(Punctuation("PipePipe".into())),
        "<-" => Ok(Punctuation("LtDash".into())),
        "<=" => Ok(Punctuation("LtEq".into())),
        "+" => Ok(Punctuation("Plus".into())),
        // bnot
        // not
        "/=" => Ok(Punctuation("SlashEq".into())),
        "<" => Ok(Punctuation("Lt".into())),
        "=/=" => Ok(Punctuation("EqSlashEq".into())),
        "=:=" => Ok(Punctuation("EqColonEq".into())),
        "=<" => Ok(Punctuation("EqLt".into())),
        "==" => Ok(Punctuation("EqEq".into())),
        ">" => Ok(Punctuation("Gt".into())),
        ">=" => Ok(Punctuation("GtEq".into())),
        // and
        // andalso
        // band
        // bor
        // bsl
        // bsr
        // bxor
        // div
        // or
        // orelse
        // rem
        // xor
        "=!" => Ok(Punctuation("EqBang".into())),
        "++" => Ok(Punctuation("PlusPlus".into())),
        "--" => Ok(Punctuation("DashDash".into())),
        "!" => Ok(Punctuation("Bang".into())),
        // receive
        // if
        // case
        // when
        "=" => Ok(Punctuation("Eq".into())),
        "?" => Ok(Punctuation("Qmark".into())),
        "?=" => Ok(Punctuation("QmarkEq".into())),
        // variable
        "'" => Ok(Punctuation("Squote".into())),
        // float
        "\"" => Ok(Punctuation("Dquote".into())),
        "$" => Ok(Punctuation("Dollar".into())),
        "'_'" => Ok(Punctuation("DeprecatedWildcard".into())),

        // "@" => Ok(Punctuation("At".into())),
        // "%" => Ok(Punctuation("Percent".into())),
        // "^" => Ok(Punctuation("Caret".into())),
        // "&" => Ok(Punctuation("Amp".into())),
        // "\\" => Ok(Punctuation("Bslash".into())),
        _ => {
            if name
                .chars()
                .all(|c| ('a'..='z').contains(&c) || ('0'..='9').contains(&c) || c == '_')
            {
                Ok(Identifier(to_camel_case(name)))
            } else {
                anyhow::bail!("invalid node name: {}", name)
            }
        }
    }
}

// ---------------------------------------------------------------------

fn to_upper_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_uppercase());
    }
    buf
}

fn to_camel_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut new_word = true;
    for c in s.chars() {
        if c == '_' {
            new_word = true;
        } else if new_word {
            buf.push(c.to_ascii_uppercase());
            new_word = false;
        } else {
            buf.push(c.to_ascii_lowercase());
        }
    }
    buf
}

fn _map_type(child: &RawType) -> Result<String> {
    match map_name(&child.nodetype)? {
        NameType::Punctuation(name) => Ok(name),
        NameType::Identifier(name) if child.named => Ok(name),
        NameType::Identifier(name) => Ok(name + "Kw"),
    }
}

fn build_fields(fields: &BTreeMap<String, RawField>) -> Vec<Field> {
    let mut mapped = Vec::new();
    let mut last_ty = None;
    let mut nth = 0;
    let mut fields: Vec<(&String, &RawField, &RawType)> = fields
        .iter()
        .map(|(k, v)| {
            let ty = match &v.types[..] {
                [ty] => ty,
                // we have some types overlapping with _expr, ignore them
                [_, ty] if ty.nodetype == "_expr" => ty,
                [] => panic!("field with no types"),
                _ => panic!("fields with multiple types are not supported {}", k),
            };
            (k, v, ty)
        })
        .collect();
    fields.sort_by_key(|(_, _, ty)| (*ty).clone());

    for (name, field, ty) in fields {
        if last_ty == Some(ty) {
            nth += 1;
        } else {
            last_ty = Some(ty);
            nth = 0;
        }

        let name = format_ident!("{}", name);

        if field.multiple {
            assert!(ty.named);
            assert!(nth == 0);
            mapped.push(Field::Multiple { name, ty: ty.ty() });
        } else if ty.named {
            mapped.push(Field::Node {
                name,
                ty: ty.ty(),
                nth,
            });
        } else {
            mapped.push(Field::Token {
                name,
                kind: ty.kind(),
                nth,
            });
        }
    }

    mapped
}

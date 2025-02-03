//! @generated file, do not edit by hand, see `xtask/src/codegen.rs`

#![allow(bad_style, missing_docs, unreachable_pub)]
use num_derive::{FromPrimitive, ToPrimitive};
#[doc = r" The kind of syntax node, e.g. `ATOM`, `IF_KW`, or `DOT`."]
#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Debug,
    FromPrimitive,
    ToPrimitive
)]
#[repr(u16)]
pub enum SyntaxKind {
    ANON_AFTER = 99u16,
    ANON_AND = 109u16,
    ANON_ANDALSO = 80u16,
    ANN_TYPE = 190u16,
    ANN_VAR = 191u16,
    ANONYMOUS_FUN = 259u16,
    ARITY = 262u16,
    ATOM = 1u16,
    ATTR_NAME = 185u16,
    B_GENERATOR = 227u16,
    ANON_BAND = 108u16,
    ANON_BANG = 78u16,
    ANON_BEGIN = 81u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 155u16,
    BIN_ELEMENT = 213u16,
    BINARY = 212u16,
    BINARY_COMPREHENSION = 222u16,
    BINARY_OP_EXPR = 204u16,
    BIT_SIZE_EXPR = 214u16,
    BIT_TYPE_LIST = 215u16,
    BIT_TYPE_UNIT = 220u16,
    BLOCK_EXPR = 210u16,
    ANON_BNOT = 104u16,
    ANON_BOR = 110u16,
    ANON_BSL = 112u16,
    ANON_BSR = 113u16,
    ANON_BXOR = 111u16,
    CALL = 248u16,
    CALLBACK = 180u16,
    ANON_CALLBACK = 67u16,
    ANON_CASE = 96u16,
    CASE_EXPR = 251u16,
    ANON_CATCH = 75u16,
    CATCH_CLAUSE = 268u16,
    CATCH_EXPR = 201u16,
    CHAR = 132u16,
    CLAUSE_BODY = 199u16,
    ANON_COLON = 3u16,
    ANON_COLON_COLON = 62u16,
    ANON_COLON_EQ = 95u16,
    ANON_COMMA = 30u16,
    COMMENT = 133u16,
    ANON_COMPILE = 47u16,
    COMPILE_OPTIONS_ATTRIBUTE = 161u16,
    CONCATABLES = 294u16,
    COND_MATCH_EXPR = 203u16,
    CR_CLAUSE = 254u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 117u16,
    ANON_DASH_GT = 70u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 51u16,
    DEPRECATED_ATTRIBUTE = 163u16,
    DEPRECATED_FA = 168u16,
    DEPRECATED_FAS = 167u16,
    DEPRECATED_MODULE = 166u16,
    DEPRECATED_WILDCARD = 57u16,
    DEPRECATION_DESC = 169u16,
    ANON_DIV = 106u16,
    ANON_DOT = 4u16,
    ANON_DOT_DOT = 73u16,
    DOTDOTDOT = 74u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 82u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 76u16,
    ANON_EQ_COLON_EQ = 124u16,
    ANON_EQ_EQ = 118u16,
    ANON_EQ_GT = 94u16,
    ANON_EQ_LT = 120u16,
    ANON_EQ_SLASH_EQ = 125u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 156u16,
    ANON_EXPORT_TYPE = 45u16,
    EXPORT_TYPE_ATTRIBUTE = 160u16,
    EXPR_ARGS = 290u16,
    EXTERNAL_FUN = 258u16,
    FA = 159u16,
    ANON_FEATURE = 53u16,
    FEATURE_ATTRIBUTE = 164u16,
    FIELD_EXPR = 246u16,
    FIELD_TYPE = 247u16,
    ANON_FILE = 49u16,
    FILE_ATTRIBUTE = 162u16,
    FLOAT = 128u16,
    ANON_FUN = 72u16,
    FUN_CLAUSE = 264u16,
    FUN_DECL = 186u16,
    FUN_TYPE = 193u16,
    FUN_TYPE_SIG = 194u16,
    FUNCTION_CLAUSE = 197u16,
    GENERATOR = 226u16,
    ANON_GT = 123u16,
    ANON_GT_EQ = 122u16,
    ANON_GT_GT = 84u16,
    GUARD = 292u16,
    GUARD_CLAUSE = 293u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 250u16,
    IF_EXPR = 249u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 157u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 127u16,
    INTERNAL_FUN = 257u16,
    ANON_LBRACE = 55u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 224u16,
    LIST = 211u16,
    LIST_COMPREHENSION = 221u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 121u16,
    ANON_LT_COLON_DASH = 91u16,
    ANON_LT_COLON_EQ = 93u16,
    ANON_LT_DASH = 90u16,
    ANON_LT_EQ = 92u16,
    ANON_LT_LT = 83u16,
    MACRO_CALL_ARGS = 286u16,
    MACRO_CALL_EXPR = 285u16,
    MACRO_EXPR = 289u16,
    MACRO_LHS = 283u16,
    MACRO_STRING = 288u16,
    MAP_COMPREHENSION = 223u16,
    MAP_EXPR = 233u16,
    MAP_EXPR_UPDATE = 232u16,
    MAP_FIELD = 235u16,
    MAP_GENERATOR = 228u16,
    MATCH_EXPR = 202u16,
    ANON_MAYBE = 101u16,
    MAYBE_EXPR = 274u16,
    MODULE = 183u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 154u16,
    MULTI_STRING = 171u16,
    ANON_NOT = 105u16,
    ANON_OF = 97u16,
    OPAQUE = 175u16,
    ANON_OPAQUE = 60u16,
    ANON_OPTIONAL_CALLBACKS = 43u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 158u16,
    ANON_OR = 114u16,
    ANON_ORELSE = 79u16,
    PAREN_EXPR = 209u16,
    PIPE = 192u16,
    ANON_PIPE = 71u16,
    ANON_PIPE_PIPE = 89u16,
    ANON_PLUS = 103u16,
    ANON_PLUS_PLUS = 116u16,
    ANON_POUND = 88u16,
    PP_DEFINE = 152u16,
    PP_ELIF = 151u16,
    PP_ELSE = 148u16,
    PP_ENDIF = 149u16,
    PP_IF = 150u16,
    PP_IFDEF = 146u16,
    PP_IFNDEF = 147u16,
    PP_INCLUDE = 143u16,
    PP_INCLUDE_LIB = 144u16,
    PP_UNDEF = 145u16,
    ANON_QMARK = 102u16,
    ANON_QMARK_EQ = 77u16,
    RANGE_TYPE = 195u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 98u16,
    RECEIVE_AFTER = 256u16,
    RECEIVE_EXPR = 255u16,
    ANON_RECORD = 63u16,
    RECORD_DECL = 178u16,
    RECORD_EXPR = 240u16,
    RECORD_FIELD = 245u16,
    RECORD_FIELD_EXPR = 238u16,
    RECORD_FIELD_NAME = 242u16,
    RECORD_INDEX_EXPR = 237u16,
    RECORD_NAME = 241u16,
    RECORD_UPDATE_EXPR = 239u16,
    ANON_REM = 107u16,
    REMOTE = 207u16,
    REMOTE_MODULE = 208u16,
    REPLACEMENT_CR_CLAUSES = 278u16,
    REPLACEMENT_EXPR_GUARD = 281u16,
    REPLACEMENT_FUNCTION_CLAUSES = 277u16,
    REPLACEMENT_GUARD_AND = 280u16,
    REPLACEMENT_GUARD_OR = 279u16,
    REPLACEMENT_PARENS = 282u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 56u16,
    ANON_SEMI = 69u16,
    ANON_SLASH = 85u16,
    ANON_SLASH_EQ = 119u16,
    SOURCE_FILE = 137u16,
    SPEC = 179u16,
    ANON_SPEC = 65u16,
    ANON_SSR = 2u16,
    SSR_DEFINITION = 139u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 140u16,
    SSR_WHEN = 141u16,
    ANON_STAR = 86u16,
    STRING = 301u16,
    ANON_TRY = 100u16,
    TRY_AFTER = 267u16,
    TRY_CLASS = 269u16,
    TRY_EXPR = 265u16,
    TRY_STACK = 270u16,
    TUPLE = 231u16,
    ANON_TYPE = 58u16,
    TYPE_ALIAS = 174u16,
    TYPE_GUARDS = 189u16,
    TYPE_NAME = 177u16,
    TYPE_SIG = 188u16,
    UNARY_OP_EXPR = 205u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 87u16,
    VAR = 126u16,
    VAR_ARGS = 291u16,
    ANON_WHEN = 6u16,
    WILD_ATTRIBUTE = 184u16,
    ANON_XOR = 115u16,
    WHITESPACE = 302u16,
    ERROR = u16::MAX,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_keyword(&self) -> bool {
        match self {
            ANON_AFTER
            | ANON_AND
            | ANON_ANDALSO
            | ANON_BAND
            | ANON_BEGIN
            | ANON_BEHAVIOR
            | ANON_BEHAVIOUR
            | ANON_BNOT
            | ANON_BOR
            | ANON_BSL
            | ANON_BSR
            | ANON_BXOR
            | ANON_CALLBACK
            | ANON_CASE
            | ANON_CATCH
            | ANON_COMPILE
            | ANON_DEFINE
            | ANON_DEPRECATED
            | ANON_DIV
            | ANON_ELIF
            | ANON_ELSE
            | ANON_END
            | ANON_ENDIF
            | ANON_EXPORT
            | ANON_EXPORT_TYPE
            | ANON_FEATURE
            | ANON_FILE
            | ANON_FUN
            | ANON_IF
            | ANON_IFDEF
            | ANON_IFNDEF
            | ANON_IMPORT
            | ANON_INCLUDE
            | ANON_INCLUDE_LIB
            | ANON_MAYBE
            | ANON_MODULE
            | ANON_NOT
            | ANON_OF
            | ANON_OPAQUE
            | ANON_OPTIONAL_CALLBACKS
            | ANON_OR
            | ANON_ORELSE
            | ANON_RECEIVE
            | ANON_RECORD
            | ANON_REM
            | ANON_SPEC
            | ANON_SSR
            | ANON_TRY
            | ANON_TYPE
            | ANON_UNDEF
            | ANON_UNIT
            | ANON_WHEN
            | ANON_XOR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_punct(&self) -> bool {
        match self {
            ANON_BANG | ANON_COLON | ANON_COLON_COLON | ANON_COLON_EQ | ANON_COMMA | ANON_DASH
            | ANON_DASH_DASH | ANON_DASH_GT | ANON_DOT | ANON_DOT_DOT | ANON_EQ
            | ANON_EQ_COLON_EQ | ANON_EQ_EQ | ANON_EQ_GT | ANON_EQ_LT | ANON_EQ_SLASH_EQ
            | ANON_GT | ANON_GT_EQ | ANON_GT_GT | ANON_LBRACE | ANON_LBRACK | ANON_LPAREN
            | ANON_LT | ANON_LT_COLON_DASH | ANON_LT_COLON_EQ | ANON_LT_DASH | ANON_LT_EQ
            | ANON_LT_LT | ANON_PIPE | ANON_PIPE_PIPE | ANON_PLUS | ANON_PLUS_PLUS | ANON_POUND
            | ANON_QMARK | ANON_QMARK_EQ | ANON_RBRACK | ANON_RPAREN | ANON_RRACE | ANON_SEMI
            | ANON_SLASH | ANON_SLASH_EQ | ANON_SSR_MATCH | ANON_STAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_literal(&self) -> bool {
        match self {
            ATOM | CHAR | COMMENT | DEPRECATED_WILDCARD | DOTDOTDOT | FLOAT | INTEGER | VAR => true,
            _ => false,
        }
    }
    pub fn is_token(&self) -> bool {
        self.is_keyword() || self.is_punct() || self.is_literal()
    }
}
#[doc = r" Tell emacs to automatically reload this file if it changes"]
#[doc = r" Local Variables:"]
#[doc = r" auto-revert-mode: 1"]
#[doc = r" End:"]
fn _dummy() -> bool {
    false
}

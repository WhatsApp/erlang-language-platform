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
    ANON_AFTER = 93u16,
    ANON_AND = 103u16,
    ANON_ANDALSO = 76u16,
    ANN_TYPE = 174u16,
    ANN_VAR = 175u16,
    ANONYMOUS_FUN = 241u16,
    ARITY = 244u16,
    ATOM = 1u16,
    ATTR_NAME = 170u16,
    B_GENERATOR = 211u16,
    ANON_BAND = 102u16,
    ANON_BANG = 74u16,
    ANON_BEGIN = 77u16,
    ANON_BEHAVIOR = 31u16,
    ANON_BEHAVIOUR = 29u16,
    BEHAVIOUR_ATTRIBUTE = 141u16,
    BIN_ELEMENT = 197u16,
    BINARY = 196u16,
    BINARY_COMPREHENSION = 206u16,
    BINARY_OP_EXPR = 188u16,
    BIT_SIZE_EXPR = 198u16,
    BIT_TYPE_LIST = 199u16,
    BIT_TYPE_UNIT = 204u16,
    BLOCK_EXPR = 194u16,
    ANON_BNOT = 98u16,
    ANON_BOR = 104u16,
    ANON_BSL = 106u16,
    ANON_BSR = 107u16,
    ANON_BXOR = 105u16,
    CALL = 230u16,
    CALLBACK = 165u16,
    ANON_CALLBACK = 61u16,
    ANON_CASE = 90u16,
    CASE_EXPR = 233u16,
    ANON_CATCH = 71u16,
    CATCH_CLAUSE = 250u16,
    CATCH_EXPR = 185u16,
    CHAR = 124u16,
    CLAUSE_BODY = 183u16,
    ANON_COLON = 64u16,
    ANON_COLON_COLON = 56u16,
    ANON_COLON_EQ = 89u16,
    ANON_COMMA = 26u16,
    COMMENT = 125u16,
    ANON_COMPILE = 43u16,
    COMPILE_OPTIONS_ATTRIBUTE = 147u16,
    CONCATABLES = 275u16,
    COND_MATCH_EXPR = 187u16,
    CR_CLAUSE = 236u16,
    ANON_DASH = 2u16,
    ANON_DASH_DASH = 111u16,
    ANON_DASH_GT = 65u16,
    ANON_DEFINE = 24u16,
    ANON_DEPRECATED = 47u16,
    DEPRECATED_ATTRIBUTE = 149u16,
    DEPRECATED_FA = 153u16,
    DEPRECATED_FAS = 152u16,
    DEPRECATED_MODULE = 151u16,
    DEPRECATED_WILDCARD = 51u16,
    DEPRECATION_DESC = 154u16,
    ANON_DIV = 100u16,
    ANON_DOT = 7u16,
    ANON_DOT_DOT = 69u16,
    DOTDOTDOT = 70u16,
    ANON_ELIF = 22u16,
    ANON_ELSE = 16u16,
    ANON_END = 78u16,
    ANON_ENDIF = 18u16,
    ANON_EQ = 72u16,
    ANON_EQ_COLON_EQ = 118u16,
    ANON_EQ_EQ = 112u16,
    ANON_EQ_GT = 88u16,
    ANON_EQ_LT = 114u16,
    ANON_EQ_SLASH_EQ = 119u16,
    ANON_EXPORT = 33u16,
    EXPORT_ATTRIBUTE = 142u16,
    ANON_EXPORT_TYPE = 41u16,
    EXPORT_TYPE_ATTRIBUTE = 146u16,
    EXPR_ARGS = 271u16,
    EXTERNAL_FUN = 240u16,
    FA = 145u16,
    FIELD_EXPR = 228u16,
    FIELD_TYPE = 229u16,
    ANON_FILE = 45u16,
    FILE_ATTRIBUTE = 148u16,
    FLOAT = 122u16,
    ANON_FUN = 68u16,
    FUN_CLAUSE = 246u16,
    FUN_DECL = 171u16,
    FUN_TYPE = 177u16,
    FUN_TYPE_SIG = 178u16,
    FUNCTION_CLAUSE = 181u16,
    GENERATOR = 210u16,
    ANON_GT = 117u16,
    ANON_GT_EQ = 116u16,
    ANON_GT_GT = 80u16,
    GUARD = 273u16,
    GUARD_CLAUSE = 274u16,
    ANON_IF = 20u16,
    IF_CLAUSE = 232u16,
    IF_EXPR = 231u16,
    ANON_IFDEF = 12u16,
    ANON_IFNDEF = 14u16,
    ANON_IMPORT = 37u16,
    IMPORT_ATTRIBUTE = 143u16,
    ANON_INCLUDE = 3u16,
    ANON_INCLUDE_LIB = 8u16,
    INTEGER = 121u16,
    INTERNAL_FUN = 239u16,
    ANON_LBRACE = 49u16,
    ANON_LBRACK = 35u16,
    LC_EXPRS = 208u16,
    LIST = 195u16,
    LIST_COMPREHENSION = 205u16,
    ANON_LPAREN = 5u16,
    ANON_LT = 115u16,
    ANON_LT_DASH = 86u16,
    ANON_LT_EQ = 87u16,
    ANON_LT_LT = 79u16,
    MACRO_CALL_ARGS = 267u16,
    MACRO_CALL_EXPR = 266u16,
    MACRO_EXPR = 270u16,
    MACRO_LHS = 264u16,
    MACRO_STRING = 269u16,
    MAP_COMPREHENSION = 207u16,
    MAP_EXPR = 215u16,
    MAP_EXPR_UPDATE = 214u16,
    MAP_FIELD = 217u16,
    MAP_GENERATOR = 212u16,
    MATCH_EXPR = 186u16,
    ANON_MAYBE = 95u16,
    MAYBE_EXPR = 256u16,
    MODULE = 168u16,
    ANON_MODULE = 27u16,
    MODULE_ATTRIBUTE = 140u16,
    MULTI_STRING = 156u16,
    ANON_NOT = 99u16,
    ANON_OF = 91u16,
    OPAQUE = 160u16,
    ANON_OPAQUE = 54u16,
    ANON_OPTIONAL_CALLBACKS = 39u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 144u16,
    ANON_OR = 108u16,
    ANON_ORELSE = 75u16,
    PAREN_EXPR = 193u16,
    PIPE = 176u16,
    ANON_PIPE = 67u16,
    ANON_PIPE_PIPE = 85u16,
    ANON_PLUS = 97u16,
    ANON_PLUS_PLUS = 110u16,
    ANON_POUND = 84u16,
    PP_DEFINE = 138u16,
    PP_ELIF = 137u16,
    PP_ELSE = 134u16,
    PP_ENDIF = 135u16,
    PP_IF = 136u16,
    PP_IFDEF = 132u16,
    PP_IFNDEF = 133u16,
    PP_INCLUDE = 129u16,
    PP_INCLUDE_LIB = 130u16,
    PP_UNDEF = 131u16,
    ANON_QMARK = 96u16,
    ANON_QMARK_EQ = 73u16,
    RANGE_TYPE = 179u16,
    ANON_RBRACK = 36u16,
    ANON_RECEIVE = 92u16,
    RECEIVE_AFTER = 238u16,
    RECEIVE_EXPR = 237u16,
    ANON_RECORD = 57u16,
    RECORD_DECL = 163u16,
    RECORD_EXPR = 222u16,
    RECORD_FIELD = 227u16,
    RECORD_FIELD_EXPR = 220u16,
    RECORD_FIELD_NAME = 224u16,
    RECORD_INDEX_EXPR = 219u16,
    RECORD_NAME = 223u16,
    RECORD_UPDATE_EXPR = 221u16,
    ANON_REM = 101u16,
    REMOTE = 191u16,
    REMOTE_MODULE = 192u16,
    REPLACEMENT_CR_CLAUSES = 260u16,
    REPLACEMENT_FUNCTION_CLAUSES = 259u16,
    REPLACEMENT_GUARD_AND = 262u16,
    REPLACEMENT_GUARD_OR = 261u16,
    REPLACEMENT_PARENS = 263u16,
    ANON_RPAREN = 6u16,
    ANON_RRACE = 50u16,
    ANON_SEMI = 63u16,
    ANON_SLASH = 81u16,
    ANON_SLASH_EQ = 113u16,
    SOURCE_FILE = 126u16,
    SPEC = 164u16,
    ANON_SPEC = 59u16,
    ANON_STAR = 82u16,
    STRING = 123u16,
    ANON_TRY = 94u16,
    TRY_AFTER = 249u16,
    TRY_CLASS = 251u16,
    TRY_EXPR = 247u16,
    TRY_STACK = 252u16,
    TUPLE = 213u16,
    ANON_TYPE = 52u16,
    TYPE_ALIAS = 159u16,
    TYPE_GUARDS = 173u16,
    TYPE_NAME = 162u16,
    TYPE_SIG = 172u16,
    UNARY_OP_EXPR = 189u16,
    ANON_UNDEF = 10u16,
    ANON_UNIT = 83u16,
    VAR = 120u16,
    VAR_ARGS = 272u16,
    ANON_WHEN = 66u16,
    WILD_ATTRIBUTE = 169u16,
    ANON_XOR = 109u16,
    WHITESPACE = 277u16,
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
            | ANON_LT | ANON_LT_DASH | ANON_LT_EQ | ANON_LT_LT | ANON_PIPE | ANON_PIPE_PIPE
            | ANON_PLUS | ANON_PLUS_PLUS | ANON_POUND | ANON_QMARK | ANON_QMARK_EQ
            | ANON_RBRACK | ANON_RPAREN | ANON_RRACE | ANON_SEMI | ANON_SLASH | ANON_SLASH_EQ
            | ANON_STAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_literal(&self) -> bool {
        match self {
            ATOM | CHAR | COMMENT | DEPRECATED_WILDCARD | DOTDOTDOT | FLOAT | INTEGER | STRING
            | VAR => true,
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

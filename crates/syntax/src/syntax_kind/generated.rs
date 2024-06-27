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
    ANON_AFTER = 95u16,
    ANON_AND = 105u16,
    ANON_ANDALSO = 78u16,
    ANN_TYPE = 183u16,
    ANN_VAR = 184u16,
    ANONYMOUS_FUN = 250u16,
    ARITY = 253u16,
    ATOM = 1u16,
    ATTR_NAME = 178u16,
    B_GENERATOR = 220u16,
    ANON_BAND = 104u16,
    ANON_BANG = 76u16,
    ANON_BEGIN = 79u16,
    ANON_BEHAVIOR = 31u16,
    ANON_BEHAVIOUR = 29u16,
    BEHAVIOUR_ATTRIBUTE = 148u16,
    BIN_ELEMENT = 206u16,
    BINARY = 205u16,
    BINARY_COMPREHENSION = 215u16,
    BINARY_OP_EXPR = 197u16,
    BIT_SIZE_EXPR = 207u16,
    BIT_TYPE_LIST = 208u16,
    BIT_TYPE_UNIT = 213u16,
    BLOCK_EXPR = 203u16,
    ANON_BNOT = 100u16,
    ANON_BOR = 106u16,
    ANON_BSL = 108u16,
    ANON_BSR = 109u16,
    ANON_BXOR = 107u16,
    CALL = 239u16,
    CALLBACK = 173u16,
    ANON_CALLBACK = 63u16,
    ANON_CASE = 92u16,
    CASE_EXPR = 242u16,
    ANON_CATCH = 73u16,
    CATCH_CLAUSE = 259u16,
    CATCH_EXPR = 194u16,
    CHAR = 129u16,
    CLAUSE_BODY = 192u16,
    ANON_COLON = 66u16,
    ANON_COLON_COLON = 58u16,
    ANON_COLON_EQ = 91u16,
    ANON_COMMA = 26u16,
    COMMENT = 130u16,
    ANON_COMPILE = 43u16,
    COMPILE_OPTIONS_ATTRIBUTE = 154u16,
    CONCATABLES = 285u16,
    COND_MATCH_EXPR = 196u16,
    CR_CLAUSE = 245u16,
    ANON_DASH = 2u16,
    ANON_DASH_DASH = 113u16,
    ANON_DASH_GT = 67u16,
    ANON_DEFINE = 24u16,
    ANON_DEPRECATED = 47u16,
    DEPRECATED_ATTRIBUTE = 156u16,
    DEPRECATED_FA = 161u16,
    DEPRECATED_FAS = 160u16,
    DEPRECATED_MODULE = 159u16,
    DEPRECATED_WILDCARD = 53u16,
    DEPRECATION_DESC = 162u16,
    ANON_DIV = 102u16,
    ANON_DOT = 7u16,
    ANON_DOT_DOT = 71u16,
    DOTDOTDOT = 72u16,
    ANON_ELIF = 22u16,
    ANON_ELSE = 16u16,
    ANON_END = 80u16,
    ANON_ENDIF = 18u16,
    ANON_EQ = 74u16,
    ANON_EQ_COLON_EQ = 120u16,
    ANON_EQ_EQ = 114u16,
    ANON_EQ_GT = 90u16,
    ANON_EQ_LT = 116u16,
    ANON_EQ_SLASH_EQ = 121u16,
    ANON_EXPORT = 33u16,
    EXPORT_ATTRIBUTE = 149u16,
    ANON_EXPORT_TYPE = 41u16,
    EXPORT_TYPE_ATTRIBUTE = 153u16,
    EXPR_ARGS = 281u16,
    EXTERNAL_FUN = 249u16,
    FA = 152u16,
    ANON_FEATURE = 49u16,
    FEATURE_ATTRIBUTE = 157u16,
    FIELD_EXPR = 237u16,
    FIELD_TYPE = 238u16,
    ANON_FILE = 45u16,
    FILE_ATTRIBUTE = 155u16,
    FLOAT = 124u16,
    ANON_FUN = 70u16,
    FUN_CLAUSE = 255u16,
    FUN_DECL = 179u16,
    FUN_TYPE = 186u16,
    FUN_TYPE_SIG = 187u16,
    FUNCTION_CLAUSE = 190u16,
    GENERATOR = 219u16,
    ANON_GT = 119u16,
    ANON_GT_EQ = 118u16,
    ANON_GT_GT = 82u16,
    GUARD = 283u16,
    GUARD_CLAUSE = 284u16,
    ANON_IF = 20u16,
    IF_CLAUSE = 241u16,
    IF_EXPR = 240u16,
    ANON_IFDEF = 12u16,
    ANON_IFNDEF = 14u16,
    ANON_IMPORT = 37u16,
    IMPORT_ATTRIBUTE = 150u16,
    ANON_INCLUDE = 3u16,
    ANON_INCLUDE_LIB = 8u16,
    INTEGER = 123u16,
    INTERNAL_FUN = 248u16,
    ANON_LBRACE = 51u16,
    ANON_LBRACK = 35u16,
    LC_EXPRS = 217u16,
    LIST = 204u16,
    LIST_COMPREHENSION = 214u16,
    ANON_LPAREN = 5u16,
    ANON_LT = 117u16,
    ANON_LT_DASH = 88u16,
    ANON_LT_EQ = 89u16,
    ANON_LT_LT = 81u16,
    MACRO_CALL_ARGS = 277u16,
    MACRO_CALL_EXPR = 276u16,
    MACRO_EXPR = 280u16,
    MACRO_LHS = 274u16,
    MACRO_STRING = 279u16,
    MAP_COMPREHENSION = 216u16,
    MAP_EXPR = 224u16,
    MAP_EXPR_UPDATE = 223u16,
    MAP_FIELD = 226u16,
    MAP_GENERATOR = 221u16,
    MATCH_EXPR = 195u16,
    ANON_MAYBE = 97u16,
    MAYBE_EXPR = 265u16,
    MODULE = 176u16,
    ANON_MODULE = 27u16,
    MODULE_ATTRIBUTE = 147u16,
    MULTI_STRING = 164u16,
    ANON_NOT = 101u16,
    ANON_OF = 93u16,
    OPAQUE = 168u16,
    ANON_OPAQUE = 56u16,
    ANON_OPTIONAL_CALLBACKS = 39u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 151u16,
    ANON_OR = 110u16,
    ANON_ORELSE = 77u16,
    PAREN_EXPR = 202u16,
    PIPE = 185u16,
    ANON_PIPE = 69u16,
    ANON_PIPE_PIPE = 87u16,
    ANON_PLUS = 99u16,
    ANON_PLUS_PLUS = 112u16,
    ANON_POUND = 86u16,
    PP_DEFINE = 145u16,
    PP_ELIF = 144u16,
    PP_ELSE = 141u16,
    PP_ENDIF = 142u16,
    PP_IF = 143u16,
    PP_IFDEF = 139u16,
    PP_IFNDEF = 140u16,
    PP_INCLUDE = 136u16,
    PP_INCLUDE_LIB = 137u16,
    PP_UNDEF = 138u16,
    ANON_QMARK = 98u16,
    ANON_QMARK_EQ = 75u16,
    RANGE_TYPE = 188u16,
    ANON_RBRACK = 36u16,
    ANON_RECEIVE = 94u16,
    RECEIVE_AFTER = 247u16,
    RECEIVE_EXPR = 246u16,
    ANON_RECORD = 59u16,
    RECORD_DECL = 171u16,
    RECORD_EXPR = 231u16,
    RECORD_FIELD = 236u16,
    RECORD_FIELD_EXPR = 229u16,
    RECORD_FIELD_NAME = 233u16,
    RECORD_INDEX_EXPR = 228u16,
    RECORD_NAME = 232u16,
    RECORD_UPDATE_EXPR = 230u16,
    ANON_REM = 103u16,
    REMOTE = 200u16,
    REMOTE_MODULE = 201u16,
    REPLACEMENT_CR_CLAUSES = 269u16,
    REPLACEMENT_EXPR_GUARD = 272u16,
    REPLACEMENT_FUNCTION_CLAUSES = 268u16,
    REPLACEMENT_GUARD_AND = 271u16,
    REPLACEMENT_GUARD_OR = 270u16,
    REPLACEMENT_PARENS = 273u16,
    ANON_RPAREN = 6u16,
    ANON_RRACE = 52u16,
    ANON_SEMI = 65u16,
    ANON_SLASH = 83u16,
    ANON_SLASH_EQ = 115u16,
    SOURCE_FILE = 133u16,
    SPEC = 172u16,
    ANON_SPEC = 61u16,
    ANON_STAR = 84u16,
    STRING = 292u16,
    ANON_TRY = 96u16,
    TRY_AFTER = 258u16,
    TRY_CLASS = 260u16,
    TRY_EXPR = 256u16,
    TRY_STACK = 261u16,
    TUPLE = 222u16,
    ANON_TYPE = 54u16,
    TYPE_ALIAS = 167u16,
    TYPE_GUARDS = 182u16,
    TYPE_NAME = 170u16,
    TYPE_SIG = 181u16,
    UNARY_OP_EXPR = 198u16,
    ANON_UNDEF = 10u16,
    ANON_UNIT = 85u16,
    VAR = 122u16,
    VAR_ARGS = 282u16,
    ANON_WHEN = 68u16,
    WILD_ATTRIBUTE = 177u16,
    ANON_XOR = 111u16,
    WHITESPACE = 293u16,
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

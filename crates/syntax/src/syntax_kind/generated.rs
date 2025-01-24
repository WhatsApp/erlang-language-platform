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
    ANON_AFTER = 97u16,
    ANON_AND = 107u16,
    ANON_ANDALSO = 80u16,
    ANN_TYPE = 188u16,
    ANN_VAR = 189u16,
    ANONYMOUS_FUN = 255u16,
    ARITY = 258u16,
    ATOM = 1u16,
    ATTR_NAME = 183u16,
    B_GENERATOR = 225u16,
    ANON_BAND = 106u16,
    ANON_BANG = 78u16,
    ANON_BEGIN = 81u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 153u16,
    BIN_ELEMENT = 211u16,
    BINARY = 210u16,
    BINARY_COMPREHENSION = 220u16,
    BINARY_OP_EXPR = 202u16,
    BIT_SIZE_EXPR = 212u16,
    BIT_TYPE_LIST = 213u16,
    BIT_TYPE_UNIT = 218u16,
    BLOCK_EXPR = 208u16,
    ANON_BNOT = 102u16,
    ANON_BOR = 108u16,
    ANON_BSL = 110u16,
    ANON_BSR = 111u16,
    ANON_BXOR = 109u16,
    CALL = 244u16,
    CALLBACK = 178u16,
    ANON_CALLBACK = 67u16,
    ANON_CASE = 94u16,
    CASE_EXPR = 247u16,
    ANON_CATCH = 75u16,
    CATCH_CLAUSE = 264u16,
    CATCH_EXPR = 199u16,
    CHAR = 130u16,
    CLAUSE_BODY = 197u16,
    ANON_COLON = 3u16,
    ANON_COLON_COLON = 62u16,
    ANON_COLON_EQ = 93u16,
    ANON_COMMA = 30u16,
    COMMENT = 131u16,
    ANON_COMPILE = 47u16,
    COMPILE_OPTIONS_ATTRIBUTE = 159u16,
    CONCATABLES = 290u16,
    COND_MATCH_EXPR = 201u16,
    CR_CLAUSE = 250u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 115u16,
    ANON_DASH_GT = 70u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 51u16,
    DEPRECATED_ATTRIBUTE = 161u16,
    DEPRECATED_FA = 166u16,
    DEPRECATED_FAS = 165u16,
    DEPRECATED_MODULE = 164u16,
    DEPRECATED_WILDCARD = 57u16,
    DEPRECATION_DESC = 167u16,
    ANON_DIV = 104u16,
    ANON_DOT = 4u16,
    ANON_DOT_DOT = 73u16,
    DOTDOTDOT = 74u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 82u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 76u16,
    ANON_EQ_COLON_EQ = 122u16,
    ANON_EQ_EQ = 116u16,
    ANON_EQ_GT = 92u16,
    ANON_EQ_LT = 118u16,
    ANON_EQ_SLASH_EQ = 123u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 154u16,
    ANON_EXPORT_TYPE = 45u16,
    EXPORT_TYPE_ATTRIBUTE = 158u16,
    EXPR_ARGS = 286u16,
    EXTERNAL_FUN = 254u16,
    FA = 157u16,
    ANON_FEATURE = 53u16,
    FEATURE_ATTRIBUTE = 162u16,
    FIELD_EXPR = 242u16,
    FIELD_TYPE = 243u16,
    ANON_FILE = 49u16,
    FILE_ATTRIBUTE = 160u16,
    FLOAT = 126u16,
    ANON_FUN = 72u16,
    FUN_CLAUSE = 260u16,
    FUN_DECL = 184u16,
    FUN_TYPE = 191u16,
    FUN_TYPE_SIG = 192u16,
    FUNCTION_CLAUSE = 195u16,
    GENERATOR = 224u16,
    ANON_GT = 121u16,
    ANON_GT_EQ = 120u16,
    ANON_GT_GT = 84u16,
    GUARD = 288u16,
    GUARD_CLAUSE = 289u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 246u16,
    IF_EXPR = 245u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 155u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 125u16,
    INTERNAL_FUN = 253u16,
    ANON_LBRACE = 55u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 222u16,
    LIST = 209u16,
    LIST_COMPREHENSION = 219u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 119u16,
    ANON_LT_DASH = 90u16,
    ANON_LT_EQ = 91u16,
    ANON_LT_LT = 83u16,
    MACRO_CALL_ARGS = 282u16,
    MACRO_CALL_EXPR = 281u16,
    MACRO_EXPR = 285u16,
    MACRO_LHS = 279u16,
    MACRO_STRING = 284u16,
    MAP_COMPREHENSION = 221u16,
    MAP_EXPR = 229u16,
    MAP_EXPR_UPDATE = 228u16,
    MAP_FIELD = 231u16,
    MAP_GENERATOR = 226u16,
    MATCH_EXPR = 200u16,
    ANON_MAYBE = 99u16,
    MAYBE_EXPR = 270u16,
    MODULE = 181u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 152u16,
    MULTI_STRING = 169u16,
    ANON_NOT = 103u16,
    ANON_OF = 95u16,
    OPAQUE = 173u16,
    ANON_OPAQUE = 60u16,
    ANON_OPTIONAL_CALLBACKS = 43u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 156u16,
    ANON_OR = 112u16,
    ANON_ORELSE = 79u16,
    PAREN_EXPR = 207u16,
    PIPE = 190u16,
    ANON_PIPE = 71u16,
    ANON_PIPE_PIPE = 89u16,
    ANON_PLUS = 101u16,
    ANON_PLUS_PLUS = 114u16,
    ANON_POUND = 88u16,
    PP_DEFINE = 150u16,
    PP_ELIF = 149u16,
    PP_ELSE = 146u16,
    PP_ENDIF = 147u16,
    PP_IF = 148u16,
    PP_IFDEF = 144u16,
    PP_IFNDEF = 145u16,
    PP_INCLUDE = 141u16,
    PP_INCLUDE_LIB = 142u16,
    PP_UNDEF = 143u16,
    ANON_QMARK = 100u16,
    ANON_QMARK_EQ = 77u16,
    RANGE_TYPE = 193u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 96u16,
    RECEIVE_AFTER = 252u16,
    RECEIVE_EXPR = 251u16,
    ANON_RECORD = 63u16,
    RECORD_DECL = 176u16,
    RECORD_EXPR = 236u16,
    RECORD_FIELD = 241u16,
    RECORD_FIELD_EXPR = 234u16,
    RECORD_FIELD_NAME = 238u16,
    RECORD_INDEX_EXPR = 233u16,
    RECORD_NAME = 237u16,
    RECORD_UPDATE_EXPR = 235u16,
    ANON_REM = 105u16,
    REMOTE = 205u16,
    REMOTE_MODULE = 206u16,
    REPLACEMENT_CR_CLAUSES = 274u16,
    REPLACEMENT_EXPR_GUARD = 277u16,
    REPLACEMENT_FUNCTION_CLAUSES = 273u16,
    REPLACEMENT_GUARD_AND = 276u16,
    REPLACEMENT_GUARD_OR = 275u16,
    REPLACEMENT_PARENS = 278u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 56u16,
    ANON_SEMI = 69u16,
    ANON_SLASH = 85u16,
    ANON_SLASH_EQ = 117u16,
    SOURCE_FILE = 135u16,
    SPEC = 177u16,
    ANON_SPEC = 65u16,
    ANON_SSR = 2u16,
    SSR_DEFINITION = 137u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 138u16,
    SSR_WHEN = 139u16,
    ANON_STAR = 86u16,
    STRING = 297u16,
    ANON_TRY = 98u16,
    TRY_AFTER = 263u16,
    TRY_CLASS = 265u16,
    TRY_EXPR = 261u16,
    TRY_STACK = 266u16,
    TUPLE = 227u16,
    ANON_TYPE = 58u16,
    TYPE_ALIAS = 172u16,
    TYPE_GUARDS = 187u16,
    TYPE_NAME = 175u16,
    TYPE_SIG = 186u16,
    UNARY_OP_EXPR = 203u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 87u16,
    VAR = 124u16,
    VAR_ARGS = 287u16,
    ANON_WHEN = 6u16,
    WILD_ATTRIBUTE = 182u16,
    ANON_XOR = 113u16,
    WHITESPACE = 298u16,
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
            | ANON_LT | ANON_LT_DASH | ANON_LT_EQ | ANON_LT_LT | ANON_PIPE | ANON_PIPE_PIPE
            | ANON_PLUS | ANON_PLUS_PLUS | ANON_POUND | ANON_QMARK | ANON_QMARK_EQ
            | ANON_RBRACK | ANON_RPAREN | ANON_RRACE | ANON_SEMI | ANON_SLASH | ANON_SLASH_EQ
            | ANON_SSR_MATCH | ANON_STAR => true,
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

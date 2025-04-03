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
    ANON_AFTER = 100u16,
    ANON_AND = 110u16,
    ANON_ANDALSO = 80u16,
    ANN_TYPE = 192u16,
    ANN_VAR = 193u16,
    ANONYMOUS_FUN = 262u16,
    ARITY = 265u16,
    ATOM = 1u16,
    ATTR_NAME = 187u16,
    B_GENERATOR = 230u16,
    ANON_BAND = 109u16,
    ANON_BANG = 78u16,
    ANON_BEGIN = 81u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 157u16,
    BIN_ELEMENT = 215u16,
    BINARY = 214u16,
    BINARY_COMPREHENSION = 224u16,
    BINARY_OP_EXPR = 206u16,
    BIT_SIZE_EXPR = 216u16,
    BIT_TYPE_LIST = 217u16,
    BIT_TYPE_UNIT = 222u16,
    BLOCK_EXPR = 212u16,
    ANON_BNOT = 105u16,
    ANON_BOR = 111u16,
    ANON_BSL = 113u16,
    ANON_BSR = 114u16,
    ANON_BXOR = 112u16,
    CALL = 251u16,
    CALLBACK = 182u16,
    ANON_CALLBACK = 67u16,
    ANON_CASE = 97u16,
    CASE_EXPR = 254u16,
    ANON_CATCH = 75u16,
    CATCH_CLAUSE = 271u16,
    CATCH_EXPR = 203u16,
    CHAR = 134u16,
    CLAUSE_BODY = 201u16,
    ANON_COLON = 4u16,
    ANON_COLON_COLON = 62u16,
    ANON_COLON_EQ = 96u16,
    ANON_COMMA = 30u16,
    COMMENT = 135u16,
    ANON_COMPILE = 47u16,
    COMPILE_OPTIONS_ATTRIBUTE = 163u16,
    CONCATABLES = 297u16,
    COND_MATCH_EXPR = 205u16,
    CR_CLAUSE = 257u16,
    ANON_D_AMP = 90u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 118u16,
    ANON_DASH_GT = 70u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 51u16,
    DEPRECATED_ATTRIBUTE = 165u16,
    DEPRECATED_FA = 170u16,
    DEPRECATED_FAS = 169u16,
    DEPRECATED_MODULE = 168u16,
    DEPRECATED_WILDCARD = 57u16,
    DEPRECATION_DESC = 171u16,
    ANON_DIV = 107u16,
    ANON_DOT = 2u16,
    ANON_DOT_DOT = 73u16,
    DOTDOTDOT = 74u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 82u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 76u16,
    ANON_EQ_COLON_EQ = 125u16,
    ANON_EQ_EQ = 119u16,
    ANON_EQ_GT = 95u16,
    ANON_EQ_LT = 121u16,
    ANON_EQ_SLASH_EQ = 126u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 158u16,
    ANON_EXPORT_TYPE = 45u16,
    EXPORT_TYPE_ATTRIBUTE = 162u16,
    EXPR_ARGS = 293u16,
    EXTERNAL_FUN = 261u16,
    FA = 161u16,
    ANON_FEATURE = 53u16,
    FEATURE_ATTRIBUTE = 166u16,
    FIELD_EXPR = 249u16,
    FIELD_TYPE = 250u16,
    ANON_FILE = 49u16,
    FILE_ATTRIBUTE = 164u16,
    FLOAT = 130u16,
    ANON_FUN = 72u16,
    FUN_CLAUSE = 267u16,
    FUN_DECL = 188u16,
    FUN_TYPE = 195u16,
    FUN_TYPE_SIG = 196u16,
    FUNCTION_CLAUSE = 199u16,
    GENERATOR = 229u16,
    ANON_GT = 124u16,
    ANON_GT_EQ = 123u16,
    ANON_GT_GT = 84u16,
    GUARD = 295u16,
    GUARD_CLAUSE = 296u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 253u16,
    IF_EXPR = 252u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 159u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 129u16,
    INTERNAL_FUN = 260u16,
    ANON_LBRACE = 55u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 226u16,
    LC_OR_ZC_EXPR = 227u16,
    LIST = 213u16,
    LIST_COMPREHENSION = 223u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 122u16,
    ANON_LT_COLON_DASH = 92u16,
    ANON_LT_COLON_EQ = 94u16,
    ANON_LT_DASH = 91u16,
    ANON_LT_EQ = 93u16,
    ANON_LT_LT = 83u16,
    MACRO_CALL_ARGS = 289u16,
    MACRO_CALL_EXPR = 288u16,
    MACRO_EXPR = 292u16,
    MACRO_LHS = 286u16,
    MACRO_STRING = 291u16,
    MAP_COMPREHENSION = 225u16,
    MAP_EXPR = 236u16,
    MAP_EXPR_UPDATE = 235u16,
    MAP_FIELD = 238u16,
    MAP_GENERATOR = 231u16,
    MATCH_EXPR = 204u16,
    ANON_MAYBE = 102u16,
    MAYBE_EXPR = 277u16,
    MODULE = 185u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 156u16,
    MULTI_STRING = 173u16,
    ANON_NOT = 106u16,
    ANON_OF = 98u16,
    OPAQUE = 177u16,
    ANON_OPAQUE = 60u16,
    ANON_OPTIONAL_CALLBACKS = 43u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 160u16,
    ANON_OR = 115u16,
    ANON_ORELSE = 79u16,
    PAREN_EXPR = 211u16,
    PIPE = 194u16,
    ANON_PIPE = 71u16,
    ANON_PIPE_PIPE = 89u16,
    ANON_PLUS = 104u16,
    ANON_PLUS_PLUS = 117u16,
    ANON_POUND = 88u16,
    PP_DEFINE = 154u16,
    PP_ELIF = 153u16,
    PP_ELSE = 150u16,
    PP_ENDIF = 151u16,
    PP_IF = 152u16,
    PP_IFDEF = 148u16,
    PP_IFNDEF = 149u16,
    PP_INCLUDE = 145u16,
    PP_INCLUDE_LIB = 146u16,
    PP_UNDEF = 147u16,
    ANON_QMARK = 103u16,
    ANON_QMARK_EQ = 77u16,
    RANGE_TYPE = 197u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 99u16,
    RECEIVE_AFTER = 259u16,
    RECEIVE_EXPR = 258u16,
    ANON_RECORD = 63u16,
    RECORD_DECL = 180u16,
    RECORD_EXPR = 243u16,
    RECORD_FIELD = 248u16,
    RECORD_FIELD_EXPR = 241u16,
    RECORD_FIELD_NAME = 245u16,
    RECORD_INDEX_EXPR = 240u16,
    RECORD_NAME = 244u16,
    RECORD_UPDATE_EXPR = 242u16,
    ANON_REM = 108u16,
    REMOTE = 209u16,
    REMOTE_MODULE = 210u16,
    REPLACEMENT_CR_CLAUSES = 281u16,
    REPLACEMENT_EXPR_GUARD = 284u16,
    REPLACEMENT_FUNCTION_CLAUSES = 280u16,
    REPLACEMENT_GUARD_AND = 283u16,
    REPLACEMENT_GUARD_OR = 282u16,
    REPLACEMENT_PARENS = 285u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 56u16,
    ANON_SEMI = 69u16,
    SHEBANG = 127u16,
    ANON_SLASH = 85u16,
    ANON_SLASH_EQ = 120u16,
    SOURCE_FILE = 139u16,
    SPEC = 181u16,
    ANON_SPEC = 65u16,
    ANON_SSR = 3u16,
    SSR_DEFINITION = 141u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 142u16,
    SSR_WHEN = 143u16,
    ANON_STAR = 86u16,
    STRING = 304u16,
    ANON_TRY = 101u16,
    TRY_AFTER = 270u16,
    TRY_CLASS = 272u16,
    TRY_EXPR = 268u16,
    TRY_STACK = 273u16,
    TUPLE = 234u16,
    ANON_TYPE = 58u16,
    TYPE_ALIAS = 176u16,
    TYPE_GUARDS = 191u16,
    TYPE_NAME = 179u16,
    TYPE_SIG = 190u16,
    UNARY_OP_EXPR = 207u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 87u16,
    VAR = 128u16,
    VAR_ARGS = 294u16,
    ANON_WHEN = 6u16,
    WILD_ATTRIBUTE = 186u16,
    ANON_XOR = 116u16,
    WHITESPACE = 305u16,
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
            ANON_BANG | ANON_COLON | ANON_COLON_COLON | ANON_COLON_EQ | ANON_COMMA | ANON_D_AMP
            | ANON_DASH | ANON_DASH_DASH | ANON_DASH_GT | ANON_DOT | ANON_DOT_DOT | ANON_EQ
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
            ATOM | CHAR | COMMENT | DEPRECATED_WILDCARD | DOTDOTDOT | FLOAT | INTEGER | SHEBANG
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

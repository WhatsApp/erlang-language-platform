//! @generated file, do not edit by hand, see `xtask/src/codegen.rs`

#![allow(bad_style, missing_docs, unreachable_pub)]
use num_derive::FromPrimitive;
use num_derive::ToPrimitive;
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
    ANON_AFTER = 108u16,
    ANON_AND = 118u16,
    ANON_ANDALSO = 88u16,
    ANN_TYPE = 204u16,
    ANN_VAR = 205u16,
    ANON_RECORD_EXPR = 262u16,
    ANON_RECORD_FIELD_EXPR = 264u16,
    ANON_RECORD_UPDATE_EXPR = 263u16,
    ANONYMOUS_FUN = 281u16,
    ARITY = 284u16,
    ATOM = 1u16,
    ATTR_NAME = 199u16,
    B_GENERATOR = 242u16,
    ANON_BAND = 117u16,
    ANON_BANG = 86u16,
    ANON_BEGIN = 89u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 165u16,
    BIN_ELEMENT = 227u16,
    BINARY = 226u16,
    BINARY_COMPREHENSION = 236u16,
    BINARY_OP_EXPR = 218u16,
    BIT_SIZE_EXPR = 228u16,
    BIT_TYPE_LIST = 229u16,
    BIT_TYPE_UNIT = 234u16,
    BLOCK_EXPR = 224u16,
    ANON_BNOT = 113u16,
    ANON_BOR = 119u16,
    ANON_BSL = 121u16,
    ANON_BSR = 122u16,
    ANON_BXOR = 120u16,
    CALL = 270u16,
    CALLBACK = 194u16,
    ANON_CALLBACK = 74u16,
    ANON_CASE = 105u16,
    CASE_EXPR = 273u16,
    ANON_CATCH = 83u16,
    CATCH_CLAUSE = 290u16,
    CATCH_EXPR = 215u16,
    CHAR = 142u16,
    CLAUSE_BODY = 213u16,
    ANON_COLON = 4u16,
    ANON_COLON_COLON = 68u16,
    ANON_COLON_EQ = 103u16,
    ANON_COLON_GT = 78u16,
    ANON_COMMA = 30u16,
    COMMENT = 143u16,
    ANON_COMPILE = 51u16,
    COMPILE_OPTIONS_ATTRIBUTE = 174u16,
    CONCATABLES = 316u16,
    COND_MATCH_EXPR = 217u16,
    CR_CLAUSE = 276u16,
    ANON_D_AMP = 97u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 126u16,
    ANON_DASH_GT = 77u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 55u16,
    DEPRECATED_ATTRIBUTE = 176u16,
    DEPRECATED_FA = 181u16,
    DEPRECATED_FAS = 180u16,
    DEPRECATED_MODULE = 179u16,
    DEPRECATED_WILDCARD = 61u16,
    DEPRECATION_DESC = 182u16,
    ANON_DIV = 115u16,
    ANON_DOT = 2u16,
    ANON_DOT_DOT = 81u16,
    DOTDOTDOT = 82u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 90u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 84u16,
    ANON_EQ_COLON_EQ = 133u16,
    ANON_EQ_EQ = 127u16,
    ANON_EQ_GT = 102u16,
    ANON_EQ_LT = 129u16,
    ANON_EQ_SLASH_EQ = 134u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 166u16,
    ANON_EXPORT_RECORD = 49u16,
    EXPORT_RECORD_ATTRIBUTE = 173u16,
    ANON_EXPORT_TYPE = 47u16,
    EXPORT_TYPE_ATTRIBUTE = 172u16,
    EXPR_ARGS = 312u16,
    EXTERNAL_FUN = 280u16,
    FA = 171u16,
    ANON_FEATURE = 57u16,
    FEATURE_ATTRIBUTE = 177u16,
    FIELD_EXPR = 268u16,
    FIELD_TYPE = 269u16,
    ANON_FILE = 53u16,
    FILE_ATTRIBUTE = 175u16,
    FLOAT = 138u16,
    ANON_FUN = 80u16,
    FUN_CLAUSE = 286u16,
    FUN_DECL = 200u16,
    FUN_TYPE = 207u16,
    FUN_TYPE_SIG = 208u16,
    FUNCTION_CLAUSE = 211u16,
    GENERATOR = 241u16,
    ANON_GT = 132u16,
    ANON_GT_EQ = 131u16,
    ANON_GT_GT = 92u16,
    GUARD = 314u16,
    GUARD_CLAUSE = 315u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 272u16,
    IF_EXPR = 271u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 167u16,
    ANON_IMPORT_RECORD = 43u16,
    IMPORT_RECORD_ATTRIBUTE = 168u16,
    IMPORT_RECORD_NAMES = 169u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 137u16,
    INTERNAL_FUN = 279u16,
    ANON_LBRACE = 59u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 238u16,
    LC_OR_ZC_EXPR = 239u16,
    LIST = 225u16,
    LIST_COMPREHENSION = 235u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 130u16,
    ANON_LT_COLON_DASH = 99u16,
    ANON_LT_COLON_EQ = 101u16,
    ANON_LT_DASH = 98u16,
    ANON_LT_EQ = 100u16,
    ANON_LT_LT = 91u16,
    MACRO_CALL_ARGS = 308u16,
    MACRO_CALL_EXPR = 307u16,
    MACRO_EXPR = 311u16,
    MACRO_LHS = 305u16,
    MACRO_STRING = 310u16,
    MAP_COMPREHENSION = 237u16,
    MAP_EXPR = 248u16,
    MAP_EXPR_UPDATE = 247u16,
    MAP_FIELD = 250u16,
    MAP_GENERATOR = 243u16,
    MATCH_EXPR = 216u16,
    ANON_MAYBE = 110u16,
    MAYBE_EXPR = 296u16,
    MODULE = 197u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 164u16,
    MULTI_STRING = 184u16,
    NOMINAL = 188u16,
    ANON_NOMINAL = 64u16,
    ANON_NOT = 114u16,
    ANON_OF = 106u16,
    OPAQUE = 189u16,
    ANON_OPAQUE = 66u16,
    ANON_OPTIONAL_CALLBACKS = 45u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 170u16,
    ANON_OR = 123u16,
    ANON_ORELSE = 87u16,
    PAREN_EXPR = 223u16,
    PIPE = 206u16,
    ANON_PIPE = 79u16,
    ANON_PIPE_PIPE = 96u16,
    ANON_PLUS = 112u16,
    ANON_PLUS_PLUS = 125u16,
    ANON_POUND = 71u16,
    ANON_POUND_UNDERSCORE = 104u16,
    PP_DEFINE = 162u16,
    PP_ELIF = 161u16,
    PP_ELSE = 158u16,
    PP_ENDIF = 159u16,
    PP_IF = 160u16,
    PP_IFDEF = 156u16,
    PP_IFNDEF = 157u16,
    PP_INCLUDE = 153u16,
    PP_INCLUDE_LIB = 154u16,
    PP_UNDEF = 155u16,
    ANON_QMARK = 111u16,
    ANON_QMARK_EQ = 85u16,
    QUALIFIED_RECORD_EXPR = 259u16,
    QUALIFIED_RECORD_FIELD_EXPR = 261u16,
    QUALIFIED_RECORD_NAME = 258u16,
    QUALIFIED_RECORD_UPDATE_EXPR = 260u16,
    RANGE_TYPE = 209u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 107u16,
    RECEIVE_AFTER = 278u16,
    RECEIVE_EXPR = 277u16,
    ANON_RECORD = 69u16,
    RECORD_DECL = 192u16,
    RECORD_EXPR = 255u16,
    RECORD_FIELD = 267u16,
    RECORD_FIELD_EXPR = 253u16,
    RECORD_FIELD_NAME = 257u16,
    RECORD_INDEX_EXPR = 252u16,
    RECORD_NAME = 256u16,
    RECORD_UPDATE_EXPR = 254u16,
    ANON_REM = 116u16,
    REMOTE = 221u16,
    REMOTE_MODULE = 222u16,
    REPLACEMENT_CR_CLAUSES = 300u16,
    REPLACEMENT_EXPR_GUARD = 303u16,
    REPLACEMENT_FUNCTION_CLAUSES = 299u16,
    REPLACEMENT_GUARD_AND = 302u16,
    REPLACEMENT_GUARD_OR = 301u16,
    REPLACEMENT_PARENS = 304u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 60u16,
    ANON_SEMI = 76u16,
    SHEBANG = 135u16,
    ANON_SLASH = 93u16,
    ANON_SLASH_EQ = 128u16,
    SOURCE_FILE = 147u16,
    SPEC = 193u16,
    ANON_SPEC = 72u16,
    ANON_SSR = 3u16,
    SSR_DEFINITION = 149u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 150u16,
    SSR_WHEN = 151u16,
    ANON_STAR = 94u16,
    STRING = 323u16,
    ANON_TRY = 109u16,
    TRY_AFTER = 289u16,
    TRY_CLASS = 291u16,
    TRY_EXPR = 287u16,
    TRY_STACK = 292u16,
    TUPLE = 246u16,
    ANON_TYPE = 62u16,
    TYPE_ALIAS = 187u16,
    TYPE_GUARDS = 203u16,
    TYPE_NAME = 191u16,
    TYPE_SIG = 202u16,
    UNARY_OP_EXPR = 219u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 95u16,
    VAR = 136u16,
    VAR_ARGS = 313u16,
    ANON_WHEN = 6u16,
    WILD_ATTRIBUTE = 198u16,
    ANON_XOR = 124u16,
    WHITESPACE = 324u16,
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
            | ANON_EXPORT_RECORD
            | ANON_EXPORT_TYPE
            | ANON_FEATURE
            | ANON_FILE
            | ANON_FUN
            | ANON_IF
            | ANON_IFDEF
            | ANON_IFNDEF
            | ANON_IMPORT
            | ANON_IMPORT_RECORD
            | ANON_INCLUDE
            | ANON_INCLUDE_LIB
            | ANON_MAYBE
            | ANON_MODULE
            | ANON_NOMINAL
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
            ANON_BANG
            | ANON_COLON
            | ANON_COLON_COLON
            | ANON_COLON_EQ
            | ANON_COLON_GT
            | ANON_COMMA
            | ANON_D_AMP
            | ANON_DASH
            | ANON_DASH_DASH
            | ANON_DASH_GT
            | ANON_DOT
            | ANON_DOT_DOT
            | ANON_EQ
            | ANON_EQ_COLON_EQ
            | ANON_EQ_EQ
            | ANON_EQ_GT
            | ANON_EQ_LT
            | ANON_EQ_SLASH_EQ
            | ANON_GT
            | ANON_GT_EQ
            | ANON_GT_GT
            | ANON_LBRACE
            | ANON_LBRACK
            | ANON_LPAREN
            | ANON_LT
            | ANON_LT_COLON_DASH
            | ANON_LT_COLON_EQ
            | ANON_LT_DASH
            | ANON_LT_EQ
            | ANON_LT_LT
            | ANON_PIPE
            | ANON_PIPE_PIPE
            | ANON_PLUS
            | ANON_PLUS_PLUS
            | ANON_POUND
            | ANON_POUND_UNDERSCORE
            | ANON_QMARK
            | ANON_QMARK_EQ
            | ANON_RBRACK
            | ANON_RPAREN
            | ANON_RRACE
            | ANON_SEMI
            | ANON_SLASH
            | ANON_SLASH_EQ
            | ANON_SSR_MATCH
            | ANON_STAR => true,
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

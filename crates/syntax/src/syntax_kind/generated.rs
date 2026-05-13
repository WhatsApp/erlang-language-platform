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
    ANON_AFTER = 109u16,
    ANON_AND = 119u16,
    ANON_ANDALSO = 89u16,
    ANN_TYPE = 205u16,
    ANN_VAR = 206u16,
    ANON_RECORD_EXPR = 263u16,
    ANON_RECORD_FIELD_EXPR = 265u16,
    ANON_RECORD_UPDATE_EXPR = 264u16,
    ANONYMOUS_FUN = 282u16,
    ARITY = 285u16,
    ATOM = 1u16,
    ATTR_NAME = 200u16,
    B_GENERATOR = 243u16,
    ANON_BAND = 118u16,
    ANON_BANG = 87u16,
    ANON_BEGIN = 90u16,
    ANON_BEHAVIOR = 35u16,
    ANON_BEHAVIOUR = 33u16,
    BEHAVIOUR_ATTRIBUTE = 166u16,
    BIN_ELEMENT = 228u16,
    BINARY = 227u16,
    BINARY_COMPREHENSION = 237u16,
    BINARY_OP_EXPR = 219u16,
    BIT_SIZE_EXPR = 229u16,
    BIT_TYPE_LIST = 230u16,
    BIT_TYPE_UNIT = 235u16,
    BLOCK_EXPR = 225u16,
    ANON_BNOT = 114u16,
    ANON_BOR = 120u16,
    ANON_BSL = 122u16,
    ANON_BSR = 123u16,
    ANON_BXOR = 121u16,
    CALL = 271u16,
    CALLBACK = 195u16,
    ANON_CALLBACK = 74u16,
    ANON_CASE = 106u16,
    CASE_EXPR = 274u16,
    ANON_CATCH = 84u16,
    CATCH_CLAUSE = 291u16,
    CATCH_EXPR = 216u16,
    CHAR = 143u16,
    CLAUSE_BODY = 214u16,
    ANON_COLON = 4u16,
    ANON_COLON_COLON = 68u16,
    ANON_COLON_EQ = 104u16,
    ANON_COLON_GT = 79u16,
    ANON_COMMA = 30u16,
    COMMENT = 144u16,
    ANON_COMPILE = 51u16,
    COMPILE_OPTIONS_ATTRIBUTE = 175u16,
    CONCATABLES = 317u16,
    COND_MATCH_EXPR = 218u16,
    CR_CLAUSE = 277u16,
    ANON_D_AMP = 98u16,
    ANON_DASH = 7u16,
    ANON_DASH_DASH = 127u16,
    ANON_DASH_GT = 77u16,
    ANON_DEFINE = 28u16,
    ANON_DEPRECATED = 55u16,
    DEPRECATED_ATTRIBUTE = 177u16,
    DEPRECATED_FA = 182u16,
    DEPRECATED_FAS = 181u16,
    DEPRECATED_MODULE = 180u16,
    DEPRECATED_WILDCARD = 61u16,
    DEPRECATION_DESC = 183u16,
    ANON_DIV = 116u16,
    ANON_DOT = 2u16,
    ANON_DOT_DOT = 82u16,
    DOTDOTDOT = 83u16,
    ANON_ELIF = 26u16,
    ANON_ELSE = 20u16,
    ANON_END = 91u16,
    ANON_ENDIF = 22u16,
    ANON_EQ = 85u16,
    ANON_EQ_COLON_EQ = 134u16,
    ANON_EQ_EQ = 128u16,
    ANON_EQ_GT = 103u16,
    ANON_EQ_LT = 130u16,
    ANON_EQ_SLASH_EQ = 135u16,
    ANON_EXPORT = 37u16,
    EXPORT_ATTRIBUTE = 167u16,
    ANON_EXPORT_RECORD = 49u16,
    EXPORT_RECORD_ATTRIBUTE = 174u16,
    ANON_EXPORT_TYPE = 47u16,
    EXPORT_TYPE_ATTRIBUTE = 173u16,
    EXPR_ARGS = 313u16,
    EXTERNAL_FUN = 281u16,
    FA = 172u16,
    ANON_FEATURE = 57u16,
    FEATURE_ATTRIBUTE = 178u16,
    FIELD_EXPR = 269u16,
    FIELD_TYPE = 270u16,
    ANON_FILE = 53u16,
    FILE_ATTRIBUTE = 176u16,
    FLOAT = 139u16,
    ANON_FUN = 81u16,
    FUN_CLAUSE = 287u16,
    FUN_DECL = 201u16,
    FUN_TYPE = 208u16,
    FUN_TYPE_SIG = 209u16,
    FUNCTION_CLAUSE = 212u16,
    GENERATOR = 242u16,
    ANON_GT = 133u16,
    ANON_GT_EQ = 132u16,
    ANON_GT_GT = 93u16,
    GUARD = 315u16,
    GUARD_CLAUSE = 316u16,
    ANON_IF = 24u16,
    IF_CLAUSE = 273u16,
    IF_EXPR = 272u16,
    ANON_IFDEF = 16u16,
    ANON_IFNDEF = 18u16,
    ANON_IMPORT = 41u16,
    IMPORT_ATTRIBUTE = 168u16,
    ANON_IMPORT_RECORD = 43u16,
    IMPORT_RECORD_ATTRIBUTE = 169u16,
    IMPORT_RECORD_NAMES = 170u16,
    ANON_INCLUDE = 8u16,
    ANON_INCLUDE_LIB = 12u16,
    INTEGER = 138u16,
    INTERNAL_FUN = 280u16,
    ANON_LBRACE = 59u16,
    ANON_LBRACK = 39u16,
    LC_EXPRS = 239u16,
    LC_OR_ZC_EXPR = 240u16,
    LIST = 226u16,
    LIST_COMPREHENSION = 236u16,
    ANON_LPAREN = 10u16,
    ANON_LT = 131u16,
    ANON_LT_COLON_DASH = 100u16,
    ANON_LT_COLON_EQ = 102u16,
    ANON_LT_DASH = 99u16,
    ANON_LT_EQ = 101u16,
    ANON_LT_LT = 92u16,
    MACRO_CALL_ARGS = 309u16,
    MACRO_CALL_EXPR = 308u16,
    MACRO_EXPR = 312u16,
    MACRO_LHS = 306u16,
    MACRO_STRING = 311u16,
    MAP_COMPREHENSION = 238u16,
    MAP_EXPR = 249u16,
    MAP_EXPR_UPDATE = 248u16,
    MAP_FIELD = 251u16,
    MAP_GENERATOR = 244u16,
    MATCH_EXPR = 217u16,
    ANON_MAYBE = 111u16,
    MAYBE_EXPR = 297u16,
    MODULE = 198u16,
    ANON_MODULE = 31u16,
    MODULE_ATTRIBUTE = 165u16,
    MULTI_STRING = 185u16,
    NOMINAL = 189u16,
    ANON_NOMINAL = 64u16,
    ANON_NOT = 115u16,
    ANON_OF = 107u16,
    OPAQUE = 190u16,
    ANON_OPAQUE = 66u16,
    ANON_OPTIONAL_CALLBACKS = 45u16,
    OPTIONAL_CALLBACKS_ATTRIBUTE = 171u16,
    ANON_OR = 124u16,
    ANON_ORELSE = 88u16,
    PAREN_EXPR = 224u16,
    PIPE = 207u16,
    ANON_PIPE = 80u16,
    ANON_PIPE_PIPE = 97u16,
    ANON_PLUS = 113u16,
    ANON_PLUS_PLUS = 126u16,
    ANON_POUND = 71u16,
    ANON_POUND_UNDERSCORE = 105u16,
    PP_DEFINE = 163u16,
    PP_ELIF = 162u16,
    PP_ELSE = 159u16,
    PP_ENDIF = 160u16,
    PP_IF = 161u16,
    PP_IFDEF = 157u16,
    PP_IFNDEF = 158u16,
    PP_INCLUDE = 154u16,
    PP_INCLUDE_LIB = 155u16,
    PP_UNDEF = 156u16,
    ANON_QMARK = 112u16,
    ANON_QMARK_EQ = 86u16,
    QUALIFIED_RECORD_EXPR = 260u16,
    QUALIFIED_RECORD_FIELD_EXPR = 262u16,
    QUALIFIED_RECORD_NAME = 259u16,
    QUALIFIED_RECORD_UPDATE_EXPR = 261u16,
    RANGE_TYPE = 210u16,
    ANON_RBRACK = 40u16,
    ANON_RECEIVE = 108u16,
    RECEIVE_AFTER = 279u16,
    RECEIVE_EXPR = 278u16,
    ANON_RECORD = 69u16,
    RECORD_DECL = 193u16,
    RECORD_EXPR = 256u16,
    RECORD_FIELD = 268u16,
    RECORD_FIELD_EXPR = 254u16,
    RECORD_FIELD_NAME = 258u16,
    RECORD_INDEX_EXPR = 253u16,
    RECORD_NAME = 257u16,
    RECORD_UPDATE_EXPR = 255u16,
    ANON_REM = 117u16,
    REMOTE = 222u16,
    REMOTE_MODULE = 223u16,
    REPLACEMENT_CR_CLAUSES = 301u16,
    REPLACEMENT_EXPR_GUARD = 304u16,
    REPLACEMENT_FUNCTION_CLAUSES = 300u16,
    REPLACEMENT_GUARD_AND = 303u16,
    REPLACEMENT_GUARD_OR = 302u16,
    REPLACEMENT_PARENS = 305u16,
    ANON_RPAREN = 11u16,
    ANON_RRACE = 60u16,
    ANON_SEMI = 76u16,
    SHEBANG = 136u16,
    ANON_SLASH = 94u16,
    ANON_SLASH_EQ = 129u16,
    SOURCE_FILE = 148u16,
    SPEC = 194u16,
    ANON_SPEC = 72u16,
    ANON_SSR = 3u16,
    SSR_DEFINITION = 150u16,
    ANON_SSR_MATCH = 5u16,
    SSR_REPLACEMENT = 151u16,
    SSR_WHERE = 152u16,
    ANON_STAR = 95u16,
    STRING = 324u16,
    ANON_TRY = 110u16,
    TRY_AFTER = 290u16,
    TRY_CLASS = 292u16,
    TRY_EXPR = 288u16,
    TRY_STACK = 293u16,
    TUPLE = 247u16,
    ANON_TYPE = 62u16,
    TYPE_ALIAS = 188u16,
    TYPE_GUARDS = 204u16,
    TYPE_NAME = 192u16,
    TYPE_SIG = 203u16,
    UNARY_OP_EXPR = 220u16,
    ANON_UNDEF = 14u16,
    ANON_UNIT = 96u16,
    VAR = 137u16,
    VAR_ARGS = 314u16,
    ANON_WHEN = 78u16,
    ANON_WHERE = 6u16,
    WILD_ATTRIBUTE = 199u16,
    ANON_XOR = 125u16,
    WHITESPACE = 325u16,
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
            | ANON_WHERE
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

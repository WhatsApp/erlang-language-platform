-module(doc_attributes_stripped).

-export([main/0]).

-if(OTP_RELEASE >= 27).
-moduledoc "
Check whether doc attributes are stripped from the AST and STUB sections.
".
-moduledoc #{
    authors => [<<"Metamate">>]
  }.

-doc "The main function".
-endif.

-spec main() -> ok.
main() ->
    ok.

-module(edoc_doc_attribute).
-moduledoc "This is the module doc, in OTP 27 style format".

-export([one/0, two/0, three/0]).

%% @doc This is function one, with old style docs
one() ->
    1.

-doc "This is function two, with OTP 27 style docs".
two() ->
    2.

-doc "This is function three, with both OTP 27 and old style docs".
% @doc This is the old style doc. Since a doc  attribute is present, this should not be displayed
three() ->
    2.

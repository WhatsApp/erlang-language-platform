% Sample module from https://github.com/erlang-ls/grammar/blob/main/tests/snap/docstring.erl
-module(otp27_docstrings).

-export([f/0]).

-doc """
     Docstring examples
     """.
f() ->
    """
  Line "1"
  Line "2"
  """ = "Line \"1\"
Line \"2\"",
    X = lists:seq(1,3), % just to check is syntax highlight is still ok

    """
      First line starting with two spaces
    Not escaped: "\t \r \xFF" and """

    """ = "  First line starting with two spaces
Not escaped: \"\\t \\r \\xFF\" and \"\"\"
",
    X = lists:seq(1,3), % just to check is syntax highlight is still ok

    """""
""""
FIXME: previous line is not a closing delimiter because opening-closing delimiter have five double quote characters
""""" = "\"\"\"\"",
    X = lists:seq(1,3), % just to check is syntax highlight is still ok

    ok.

-define(THIS_IS_THE_END, "end"). % just to check is syntax highlight is still ok

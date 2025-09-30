-module(docs).
-compile(nowarn_missing_spec).
-export([my_function/0, my_other_function/0]).

-doc """
This is the doc for my_function
""".
my_function() -> ok.

my_other_function() -> my_function().

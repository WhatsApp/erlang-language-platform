-module(extract_function).

-export([version/1]).

-spec version(atom()) -> string().
version(Application) ->
  {ok, Version} = application:get_key(Application, vsn),
  Version.

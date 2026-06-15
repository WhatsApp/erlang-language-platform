-module(eqwalizer).

-export_type([dynamic/0]).

%% @doc
%% This type is intended to help with code being transitioned
%% from untyped/unchecked mode to gradual mode.
%% Values of dynamic types slip through
%% the fingers of eqWAlizer in GRADUAL mode.
%% dynamic() is a special type which is a subtype and a supertype of all types.
%% It is defined as an alias to any() to be friendly
%% for other tooling (as dialyzer).
%% In strict mode dynamic() is treated by eqWAlizer as an alias to any().
%% @end
-type dynamic() :: any().

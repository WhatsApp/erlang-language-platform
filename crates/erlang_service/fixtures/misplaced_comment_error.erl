-module(misplaced_comment_error).

bad_hint_arg() ->
  get(
    %$eqwalizer: cast(Key) :: dynamic()
    key
  ).

bad_hint_comprehension() ->
  L0 = [1, 2, 3],
  L1 = [
    X || X <- L0,
    %$eqwalizer: cast(X) :: pos_integer()
    X > 1
  ],
  L1.

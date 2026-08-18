%% Same basename as app_a/include/shared.hrl, on purpose: the two must be
%% reported separately.
% elp:ignore W0083 (avoid_type_defs_in_header)
-type app_b_type() :: term().

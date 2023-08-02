-module(docsh_writer).

-callback from_internal(docsh_internal:t()) -> any().
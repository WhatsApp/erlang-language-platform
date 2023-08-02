Dummy modules to exercise "goto definition".

Tests themselves live in `//whatsapp/elp_test/` repo.
To run them against local elp changes, you can do:
```
% ./meta/cargo.sh build --release --target-dir ~/elp-out/
% buck test @mode/dev //whatsapp/elp_test/test:test -- definition --env ELP_PATH_FOR_LSP_TEST=$HOME/elp-out/release/elp
```

Dummy modules to exercise "documentation tooltip (edoc) on hover".

Tests themselves live in `//whatsapp/elp_test/` repo.
To run them against local elp changes, you can do:
```
% ./meta/cargo.sh build --release --target-dir ~/elp-out/
% buck test @mode/dev //whatsapp/elp_test/test:test -- hover --env ELP_PATH_FOR_LSP_TEST=$HOME/elp-out/release/elp
```

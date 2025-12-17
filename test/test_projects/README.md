# Fixture erlang projects for e2e tests.

The key to confine ELP to a non-waserver project is to have:
- rebar.config
- .rebar.root
- build_info plugin (for now copy-pasted from waserver)

From this directory, you can manually test:
```shell
% elp parse-all --project . --to ~/tmp
```

Automated tests live in crates/elp/bin/main.rs

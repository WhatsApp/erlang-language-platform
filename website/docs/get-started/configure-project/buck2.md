---
sidebar_position: 2
---

# Buck2

:::warning

Up to and including the
[2024-02-07](https://github.com/WhatsApp/erlang-language-platform/releases/tag/2024-02-07)
release, the github version is not built with buck2 support enabled. This will
change soon, once we tweak the tests.

:::

If your project uses the [Buck2](https://buck2.build/) build system, add a [.elp.toml](./elp-toml.md) file in the root dir of your project and enable buck support:

```
[buck]
enabled = true
```

Please refer to the [[buck]](./elp-toml.md#buck) section for more configuration options.

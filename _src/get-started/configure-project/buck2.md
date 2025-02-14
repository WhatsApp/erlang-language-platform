---
sidebar_position: 3
---

# Buck2

:::warning

The github version is not built with buck2 support enabled. This will
change soon, once we tweak the tests.

:::

If your project uses the [Buck2](https://buck2.build/) build system, add a [.elp.toml](./elp-toml.md) file in the root dir of your project and enable buck support:

```
[buck]
enabled = true
```

Please refer to the [[buck]](./elp-toml.md#buck) section for more configuration options.

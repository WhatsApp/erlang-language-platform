# Erlang Grammar

TextMate grammar (also used by Visual Studio Code) for the Erlang programming
language.

## Credits

The grammar has been forked from the original Erlang TextMate bundle:

https://github.com/textmate/erlang.tmbundle

The original license stated:

```text
Permission to copy, use, modify, sell and distribute this
software is granted. This software is provided "as is" without
express or implied warranty, and with no claim as to its
suitability for any purpose.
```

The reason of the fork is twofold:

* The original Erlang grammar was not actively maintained and response
  times for Pull Requests were long.
* The grammar can be included in isolation in the VS Code extension

## License

The current repository is licensed under the Apache License 2.0.
Please refer to the `LICENSE` file for details.

## Lazy Guide

According Visual Studio Code Syntax Highlight Guide:
> As a grammar grows more complex, it can become difficult to understand and
> maintain it as json. If you find yourself writing complex regular expressions
> or needing to add comments to explain aspects of the grammar, consider using
> yaml to define your grammar instead.
>
> Yaml grammars have the exact same structure as a json based grammar but allow
> you to use yaml's more concise syntax, along with features such as multi-line
> strings and comments.
>
> VS Code can only load json _[or plist]_ grammars, so yaml based grammars must
> be converted to json _[or plist]_.

To work easier with TextMate YAML language files install VSCode extension e.g.
[TextMate Languages](https://marketplace.visualstudio.com/items?itemName=Togusa09.tmlanguage)
from _Ben Hockley_.

* Use commands _"Convert to YAML-tmLanguage file"_ and
  _"Convert to tmLanguage file"_ provided by extension _TextMate Languages_.

* Alternatively download
  [grahampugh/plist-yaml-plist](https://github.com/grahampugh/plist-yaml-plist)
  from Github and use below commands: (On Windows use e.g. WSL)

  ```bash
  /path/to/plist_yaml.py Erlang.plist Erlang.yaml
  # Edit YAML file here ... then if you are ready convert back to PLIST
  /path/to/yaml_plist.py Erlang.yaml Erlang.plist
  ```

Commit your updates on `Erlang.plist` and ignode `Erlang.yaml`.

See more:

1. Visual Studio Code [Syntax Highlight Guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
2. TextMate [Language Grammars](https://macromates.com/manual/en/language_grammars)
3. [Writing a TextMate Grammar: Some Lessons Learned](https://www.apeth.com/nonblog/stories/textmatebundle.html)

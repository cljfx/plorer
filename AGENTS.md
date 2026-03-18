# Rules

Fix reflection warnings. Use variables and keywords with "?" suffix only to denote predicate functions, never boolean values.

# Format code

```bash
clojure -M:fmt fix src test
```

# Run tests

```bash
clojure -M:test
```

# Git

Only read-only use
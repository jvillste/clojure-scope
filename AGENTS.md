# Test running

## Run all tests

From the project root:

```bash
clojure -M:test
```

## Run a single test

To run one test with the Clojure CLI, call `clojure.test/test-vars` directly:

```bash
clojure -M -e "(require 'clojure-scope.core-test 'clojure.test) (clojure.test/test-vars [#'clojure-scope.core-test/var-dependency-graph-test])"
```

Pattern:

```bash
clojure -M -e "(require 'YOUR.TEST.NS 'clojure.test) (clojure.test/test-vars [#'YOUR.TEST.NS/YOUR-TEST-NAME])"
```

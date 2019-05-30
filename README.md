# Orient(able)
(↻Observe-Orient-Decide-Act↩)

This is a proof-of-concept reference implementation intended as a lightweight way to explore and clarify the [Orientable specification](https://docs.google.com/document/d/1zjWHegvZwTgvU4fOAjUbIwMwQyfPzHoXJVTX8iR--2E/edit#heading=h.2jf8rxk263pw).

Code is in Common Lisp, developed and (to the extent it is) tested with SBCL.

Uses ASDF and QuickLisp.

When configured correctly, this should work and show no failures:
```lisp
(asdf:test-system :orient)
```

To start a web server, from REPL:

```lisp
(orient.web:start-web)
```

Then navigate to `http://localhost:8888`.

To generate and view graphs, `graphviz` must be installed locally, and `dot` must be in the path.

## License

- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

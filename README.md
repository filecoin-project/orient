# Orient(able)
(↻Observe-Orient-Decide-Act↩)

This is a proof-of-concept reference implementation intended as a lightweight way to explore and clarify the [Orientable specification](https://docs.google.com/document/d/1zjWHegvZwTgvU4fOAjUbIwMwQyfPzHoXJVTX8iR--2E/edit#heading=h.2jf8rxk263pw).

Code is in Common Lisp, developed and (to the extent it is) tested with SBCL.

Uses ASDF and QuickLisp.

When configured correctly, this should work and show no failures:
```lisp
(asdf:test-system :orient)
```

## License

- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

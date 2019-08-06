# Orient(able)
(↻Observe-Orient-Decide-Act↩)

[![CircleCI](https://circleci.com/gh/filecoin-project/orient/tree/master.svg?style=svg)](https://circleci.com/gh/filecoin-project/orient/tree/master)

This is a proof-of-concept reference implementation intended as a lightweight way to explore and clarify the [Orientable specification](https://docs.google.com/document/d/1zjWHegvZwTgvU4fOAjUbIwMwQyfPzHoXJVTX8iR--2E/edit#heading=h.2jf8rxk263pw).

## Installation

### sbcl

Code is in Common Lisp, developed and (to the extent it is) tested with SBCL.
```bash
> brew install sbcl
```

or
```bash
> apt-get install sbcl
```

### QuickLisp & ASDF

Install [QuickLisp](https://www.quicklisp.org):

- Download the file for installation. (https://beta.quicklisp.org/quicklisp.lisp)
- Then run sbcl with that file loaded by this command.

```sh
sbcl --load path/of/quicklisp.lisp
```

After sbcl launched, type in the command below.

```lisp
(quicklisp-quickstart:install)
```

At this moment, Quicklisp has already been installed. To load Quicklisp every time you start Lisp (which is recommended), type in command below.

```lisp
(ql:add-to-init-file)
```

### Optional for Emacs Users

Type in the command which will create a file you can load in Emacs to configure the right load-path for loading Quicklisp's installation of SLIME.

```lisp
(ql:quickload "quicklisp-slime-helper")
```

### Integrate the project with quicklisp

QuickLisp needs to find the project, so add a symlink:

```bash
> cd ~/quicklisp/local-projects
> ln -s ~/<installdir>/orient/orient.asd orient.asd
```

### Test the Setup

When configured correctly, this should work and show no failures:

```bash
> sbcl
This is SBCL 1.4.14, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (ql:quickload :orient)
To load "orient":
  Load 1 ASDF system:
    orient
; Loading "orient"
.....................................
(:ORIENT)
* (asdf:test-system :orient)

Running test suite ORIENT-SUITE
 Running test RENAME-ATTRIBUTES ..
 Running test RESTRICT .
 Running test PROJECT-TUPLE .
 Running test PROJECT-RELATION .
 Running test MULTIPLICATION-CONSTRAINT .......
 Running test DIVISION-CONSTRAINT ....
 Running test ADDITION-CONSTRAINT .......
 Running test SUBTRACTION-CONSTRAINT ...
 Running test LOG-CONSTRAINT ....
 Running test INTEGER-CONSTRAINT .......
 Running test EQUALITY-CONSTRAINT ...
 Running test LESS-THAN-CONSTRAINT ..
 Running test LESS-THAN-OR-EQUAL-CONSTRAINT ...
 Running test GREATER-THAN-CONSTRAINT ..
 Running test GREATER-THAN-OR-EQUAL-CONSTRAINT ...
 Running test AND-CONSTRAINT .............
 Running test CONSTRAINT-CONSTANTS ..
 Running test ORIENT-TESTS ............
 Running test JOIN ....
 Running test RENAME-TUPLE .
 Running test RENAME-RELATION .
 Running test SIMPLE-BIDIRECTIONAL ..
 Running test PLANNING-TERMINATES .
 Did 86 checks.
    Pass: 86 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite INTERFACE-SUITE
 Running test ROUNDTRIP-TRANSFORMATION .
 Running test ROUNDTRIP-SIGNATURE .
 Running test ROUNDTRIP-TUPLE .
 Running test ROUNDTRIP-SCHEMA .
 Running test ROUNDTRIP-PARAMETER .
 Running test ROUNDTRIP-COMPONENT .
 Running test ROUNDTRIP-SYSTEM .
 Did 7 checks.
    Pass: 7 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite FILECOIN-SUITE
 Running test PERFORMANCE-TEST .
 Running test MERKLE-TREE-CONSTRAINT-SYSTEM ...
 Running test SELECT-MERKLE-HASH-FUNCTION .
 Running test SELECT-KDF-HASH-FUNCTION .
 Running test MULTIPLE-HASH-FUNCTION-SELECTORS .
 Running test ZIGZAG-SYSTEM .
 Running test FILECOIN-DEFAULTS .
 Did 9 checks.
    Pass: 9 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Running test suite WEB-SUITE
 Running test TEST-ECONOMIC-PERFORMANCE .
 Running test TEST-ZIGZAG .
 Running test TEST-FILECOIN-SECURITY .
 Running test TEST-FILECOIN .
 Did 4 checks.
    Pass: 4 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
*
```

You should get similar output from the shell:

```bash
>  make test
```

To start a web server, from REPL:

```lisp
(orient.web:start-web)
```

Then navigate to `http://localhost:8888`.

To generate graphs, you will need [Graphviz](https://www.graphviz.org/).

To generate and view graphs, `graphviz` must be installed locally, and `dot` must be in the path.


## CLI

There are severals ways to run the CLI.

### Development Mode

- Install [cl-launch](https://www.cliki.net/cl-launch), so `/usr/local/bin/cl`

For example, you may need to create a symlink:
```bash
> ln -s /<installdir>cl-launch.sh /usr/local/bin/cl
>  cl
cl-launch.sh 4.1.5

For help, invoke script with help argument:
        /usr/local/bin/cl -h
```

Now you can run the CLI and source changes will be immediately reflected, but startup is a bit slow.
```bash
> ./bin/orient ... <args>
```

### Executable Image

If you just want to *use* the CLI, first dump an image:

```bash
> make ubercalc
```

Now startup should be very fast:

```bash
> ./bin/ucalc ... <args>
```

### Docker

If you're having a hard time setting up a development environment, Docker might be easiest, but startup is a bit slow.

```docker
> make docker
> ./bin/dcalc ... <args>
```

### Tests from shell

```bash
> make test
```

## License

- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

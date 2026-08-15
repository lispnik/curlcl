# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A CFFI binding to libcurl covering the whole public C API — easy, multi, share,
URL, MIME, header, websockets — with an HTTP client layered on top. SBCL,
ocicl, FiveAM.

## Build & test

```
ocicl install                 # restore dependencies (ocicl/ is gitignored)
make test                     # load #:libcurl/test and run the suite
CURL_LIVE_TESTS=1 make test   # additionally run the network suite
make tables                   # regenerate the option/info tables from headers
```

Run one test from a REPL (FiveAM has no CLI selector):

```lisp
(asdf:load-system :libcurl/test)
(fiveam:run! 'libcurl/test::setopt-long-passes-the-exact-value)
(fiveam:run! 'libcurl/test::client)     ; or a whole suite
```

Adding a slot to a struct needs the fasl cache cleared — `:serial t` does not
save you, and SBCL dies with "attempt to redefine the STRUCTURE-OBJECT class
incompatibly". `make clean` removes `~/.cache/common-lisp/*/$(CURDIR)`.

## Architecture

Load order is significant and encoded in `libcurl.asd`:

```
package → conditions → library → types → varargs → easy-raw → memory
        → options → options-table → infos → infos-table
        → callbacks → easy → url → headers → mime → share → multi → websockets
        → client-response → client-retry → client-session → client-request
```

- `conditions.lisp` has no foreign calls, so it can report a failure to open
  the library itself.
- `varargs.lisp` is the ABI layer; everything that sets an option goes through
  it.
- `easy-raw.lisp` is the unadorned DEFCFUN layer and loads before the tables,
  which are validated against the introspection functions it declares.
- `client-session.lisp` loads before `client-request.lisp` because
  `with-session-handle` is a macro.

`src/options-table.lisp` and `src/infos-table.lisp` are **generated** — edit
`generator/generate-tables.lisp` instead.

## Conventions specific to this codebase

- **Never call a variadic libcurl function directly.** `curl_easy_setopt`,
  `curl_easy_getinfo`, `curl_multi_setopt` and `curl_share_setopt` are variadic,
  and on Darwin arm64 a plain `foreign-funcall` passes the value in a register
  while libcurl reads the stack. There is no error, the option silently takes
  garbage, and on x86-64 the same wrong call works — so this cannot be caught
  by testing on Linux. Use `%setopt-long`, `%setopt-pointer`, `%setopt-off-t`
  and `%getinfo` from `src/varargs.lisp`.

- **A test that only checks `CURLE_OK` proves nothing about argument passing.**
  Pick an option whose *value* libcurl validates and assert on the shape of the
  validation. `CURLOPT_HTTP_VERSION` accepts the sparse set `{0–5, 30, 31}`,
  answers the gaps with `CURLE_UNSUPPORTED_PROTOCOL` and negatives with
  `CURLE_BAD_FUNCTION_ARGUMENT` — three outcomes keyed on the exact integer.
  Which of those values are *accepted* depends on whether the build has HTTP/2
  and HTTP/3, so take the sets from the feature bits; hard-coding one machine's
  answer breaks on Ubuntu (no HTTP/3) and Windows (neither).
  `CURLOPT_MAXFILESIZE_LARGE` set to `#x180000000` is positive as 64 bits and
  negative in its low 32, so a truncated argument is rejected.

- **A test that asserts a width has to measure it, not restate it.**
  `curl_socket_t` was declared `:int` and the test asserted 4; both were wrong
  on Win64, where it is a `SOCKET` (`UINT_PTR`, 8 bytes), and they agreed with
  each other so nothing failed. Where a width is really being checked, get it
  from the platform (`(cffi:foreign-type-size :uintptr)`) or from libcurl —
  poison a buffer, have libcurl write into it, and see how much changed.

- **Windows is in CI, and three things about it are load-bearing.** C `long` is
  4 bytes, not 8. `curl_socket_t` is 8, not 4, and `struct curl_waitfd` grows
  with it. SBCL keeps an OS handle where Unix keeps a file descriptor, so
  standard output is not descriptor 1 — ask `standard-descriptor`, never write
  the number.

- **Do not let a `.lisp` file be checked out with CRLF.** `FORMAT`'s
  line-continuation directive is `~` followed by a *newline*; after a Return it
  is not a directive at all, and every wrapped format string in the file fails
  to compile. `.gitattributes` pins this and must stay.

- **Conditions never unwind into C.** Every trampoline body runs inside
  `with-callback-guard`, which catches `serious-condition`, stashes it, and
  returns the abort value for that callback. Per-callback values are in
  `*callback-options*` and `src/types.lisp`; several are 32-bit magic numbers
  returned from `size_t`-valued functions and must be returned *exactly*.
  A non-local exit out of a callback is still undefined; `handler-case` cannot
  intercept a `throw`, and nothing else can either.

- **"Never set" and "set to NULL" are different states in libcurl.** This bit
  three times. `CURLOPT_READDATA` set to NULL makes the built-in read callback
  `fread` a null `FILE*` and segfault. `CURLOPT_HEADERDATA` set with no header
  function routes headers into the *write* callback. `CURLOPT_WRITEDATA`'s
  built-in writes to stdout. So the binding installs its own trampolines and
  lets a nil closure be a no-op sink, rather than ever clearing a `*DATA` slot.

- **Release order is fixed**: `curl_easy_cleanup` first (it is still reading the
  error buffer, slists and any borrowed payload until it returns), then owned
  foreign memory, then the registry key last so no in-flight callback can find
  a freed state. `reset-handle` has the same hazard — it must null the
  error-buffer slot at the moment it frees it, because `%set-option` reads that
  slot to attach detail to conditions.

- **`curl_easy_duphandle` copies every option value**, so a duplicate points at
  the original's registry key and error buffer. Re-point both.

- **Do not remove a handle from a multi while iterating
  `curl_multi_info_read`.** The `CURLMsg` points into the easy handle's memory
  and `curl_multi_remove_handle` ends its validity; doing it inline made the
  queue re-report a message forever. Drain and copy first, remove after.

- **Layouts are hand-written, not groveled**, so the build needs no C toolchain.
  Their sizes and offsets are pinned by `test/types-tests.lisp` against
  measured values — that is the only thing keeping them honest. `CURLMsg`'s
  union in particular yields a plausible *wrong* CURLcode if read at the wrong
  offset, rather than crashing.

- **Result codes decode tolerantly.** A libcurl newer than this binding can
  return a code it has never heard of; `curlcode-keyword` returns the integer
  unchanged rather than signalling, so that surfaces as an unfamiliar error and
  not as a broken binding.

- **The test server can be made to misbehave on purpose** — `/close-early`
  truncates a body, `/redirect-loop` never terminates, `/flaky` fails exactly N
  times, `/drip` trickles. When a test hangs, suspect the fixture first: a
  route once advertised `Content-Length` eight bytes larger than it wrote, and
  libcurl waiting forever for those bytes was correct behaviour that looked
  exactly like a bug in the multi loop.

- **CURLOPT_POSTFIELDS and CURLOPT_UPLOAD cancel each other.** Setting
  POSTFIELDS -- even to the empty string, which a bodyless POST otherwise
  needs -- replaces a streaming body arranged with UPLOAD, so the upload
  silently becomes zero-length. Likewise CURLOPT_HTTPGET and CURLOPT_POST undo
  UPLOAD, which is why a streamed POST sets its method with CUSTOMREQUEST.

- **A dumped image must not carry a record of libcurl.** SBCL notes open shared
  objects and dyld reopens them at startup *by soname*, which on macOS resolves
  through the shared cache to the system libcurl -- so `bin/curlcl` ran 8.7.1
  while the same code from source ran Homebrew's 8.21.0, with a different TLS
  backend and no websockets. The image-dump hook closes the library so the
  restore hook opens exactly one. Check with `curlcl -V`, which prints the path.

- **Implementation-specific code is confined to three places**, and there is no
  `#-sbcl (error ...)` left: the bulk octet copy in `memory.lisp` (portable
  fallback), `fd-byte-stream` in `cli.lisp` (SBCL/ECL/CCL/CLISP clauses plus a
  /dev/fd fallback), and `force-gc` in the tests. ECL loads the library and
  performs requests, but the full suite stalls partway, so it is not in CI.

- **Exported symbols collide with test helpers.** Three times a test-local
  `defun` or `defmacro` silently redefined a newly exported library function
  (`with-easy`, `header-value`, `response-header`). After adding to the export
  list, re-run the whole suite, not just the new tests.

# libcurl binding for Common Lisp

libcurl is installed everywhere already, make use of it

[![CI](https://github.com/lispnik/libcurl/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/libcurl/actions/workflows/ci.yml)

A comprehensive Common Lisp binding to libcurl — the easy interface with all
308 of its options and 78 info values, multi, share, the URL parser, MIME,
the header API and websockets — plus an HTTP client built on top of it.

Two problems shaped the design, and both are the kind that produce wrong
answers rather than error messages.

**`curl_easy_setopt` is variadic, and that is not a formality.** Under the
Darwin arm64 ABI, variadic arguments are passed on the stack while an ordinary
CFFI call puts the third argument in a register. libcurl then reads the stack
and gets whatever was there: no crash, no error code, the option simply takes
a garbage value. On x86-64 the same wrong call happens to work, which is
exactly why the bug hides. Every option and info call therefore goes through
libffi's `ffi_prep_cif_var`, with the three cifs it needs prepared once at load
time. The test for it does not check that `setopt` returned `CURLE_OK` — that
proves nothing — but that `CURLOPT_HTTP_VERSION` still sorts its sparse
accepted set `{0–5, 30, 31}` from the gaps and from negatives into three
different outcomes.

**A Lisp condition must never unwind through a C frame.** Signalling out of a
write callback is undefined behaviour, but so is swallowing the error. So a
condition raised inside any callback is caught at the boundary, the callback
returns the abort value libcurl documents for *that particular* callback — they
are all different, and several are 32-bit magic numbers returned from functions
declared `size_t` — and `perform` re-signals the original from Lisp once
`curl_easy_perform` has returned. The caller sees their own `file-error`, not
a generic `CURLE_WRITE_ERROR`.

Callbacks reach Lisp through one static trampoline per C signature and a
registry keyed by an integer stored in libcurl's own userdata slot. Every
libcurl callback has such a slot — all 27 of them — so runtime-minted C
function pointers are unnecessary here, and the read path takes no lock.

## Installation

```
ocicl install
```

Then:

```lisp
(asdf:load-system :libcurl)
```

Needs libcurl and `cffi-libffi`. On macOS the binding prefers Homebrew's
libcurl over the one in the dyld shared cache, because the system build has no
websocket support; set `LIBCURL_LIBRARY` to pin a specific one.

## Usage

The package is `libcurl`, nicknamed `curl`.

```lisp
(curl:http-get "https://example.com/")
;; => #<RESPONSE 200 https://example.com/ 559 bytes>
```

A non-2xx status is a response, not a condition. Only transport failures
signal, because only then is there nothing to return.

```lisp
(let ((response (curl:http-get "https://api.example.com/thing")))
  (case (curl:response-status response)
    (200 (curl:response-text response))
    (404 nil)
    (t (warn "unexpected ~D" (curl:response-status response)))))
```

Bodies are decoded when the `Content-Type` says they are text and names a
charset we know; otherwise they arrive as octets. A charset we do not
recognise gives octets rather than a guess.

```lisp
(curl:http-post "https://example.com/form"
                :content '(("name" . "a value") ("other" . "x&y")))

(curl:http-post "https://example.com/upload"
                :multipart '((:name "field" :data "value")
                             (:name "file" :file #p"/tmp/report.pdf"
                              :content-type "application/pdf")))
```

Headers are accepted as an alist, a plist, or a list of strings, because all
three appear in real code:

```lisp
(curl:http-get "https://example.com/" :headers '(("Accept" . "application/json")))
(curl:http-get "https://example.com/" :headers '(:accept "application/json"))
(curl:http-get "https://example.com/" :headers '("Accept: application/json"))
```

Duplicated response headers are kept, which is the only representation that can
be right for `Set-Cookie`:

```lisp
(curl:response-header-values response "set-cookie")   ; => ("a=1" "b=2")
```

### Streaming

Nothing is buffered when you give it somewhere to go — in either direction:

```lisp
(curl:download "https://example.com/big.iso" #p"/tmp/big.iso")

(curl:http-get "https://example.com/big.iso"
               :on-data (lambda (octets) (process octets)))

;; :INPUT is the request-side counterpart of :OUTPUT.  A pathname, a stream,
;; or a reader function; the source never has to fit in memory.
(curl:http-put "https://example.com/big.iso" :input #p"/tmp/big.iso")
```

A size is declared when it can be known, so the request carries a
Content-Length; when it cannot — a pipe, a generator — the body goes out
chunked. A file also gets a seek callback, so libcurl can rewind it to repeat
the request for a redirect or an authentication challenge.

Streaming and `:retry` interact, because a retried transfer delivers its body
from the beginning. Who owns the destination decides whether that is a problem:

```lisp
;; Fine: the pathname is ours, so each attempt reopens and truncates the file.
(curl:download "https://example.com/big.iso" #p"/tmp/big.iso" :retry 3)

;; Signals UNSAFE-RETRY: the stream is yours, and rewinding or truncating it is
;; not this library's to do -- so the failed attempt's bytes would sit in front
;; of the successful attempt's, with nothing to say so.
(with-open-file (out #p"/tmp/big.iso" :direction :output
                                      :element-type '(unsigned-byte 8))
  (curl:http-get url :output out :retry 3))

;; Retry it anyway, having said out loud that seeing part of the body twice is
;; acceptable.
(curl:http-get url :on-data #'process :retry 3 :retry-streamed t)
```

The refusal happens before the first attempt rather than on the retry that
would have corrupted the file.

### Sessions

A session pools easy handles over a share, so connections, DNS answers, TLS
sessions and cookies are common to a run of requests:

```lisp
(curl:with-session (session)
  (curl:http-post (test-url "/login") :content credentials :session session)
  ;; The cookie set by the login is sent with this one.
  (curl:http-get "https://example.com/account" :session session))
```

### Retry

libcurl has no retry logic; this does. The defaults are narrow on purpose —
transport failures that say nothing was processed, plus the statuses that
explicitly mean "try again" — and POST is not retried unless you say so,
because only you know whether repeating one duplicates an order.

```lisp
(curl:http-get "https://flaky.example.com/"
               :retry '(:max-attempts 5 :initial-delay 0.5))
```

### Several at once

```lisp
(curl:request-many (list "https://a.example.com/"
                         (list "https://b.example.com/" :method :post
                                                        :content "body")))
;; => (#<RESPONSE 200 ...> #<RESPONSE 201 ...>)
```

A failure sits in its own slot as a condition rather than aborting the batch.

Retries work here too, and are scheduled rather than sequential — a request
waits out its backoff while the rest of the batch keeps transferring:

```lisp
(curl:request-many urls
                   :retry '(:max-attempts 4 :initial-delay 0.4)
                   :on-complete (lambda (index outcome) (report index outcome)))
```

`:on-complete` fires as each request reaches its final outcome, so it can drive
a progress display; an individual request can override the batch policy, or opt
out with `:retry nil`.

### The binding underneath

The client is a thin layer; the whole C API is there if you want it.

```lisp
(curl:with-easy (handle)
  (setf (curl:callback-function handle :write)
        (lambda (octets) (write-sequence octets *standard-output*) t))
  (curl:setopts handle :url "https://example.com/" :followlocation t)
  (curl:perform handle)
  (curl:getinfo handle :response-code))
```

Options are keywords derived mechanically from the C names — drop `CURLOPT_`,
downcase, underscores to hyphens — so `CURLOPT_SSL_VERIFYPEER` is
`:ssl-verifypeer`. An option the loaded libcurl does not have is reported by
name rather than as `CURLE_UNKNOWN_OPTION`.

## The command-line driver

`make build` produces `bin/curlcl`, a curl(1) workalike built on the library.
Option names, defaults, output destinations and **exit codes** follow curl, so
most curl command lines work unchanged and scripts that check the exit status
keep working — the codes are libcurl's own CURLcode values.

```
$ curlcl -s -o /dev/null -w '%{http_code} %{size_download}b in %{time_total}s\n' https://example.com/
200 559b in 0.086570s

$ curlcl -s -L -H 'Accept: application/json' https://api.example.com/thing
$ curlcl -s -F 'file=@report.pdf;type=application/pdf' https://example.com/upload
$ curlcl -sZ -o a.html -o b.html https://a.example/ https://b.example/   # parallel
$ curlcl --retry 3 https://flaky.example/                                # scheduled backoff
```

Holding to curl's behaviour is the point: it forces the library to cover what a
real client needs rather than what is convenient to expose. `-Z` goes through
`request-many`, `--retry` through the scheduled backoff, `-F` through
`curl_mime_*`, `-w` through `getinfo`.

Two deliberate differences, both in `--help`: there is no progress meter unless
`--progress-bar` is given, and `--upload-file` reads the file into memorycB
rather than streaming it.

`curlcl -V` also reports **which** libcurl it loaded, which curl has no need to
do — this binding can load any of several, and on macOS they differ in version,
TLS backend and protocol support.

## Running the tests

```
make test
```

Integration tests run against an HTTP server started inside the image on an
ephemeral loopback port, so the suite is hermetic and can serve responses no
public endpoint will produce on demand — a truncated body, a redirect loop, a
slow trickle, a route that fails exactly twice. There is a websocket echo
server too.

```
CURL_LIVE_TESTS=1 make test
```

adds a small suite that uses the real network, for the things a local server
cannot stand in for: a real certificate chain, a rejected expired one, real DNS,
HTTP/2, and connection reuse observed through timing.

## Implementation notes

- **The option and info tables are generated** from the curl headers by
  `generator/generate-tables.lisp` and committed, so a build needs no headers
  and no C compiler. Parsing C with regular expressions is only defensible
  because libcurl can describe its own options at runtime: the suite checks
  every entry against `curl_easy_option_by_name` on the library actually
  loaded, which also catches the table and the library having drifted apart.
  Run `make tables` to regenerate.

- **The spelled type matters.** `CURLOPT_URL`, `CURLOPT_HTTPHEADER`,
  `CURLOPT_WRITEDATA` and `CURLOPT_POSTFIELDS` are all "10000 plus something",
  but one is a string libcurl copies, one is an slist the caller must keep
  alive, one is opaque callback data, and one is a buffer libcurl explicitly
  does *not* copy. The generator keeps the nine spelled types rather than the
  five numeric bases, because that distinction decides who owns the memory.

- **`:postfields` is routed to `CURLOPT_COPYPOSTFIELDS`**, so libcurl owns the
  copy. `CURLOPT_COPYPOSTFIELDS` exists precisely because `CURLOPT_POSTFIELDS`
  does not copy, and making callers reason about that is not worth the one
  avoided memcpy.

- **There are no finalizers**, deliberately. The callback registry must hold a
  handle's state strongly for as long as libcurl might call into it, which
  would keep the handle alive and stop a finalizer firing anyway. `with-easy`
  and `with-session` are the contract; the client layer wraps everything in
  `unwind-protect`, so ordinary use cannot leak. `live-callback-count` makes a
  leak assertable.

- **`curl_easy_duphandle` copies every option value**, including
  `CURLOPT_WRITEDATA` and `CURLOPT_ERRORBUFFER` — so a duplicate points at the
  *original's* registry key and error buffer. Both are re-pointed.

- **Clearing a callback keeps the trampoline installed**, with a nil closure
  acting as a no-op sink. "Never set" and "set to NULL" are different states in
  libcurl: `CURLOPT_READDATA` set to NULL makes the built-in read callback call
  `fread` on a null `FILE*`. For the same reason the write and read trampolines
  are installed from the start — libcurl's built-in ones write to stdout and
  read from stdin.

- **Websockets are feature-gated at runtime**, since whether `ws://` works is a
  property of the loaded library. macOS ships the headers for a libcurl built
  without it. libcurl marks this API experimental; that caveat is passed on.

## Implementations

Developed and tested on SBCL, which is what CI runs on macOS, Linux and
Windows.

The library itself is close to portable: the only implementation-specific code
is a bulk octet copy in `memory.lisp`, which has a portable fallback, and it
loads and performs real HTTPS requests on ECL. The full suite does not yet pass
there — it gets through fifteen suites and stalls in the sixteenth, apparently
because the in-process test server spawns a thread per keep-alive connection
and never reaps them — so ECL is **not** in the CI matrix and should be treated
as unverified rather than supported.

`bin/curlcl` needs a byte stream on standard input and output, which the
standard has no way to ask for. There are clauses for SBCL, ECL, CCL and CLISP,
and a `/dev/fd` fallback for any other Unix implementation. The descriptor
comes from the implementation rather than being written as 0 or 1 — see
`standard-descriptor` in `src/cli.lisp` for why that distinction is not
academic.

### Windows

Supported on SBCL, in CI, with the same suite: 1207 checks pass, 16 skip. The
skips are the websocket tests, because the libcurl the runner picks up is built
without `ws://`.

Four things had to be true for this to work, and three of them were broken when
it was first tried:

- **Line endings.** `FORMAT`'s continuation directive is `~` followed by a
  *newline*; with CRLF the next character is a Return, which is not a directive,
  and every format string that wraps stops compiling. `.gitattributes` pins
  `*.lisp` and `*.asd` to LF.
- **`curl_socket_t`.** A file descriptor on Unix, a Win32 `SOCKET` — `UINT_PTR`,
  8 bytes — on Win64. Declaring it `:int` would have had
  `CURLINFO_ACTIVESOCKET` write 8 bytes into 4, passed half a socket to
  `curl_multi_socket_action`, and made `struct curl_waitfd` the wrong size for
  `curl_multi_wait` to read as an array.
- **Standard descriptors.** SBCL on Windows keeps an OS handle where Unix keeps
  a descriptor, so `curlcl` could not write a response body until it stopped
  assuming 1 meant standard output.
- **`cffi-libffi`**, which needs a C toolchain and libffi. MSYS2's MinGW-w64
  packages supply both; this was the part expected to be the obstacle, and it
  was the one thing that worked first time.

C `long` is 4 bytes on Windows and 8 on LP64 Unix. The binding passes CFFI's
`:LONG`, which already tracks that, so nothing needed changing — but the test
that claimed to check it had hard-coded 8, and now measures the width by asking
libcurl to write a `CURLINFO_LONG` into a poisoned buffer instead.

## What is not bound, and why

79 of the 100 `curl_*` symbols the library exports are bound. The rest are
left out deliberately:

| Not bound | Why |
|---|---|
| `curl_formadd`, `curl_formfree`, `curl_formget` | Deprecated on every enumerator since 7.56.0, and variadic with a sentinel-terminated option list. `curl_mime_*` replaces it. |
| `curl_escape`, `curl_unescape` | Deprecated forms that take no handle; `curl_easy_escape` is bound. |
| `curl_multi_socket`, `curl_multi_socket_all` | Deprecated; `curl_multi_socket_action` is bound. |
| `curl_mprintf` and its nine relatives | Lisp has `format`. |
| `curl_strequal`, `curl_strnequal`, `curl_getenv` | `string-equal` and `uiop:getenv`. |
| `curl_global_init_mem` | Bindable, but a Lisp allocator called from libcurl's resolver threads is a GC-deadlock foothold. Left out rather than offered as a trap. |

Version-gated functions — `curl_ws_start_frame`, `curl_multi_get_offt`,
`curl_multi_notify_enable`/`disable`, `curl_easy_ssls_import`/`export` — are
resolved at load time rather than declared, so an older libcurl reports the
absence through `unsupported-feature` instead of failing at the first call.

## License

MIT. See [LICENSE](LICENSE).

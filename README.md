# http-protocol

MIT. CLOS **HTTP client protocol** for [cl-stack](https://github.com/egao1980/cl-stack).

Facade package: `http` (`http:get`, `http:request`, …). Protocol package: `http-protocol`.

| Layer | Repo |
|-------|------|
| Protocol + facade (this) | generics, CE, types, `http:get` / `send` |
| Sync backend | [`http-backend-dexador`](https://github.com/egao1980/http-backend-dexador) |
| gzip / deflate | [`http-encoding-chipz`](https://github.com/egao1980/http-encoding-chipz) |
| br | [`http-encoding-brotli`](https://github.com/egao1980/http-encoding-brotli) |
| zstd | [`http-encoding-zstd`](https://github.com/egao1980/http-encoding-zstd) |

```lisp
(asdf:load-system "http-backend-dexador")
(let ((*http-backend* (http-backend-dexador:make-dexador-backend)))
  (http:get "https://example.com/" :params '(("q" . "hi")))
  (http:post "https://example.com/" :form-data '(("a" . "1")))  ; urlencoded
  (http:post "https://example.com/" :data "hello" :data-type :text)
  ;; typed JSON (no hard JSON dep — bind a codec)
  (http:with-data-codec (:json :encoder #'my-encode :decoder #'my-decode)
    (http:post "https://example.com/" :data ht :data-type :json)
    (http:response-data (http:get "https://example.com/") :json))
  (http:post "https://example.com/"
             :files `(("f" . ,(http:make-http-file (http-protocol:coerce-to-octets "abc")
                                                   :filename "x.bin")))))
```

## Multipart

`:files` / `:form-data` content is a binary input stream, a string (sent as
UTF-8) or a vector of `(unsigned-byte 8)`. Anything else, `(vector 1 2)`
included, signals `http-protocol-error` where the file or part is made
(`make-http-file`, `build-multipart-parts`), not later on the wire.

Boundaries are `----cl-stack-` and 32 hex digits (128 bits). They come from a
process-local `random-state` seeded by `(make-random-state t)` on first use,
never from `cl:*random-state*` (the same in every fresh SBCL image), and the
state is cleared before an image is saved. `make-multipart-body` checks that
`--boundary` occurs in no in-memory part (string or octets, headers included)
and draws another when it does (RFC 2046 §5.1.1), so use the Content-Type and
boundary it returns. A `:boundary` you pass must be 1-70 RFC 2046 bchars.
Stream content cannot be scanned in advance: for stream parts the body relies
on the 128 random bits alone.

## Credentials never print

Wrap a credential with `make-secret`: it prints as `#<SECRET [redacted]>`
in backtraces, condition reports and `describe`. `reveal` unwraps it.

```lisp
(http:post url :headers `(("authorization" . ,(http:make-secret "Bearer sk-…")))
               :auth (list :bearer (http:make-secret token)))
(http-protocol:redact-headers headers)   ; copy for logging
(http-protocol:protect-headers headers)  ; wrap Authorization, Cookie, x-api-key, …
```

`http-request` and `http-client` print method and URL only, never headers or
`:auth`. A backend that returns T from `backend-keeps-secrets-p`
(http-backend-async does) keeps the values wrapped until it writes the wire.
For any other backend, `send` / `send-async` unwrap them first, so older
backends keep working.

**Brief:** [cl-stack `docs/capabilities/http-protocol.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/http-protocol.md)

## Tracking

[cl-stack#30](https://github.com/egao1980/cl-stack/issues/30) · [#47](https://github.com/egao1980/cl-stack/issues/47)

## Publish

Source-only OCI publish is centralized in [`cl-stack-systems`](https://github.com/egao1980/cl-stack-systems)
(`imports/http-protocol/qlfile` pin + shared `publish.yml`). Packaging metadata lives in the `.asd`
(`auto-package-spec`):

```bash
gh workflow run publish.yml -R egao1980/cl-stack-systems -f import=http-protocol
```


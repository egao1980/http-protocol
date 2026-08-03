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
             :files `(("f" . ,(http:make-http-file #(1 2 3) :filename "x.bin")))))
```

**Brief:** [cl-stack `docs/capabilities/http-protocol.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/http-protocol.md)

## Tracking

[cl-stack#30](https://github.com/egao1980/cl-stack/issues/30) · [#47](https://github.com/egao1980/cl-stack/issues/47)

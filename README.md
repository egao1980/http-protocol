# http-protocol

MIT. CLOS **HTTP client protocol** for [cl-stack](https://github.com/egao1980/cl-stack).

Wave-1 starts with **Content-Encoding** (`decode-content-coding` /
`encode-content-coding`). Sync/async backends and the `http` facade follow.

| Concern | Pin |
|---------|-----|
| gzip / deflate | **chipz** + **salza2** |
| br | optional [`cl-stack-brotli`](https://github.com/egao1980/cl-stack-brotli) |
| zstd | optional [`cl-stack-zstd`](https://github.com/egao1980/cl-stack-zstd) |
| MIME / CTE | [`cl-mime`](https://github.com/egao1980/cl-mime) (later; not this module) |

**Brief:** [cl-stack `docs/capabilities/http-protocol.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/http-protocol.md)

`mime:decode-content` / `encode-content` are Content-Transfer-Encoding — different API.

## Load / test

```bash
qlot install
qlot exec ros -S . -e '(asdf:test-system "http-protocol")'
```

Without qlot (Quicklisp + source registry):

```bash
sbcl --eval '(asdf:load-asd "http-protocol.asd")' \
     --eval '(asdf:test-system "http-protocol")'
```

`default-accept-encoding` omits `br`/`zstd` when overlays are missing (warns once).

## Tracking

[cl-stack#3](https://github.com/egao1980/cl-stack/issues/3) · [#47](https://github.com/egao1980/cl-stack/issues/47) · [#30](https://github.com/egao1980/cl-stack/issues/30)

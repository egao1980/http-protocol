# http-protocol

MIT. CLOS **HTTP client protocol** for [cl-stack](https://github.com/egao1980/cl-stack).

Wave-1 starts with **Content-Encoding** generics. Sync/async backends and the `http` facade follow.

| Layer | Repo |
|-------|------|
| Protocol (this) | generics, parse, Accept-Encoding probe, conformance |
| gzip / deflate | [`http-encoding-chipz`](https://github.com/egao1980/http-encoding-chipz) |
| br | [`http-encoding-brotli`](https://github.com/egao1980/http-encoding-brotli) → [`cl-stack-brotli`](https://github.com/egao1980/cl-stack-brotli) |
| zstd | [`http-encoding-zstd`](https://github.com/egao1980/http-encoding-zstd) → [`cl-stack-zstd`](https://github.com/egao1980/cl-stack-zstd) |

Same shape as `event-protocol` / `event-backend-*`: **no plugin registry** — load the ASDF system; methods appear. Soft-load probes `*content-coding-systems*` for `default-accept-encoding`.

`mime:decode-content` / `encode-content` are Content-Transfer-Encoding — different API.

**Brief:** [cl-stack `docs/capabilities/http-protocol.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/http-protocol.md)

## Load / test

```bash
qlot install
qlot exec ros -S . -e '(asdf:test-system "http-protocol")'
```

Conformance (from a backend test system):

```lisp
(http-protocol/conformance:run-for-codings '(:gzip :deflate))
```

## Adding an encoding backend

1. New repo `http-encoding-<impl>` depending on `http-protocol` (+ natives as needed).
2. Specialize `decode-content-coding` / `encode-content-coding` on coding keywords (octets + stream).
3. Add the coding → system entry to `*content-coding-systems*` in this repo (PR).
4. `/tests` calls `http-protocol/conformance:run-for-codings` with the codings you implement.

## Tracking

[cl-stack#3](https://github.com/egao1980/cl-stack/issues/3) · [#47](https://github.com/egao1980/cl-stack/issues/47) · [#30](https://github.com/egao1980/cl-stack/issues/30)

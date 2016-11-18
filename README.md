# gRPC for Haskell

This is an experimental library for using gRPC from Haskell. It's not ready for
production use.

The haskell library wraps the [C library](https://github.com/grpc/grpc). Only
the client side of RPCs are supported.
`grpc-haskell` does not generate the RPC stubs, nor does it
handle proto serialization/deserialization. Projects that generate Haskell code
for serialization/deserialization may also generate stubs that use
`grpc-haskell`.

Client side features;
 - [x] Low level API for client calls, streaming and unary.
 - [ ] Higher level client API.

## License

grpc-haskell is released under the same license as
[gRPC](https://github.com/grpc/grpc), repeated in [LICENSE](LICENSE).

## Contributing

Please get involved! See our [guidelines for contributing](CONTRIBUTING.md).

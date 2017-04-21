
.PHONY: all clean

all: grpc.cabal

grpc.cabal: grpc.cabal.template third_party/grpc/build.yaml
	rm third_party/grpc/grpc.cabal || true
	cp grpc.cabal.template third_party/grpc/templates/
	(cd third_party/grpc && ./tools/buildgen/generate_projects.sh)
	cp third_party/grpc/grpc.cabal grpc.cabal

clean:
	cabal clean
	rm grpc.cabal

# vim: noexpandtab tabstop=8 shiftwidth=8 softtabstop=8

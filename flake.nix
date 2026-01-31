{
  description = "Crudder - A transport-agnostic DSL for API contracts";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Rust
            rustToolchain
            pkg-config
            openssl

            # Database
            postgresql_16

            # Node.js for TypeScript testing
            nodejs_22
            nodePackages.typescript
            nodePackages.ts-node

            # Testing tools
            curl
            jq
          ];

          shellHook = ''
            export PGDATA="$PWD/.pg-data"
            export PGHOST="$PWD/.pg-socket"
            export DATABASE_URL="postgresql:///crudder_test?host=$PGHOST"

            # Initialize PostgreSQL if needed
            if [ ! -d "$PGDATA" ]; then
              echo "Initializing PostgreSQL..."
              initdb --no-locale --encoding=UTF8 -D "$PGDATA"
              echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"
              echo "listen_addresses = '''" >> "$PGDATA/postgresql.conf"
            fi

            mkdir -p "$PGHOST"

            echo ""
            echo "Crudder development shell"
            echo "========================="
            echo ""
            echo "To start PostgreSQL:"
            echo "  pg_ctl start -D \$PGDATA -l \$PGDATA/log"
            echo ""
            echo "To create test database:"
            echo "  createdb crudder_test"
            echo ""
            echo "To stop PostgreSQL:"
            echo "  pg_ctl stop -D \$PGDATA"
            echo ""
          '';
        };

        # Test shell with PostgreSQL auto-started
        devShells.test = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustToolchain
            pkg-config
            openssl
            postgresql_16
            curl
            jq
          ];
        };
      }
    );
}

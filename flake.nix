{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    crane.url = "github:ipetkov/crane";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    flake-utils,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;

        overlays = [(import rust-overlay)];
      };

      rustToolchain = pkgs.rust-bin.stable.latest.default;

      craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

      commonArgs = with pkgs; {
        src = lib.cleanSourceWith {
          src = craneLib.path ./.;

          filter = path: type:
            (lib.hasSuffix "Cargo.toml" path)
            || (lib.hasSuffix "Cargo.lock" path)
            || (lib.hasSuffix "rustfmt.toml" path)
            || (
              ((lib.hasInfix "/src" path) || (lib.hasInfix "/tests" path) || (lib.hasInfix "/benches" path))
              && (
                (lib.hasSuffix ".log" path)
                || (lib.hasSuffix ".nes" path)
                || (craneLib.filterCargoSources path type)
              )
            );
        };

        buildInputs = with pkgs; [SDL2];
      };

      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      nes = craneLib.buildPackage (commonArgs
        // {
          inherit cargoArtifacts;

          strictDeps = true;
        });
    in {
      checks = {
        inherit nes;

        nes-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;

            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

        nes-fmt = craneLib.cargoFmt commonArgs;
      };

      packages = {
        default = nes;
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.checks.${system};

        nativeBuildInputs = with pkgs; [
          rustToolchain
          cargo-outdated
        ];
      };

      apps.default = flake-utils.lib.mkApp {
        drv = nes;
      };

      formatter = pkgs.alejandra;
    });
}

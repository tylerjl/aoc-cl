{
  description = "Advent of Code in Common Lisp";

  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs @ { devshell, flake-parts, flake-utils, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        devshell.flakeModule
      ];

      systems = flake-utils.lib.defaultSystems;

      perSystem = { pkgs, ... }: {
        devshells.default = {
          packages = with pkgs; [
            just
            rlwrap
            (sbcl.withPackages (p: with p; [
              adopt
              alexandria
              cl-ppcre
              esrap
              fiveam
              iterate
              lparallel
              serapeum
              transducers
              trivia
              trivial-benchmark
            ]))
          ];
        };
      };
    };
}

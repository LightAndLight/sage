{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs { inherit system; }; in {
    devShell = pkgs.mkShell {
      buildInputs = with pkgs; [
        haskellPackages.ghc
        haskell-language-server
        cabal-install
        haskellPackages.fourmolu
        
        valgrind
        python3 # used by cg_annotate
      ];
    };
  });
}

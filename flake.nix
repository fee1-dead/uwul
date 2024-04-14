{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.default = with pkgs; pkgs.mkShell {
        buildInputs = [ pkg-config openssl gcc llvmPackages_14.llvm libffi libxml2 ];
        shellHook = ''
          export LLVM_SYS_140_PREFIX=${llvmPackages_14.llvm.dev}
        '';
      };
    });
}
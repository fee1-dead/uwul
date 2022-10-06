with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "terry-env";
  buildInputs = [ pkg-config openssl gcc ];
}
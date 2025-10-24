# shell.nix
let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixos-25.05";
  pkgs = import nixpkgs { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
      pkgs.clojure
	  pkgs.clojure-lsp
  ];
}


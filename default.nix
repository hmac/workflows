let
  name = "workflows";
  compiler = "ghc883";

  nixpkgs = import <nixpkgs> {};
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "${name}" = self.callCabal2nix "${name}" (gitignore ./.) {};
    };
  };

  shell = haskellPackages.shellFor {
    packages = p: [p."${name}"];
    buildInputs = with nixpkgs.haskellPackages; [
      haskellPackages.cabal-install
      ghcid
      ormolu
      hlint
      nixpkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };

  exe = nixpkgs.haskell.lib.justStaticExecutables (haskellPackages."${name}");

in {
  inherit shell;
  inherit exe;
  inherit haskellPackages;
  "${name}" = haskellPackages."${name}";
}

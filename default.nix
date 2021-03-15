{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
, node ? "nodejs-15_x"
}: pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "wasm-alife";
    src = ./.;
  };

  modules = let nodeSrc = pkgs."${node}"; in
    [{
      packages.inline-js-core.configureFlags = [
        ''--ghc-option=-DINLINE_JS_NODE=\"${nodeSrc}/bin/node\"''
      ];
    }];
}

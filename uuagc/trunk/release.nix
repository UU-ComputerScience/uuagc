{nixpkgs ? ../nixpkgs}:

let

  pkgs = import nixpkgs {};


  jobs = rec {

    tarball =
      { uuagcSrc ? {outPath = ./.; rev = 1234;}
      , officialRelease ? false
      }:

      pkgs.releaseTools.makeSourceTarball {
        name = "uuagc-tarball";
        version = builtins.readFile ./VERSION;
        src = uuagcSrc;
        inherit officialRelease;
	distTarget = "source_tarball";
        buildInputs = [ pkgs.haskellPackages.uuagc pkgs.haskellPackages.ghc pkgs.haskellPackages.uulib ];
      };


    build =
      { tarball ? jobs.tarball {}
      , system ? "i686-linux"
      , uulib ? (import ../uulib/release.nix {}).build {inherit system;}
      }:

      with import nixpkgs {inherit system;};

      haskellPackages.cabal.mkDerivation (self: {
        pname = "uuagc";
        version = "head";
        src = "${tarball}/tarballs/*.tar.gz";
        extraBuildInputs = [ uulib ];
      });
  };

in jobs

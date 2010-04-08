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
      , uulib_i686 ? (import ../uulib/release.nix {}).build { system = "i686-linux"; }
      , uulib_x64 ? {}
      }:

      with import nixpkgs {inherit system;};

      let uulib = if system == "i686-linux" then uulib_i686 else uulib_x64; in

      haskellPackages.cabal.mkDerivation (self: {
        pname = "uuagc";
        version = "head";
        src = "${tarball}/tarballs/*.tar.gz";
        extraBuildInputs = [ uulib ];
      });
  };

in jobs

{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" ])
          root;
      };
      projects =
        with lib;
        genAttrs'
          (fileset.toList (fileset.fileFilter (file: file.hasExt "cabal") ./.))
          (file: nameValuePair (removeSuffix ".cabal" (baseNameOf file)) (dirOf file));
      pnames = attrNames projects;
      libPnames = filter (pname: !lib.elem pname [ "tourist" ]) pnames;
      haskell-overlay = pkgs: with pkgs.haskell.lib.compose; lib.composeManyExtensions [
        (hfinal: hprev: lib.mapAttrs (pname: dir: hfinal.callCabal2nix pname (sourceFilter dir) { }) projects)
        (hfinal: hprev: {
          typed-process-effectful = dontCheck (doJailbreak (unmarkBroken hprev.typed-process-effectful));
          dramaturge = lib.pipe hprev.dramaturge [
            (drv: drv.overrideAttrs (attrs: {
              # Firefox needs a writable home for fontconfig
              preCheck = ''
                ${attrs.preCheck or ""}
                export HOME=$TMPDIR
              '';
            }))
            (addTestToolDepend pkgs.firefox)
          ];
          tourist = hprev.tourist.overrideAttrs (attrs: {
            nativeBuildInputs = [ pkgs.makeWrapper ] ++ attrs.nativeBuildInputs or [ ];
            postInstall = ''
              ${attrs.postInstall or ""}
              wrapProgram "$out"/bin/tourist --prefix PATH : "${lib.makeBinPath [pkgs.firefox]}"
            '';
          });
        })
        (hfinal: hprev: lib.optionalAttrs (lib.versionAtLeast hprev.ghc.version "9.12") {
          HList = doJailbreak hprev.HList;
        })
      ];
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay prev)
            ];
          };
          inherit (final.haskellPackages) tourist;
        })
      ];
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let
        pkgs = pkgs'.extend overlay;
        hps = with lib; foldlAttrs
          (acc: name: hp':
            let
              hp = tryEval hp';
              version = getVersion hp.value.ghc;
              majorMinor = versions.majorMinor version;
              ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
            in
            if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
            then acc // { ${ghcName} = hp.value; }
            else acc
          )
          { default = pkgs.haskellPackages; }
          pkgs.haskell.packages;
        pname = "dramaturge";
        libs = pkgs.buildEnv {
          name = "${pname}-libs";
          paths =
            lib.mapCartesianProduct
              ({ hp, pname }: hp.${pname})
              { hp = attrValues hps; pname = libPnames; };
          pathsToLink = [ "/lib" ];
        };
        docs = pkgs.buildEnv {
          name = "${pname}-docs";
          paths = map (pname: pkgs.haskell.lib.documentationTarball hps.default.${pname}) libPnames;
        };
        sdist = pkgs.buildEnv {
          name = "${pname}-sdist";
          paths = map (pname: pkgs.haskell.lib.sdistTarball hps.default.${pname}) libPnames;
        };
        docsAndSdist = pkgs.linkFarm "${pname}-docsAndSdist" { inherit docs sdist; };
      in
      {
        legacyPackages.${system} = pkgs;
        packages.${system}.default = pkgs.symlinkJoin {
          name = "${pname}-all";
          paths = [ libs docsAndSdist ];
          inherit (hps.default.syntax) meta;
        };
        devShells.${system} =
          foreach hps (ghcName: hp: {
            ${ghcName} = hp.shellFor {
              packages = ps: map (pname: ps.${pname}) pnames;
              nativeBuildInputs = with pkgs'; with haskellPackages; [
                cabal-install
                cabal-gild
                fourmolu
                firefox
                hp.haskell-language-server
              ];
            };
          });
        formatter.${system} = pkgs.writeShellApplication {
          name = "formatter";
          runtimeInputs = with pkgs; with haskellPackages; [
            cabal-gild
            fd
            fourmolu
            nixpkgs-fmt
          ];
          text = ''
            fd --extension=nix -X nixpkgs-fmt
            fd --extension=hs -X fourmolu -i
            fd --extension=cabal -x cabal-gild --io
          '';
        };
      }
    );
}

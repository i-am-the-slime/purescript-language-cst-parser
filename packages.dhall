let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230401/packages.dhall
        sha256:d385eeee6ca160c32d7389a1f4f4ee6a05aff95e81373cdc50670b436efa1060

in  upstream
  with argonaut-codecs.version = "50e79f6cd526f875a1e93477aafd4ed98f270266"
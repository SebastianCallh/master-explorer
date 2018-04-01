{}:

(import ./deps/reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common  = ./common;
    server  = ./server;
    client  = ./client;
    scraper = ./scraper;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc   = ["common" "client" "server" "scraper"];
    ghcjs = ["common" "client"];
  };

  overrides = self: super: {
    servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
      owner  = "sebastiancallh";
      repo   = "servant-reflex";
      rev    = "2ca67f5d92750821e864ec5c968db3ecc5da92e1";
      sha256 = "012d7n565ariw1zkrr1lsncicssfqwjdxa12ylh49wd9qfy4h54a";
    }) {};
  };
})
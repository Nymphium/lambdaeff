let
  pkgs = import ./nix/pkgs.nix;
  hls = pkgs.haskell-language-server.override {
            # corresponds to lts-20.20
            supportedGhcVersions = [ "902" ];
            dynamic = true;
        };
in
pkgs.mkShell {
  buildInputs = [ pkgs.stack pkgs.gmp hls ];
  shellHook = ''
    export LANG=C.UTF-8
  '';
}

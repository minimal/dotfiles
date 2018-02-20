# scripts to put in bin
self: super: {
  sshrc = with super; stdenv.mkDerivation rec {
    name = "sshrc-${version}";
    version = "042120";

    src = fetchFromGitHub {
      owner = "Russell91";
      repo = "sshrc";
      rev = "0421208e776203bfdec76c453bd90e5ce23774c7";
      sha256 = "0v786p3kfgx2pglx322f961pbyskwq7vnmb5yqpdkm5qr5rl1i4a";
    };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp -p sshrc $out/bin/sshrc
      cp -p moshrc $out/bin/moshrc
    '';

    meta = with stdenv.lib; {
      description = "bring your .bashrc, .vimrc, etc. with you when you ssh";
      homepage = https://github.com/Russell91/sshrc;
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };

  loc = with super; stdenv.mkDerivation rec {
    name = "loc-${version}";
    version = "0.4.1";

    src = fetchzip {
      url = "https://github.com/cgag/loc/releases/download/v0.4.1/trust-v0.4.1-i686-apple-darwin.tar.gz";
      sha256 = "1akckl5jy7ndd32k0792dlbw3dyfkcpi5ws0zh3hqf2n04ylnqqd";
      name = "loc";
    };

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp -p $src/loc $out/bin/loc
    '';

    meta = with stdenv.lib; {
      description = "Count lines of code quickly.";
      homepage = https://github.com/cgag/loc;
    };
  };
}

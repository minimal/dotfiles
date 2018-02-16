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
}

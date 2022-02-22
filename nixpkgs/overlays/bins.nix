# scripts to put in bin
self: super: {
  sshrc = with super; stdenv.mkDerivation rec {
    name = "sshrc-${version}";
    version = "042120";

    src = null; # repo no longer exists. Find a fork
    #   fetchFromGitHub {
    #   owner = "Russell91";
    #   repo = "sshrc";
    #   rev = "0421208e776203bfdec76c453bd90e5ce23774c7";
    #   sha256 = "0v786p3kfgx2pglx322f961pbyskwq7vnmb5yqpdkm5qr5rl1i4a";
    # };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp -p sshrc $out/bin/sshrc
      cp -p moshrc $out/bin/moshrc
    '';

    meta = with stdenv.lib; {
      description = "[github page 404] bring your .bashrc, .vimrc, etc. with you when you ssh";
      homepage = https://github.com/Russell91/sshrc;
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };

  # superceded by scc. Left here as example of intalling from zip
  # loc = with super; stdenv.mkDerivation rec {
  #   name = "loc-${version}";
  #   version = "0.4.1";

  #   src = fetchzip {
  #     url = "https://github.com/cgag/loc/releases/download/v0.4.1/trust-v0.4.1-i686-apple-darwin.tar.gz";
  #     sha256 = "1akckl5jy7ndd32k0792dlbw3dyfkcpi5ws0zh3hqf2n04ylnqqd";
  #     name = "loc";
  #   };

  #   phases = [ "installPhase" ];

  #   installPhase = ''
  #     mkdir -p $out/bin
  #     cp -p $src/loc $out/bin/loc
  #   '';

  #   meta = with stdenv.lib; {
  #     description = "Count lines of code quickly.";
  #     homepage = https://github.com/cgag/loc;
  #   };
  # };

  prettyping = with super; stdenv.mkDerivation rec {
    name = "prettyping-${version}";
    version = "e8d753";

    src = fetchFromGitHub {
      owner = "denilsonsa";
      repo = "prettyping";
      rev = "e8d7538b8742b27cffe28e9dfe13d1d1a12288e3";
      sha256 = "05vfaq9y52z40245j47yjk1xaiwrazv15sgjq64w91dfyahjffxf";
    };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp -p prettyping $out/bin/prettyping
    '';

    meta = with lib; {
      description = "prettyping` is a wrapper around the standard `ping` tool, making the output prettier, more colorful, more compact, and easier to read.";
      homepage = http://denilsonsa.github.io/prettyping/;
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };

  babashka-bin =
    with super; stdenv.mkDerivation rec {
      name = "babashka-${version}";
      version = "0.7.5";
      src =
        let
          mac = fetchzip {
              url = "https://github.com/babashka/babashka/releases/download/v0.7.5/babashka-0.7.5-macos-amd64.tar.gz";
              sha256 = "bO4UparMVneoiaeVlBYbLfGeBZauzE9sH0Ih1PHT/Xk=";
              name = "babashka";
            };
          sources = {
            "x86_64-darwin" = mac;
            "aarch64-darwin" = mac;
            "x86_64-linux" = fetchzip {
              url = "https://github.com/babashka/babashka/releases/download/v0.7.5/babashka-0.7.5-linux-amd64.tar.gz";
              sha256 = "SnSNWTrPFqqiUY3MVX6uCYvayxQYd7s3liIWWbVeVGk=" ;
              name = "babashka";
            };
          };
        in
        sources."${stdenv.hostPlatform.system}";

      phases = [ "installPhase" ];

      installPhase = ''
        mkdir -p $out/bin
        cp -p $src/bb $out/bin/bb
      '';

      meta = with lib; {
        description = "Native, fast starting Clojure interpreter for scripting";
        homepage = https://github.com/babashka/babashka;
        platforms = [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];
      };
    };
}
# See for fetching different archs
# https://github.com/jtacoma/nixpkgs/blob/42e09c2134add3ae66c6579478c474aeffd8443d/pkgs/development/interpreters/dart/default.nix

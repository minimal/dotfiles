# scripts to put in bin
self: super: {
  sshrc = with super;
    stdenv.mkDerivation rec {
      name = "sshrc-${version}";
      version = "042120";

      src = null; # repo no longer exists. Find a fork
      #   fetchFromGitHub {
      #   owner = "Russell91";
      #   repo = "sshrc";
      #   rev = "0421208e776203bfdec76c453bd90e5ce23774c7";
      #   sha256 = "0v786p3kfgx2pglx322f961pbyskwq7vnmb5yqpdkm5qr5rl1i4a";
      # };

      phases = ["unpackPhase" "installPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp -p sshrc $out/bin/sshrc
        cp -p moshrc $out/bin/moshrc
      '';

      meta = with stdenv.lib; {
        description = "[github page 404] bring your .bashrc, .vimrc, etc. with you when you ssh";
        homepage = "https://github.com/Russell91/sshrc";
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

  prettyping = with super;
    stdenv.mkDerivation rec {
      name = "prettyping-${version}";
      version = "e8d753";

      src = fetchFromGitHub {
        owner = "denilsonsa";
        repo = "prettyping";
        rev = "e8d7538b8742b27cffe28e9dfe13d1d1a12288e3";
        sha256 = "05vfaq9y52z40245j47yjk1xaiwrazv15sgjq64w91dfyahjffxf";
      };

      phases = ["unpackPhase" "installPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp -p prettyping $out/bin/prettyping
      '';

      meta = with lib; {
        description = "prettyping` is a wrapper around the standard `ping` tool, making the output prettier, more colorful, more compact, and easier to read.";
        homepage = "http://denilsonsa.github.io/prettyping/";
        license = licenses.mit;
        platforms = platforms.unix;
      };
    };

  babashka-bin = with super;
    stdenv.mkDerivation rec {
      name = "babashka-${version}";
      version = "0.8.2";
      src = let
        mac = fetchzip {
          url = "https://github.com/babashka/babashka/releases/download/v0.8.2/babashka-0.8.2-macos-amd64.tar.gz";
          sha256 = "FpEDWCEFP6joortbcQMyiVuB3Boio+2nmGihEthdMfM=";
          name = "babashka";
        };
        sources = {
          "x86_64-darwin" = mac;
          "aarch64-darwin" = mac;
          "x86_64-linux" = fetchzip {
            url = "https://github.com/babashka/babashka/releases/download/v0.8.2/babashka-0.8.2-linux-amd64.tar.gz";
            sha256 = "SmSNWTrPFqqiUY3MVX6uCYvayxQYd7s3liIWWbVeVGk=";
            name = "babashka";
          };
        };
      in
        sources."${stdenv.hostPlatform.system}";

      phases = ["installPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp -p $src/bb $out/bin/bb
      '';

      meta = with lib; {
        description = "Native, fast starting Clojure interpreter for scripting";
        homepage = https://github.com/babashka/babashka;
        platforms = ["aarch64-darwin" "x86_64-darwin" "x86_64-linux"];
      };
    };

  # beads_rust (br) - agent-first issue tracker.
  # Uses the statically-linked musl build on Linux: the gnu prebuilds require
  # glibc >= 2.38 and fail on older distros (e.g. Ubuntu 22.04 / glibc 2.35).
  # To update: bump `version`, then refresh the hashes (one per artifact) with:
  #   nix-prefetch-url --unpack "https://github.com/Dicklesworthstone/beads_rust/releases/download/vX.Y.Z/br-X.Y.Z-linux_musl_amd64.tar.gz"
  br = with super;
    stdenv.mkDerivation rec {
      name = "br-${version}";
      version = "0.3.2";
      src = let
        sources = {
          "x86_64-linux" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_rust/releases/download/v${version}/br-${version}-linux_musl_amd64.tar.gz";
            sha256 = "sha256-YAlhCv1lRbZCkX4SQkxsDa8zjpYbGVX0WlJIm7Oa8zQ=";
            name = "br";
            stripRoot = false; # tarball has br + README + LICENSE at top level
          };
          "aarch64-linux" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_rust/releases/download/v${version}/br-${version}-linux_musl_arm64.tar.gz";
            sha256 = "sha256-Mt0NJzl/oiQ4qOZE+5qE3Mt6UMUwaBImnMZIK2kyFDM=";
            name = "br";
            stripRoot = false; # tarball has br + README + LICENSE at top level
          };
          "x86_64-darwin" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_rust/releases/download/v${version}/br-${version}-darwin_amd64.tar.gz";
            sha256 = "sha256-+X7IAiZ/YrxjomL5t/T8NU1y/uCie05v85Yoi7vjap8=";
            name = "br";
            stripRoot = false; # tarball has br + README + LICENSE at top level
          };
          "aarch64-darwin" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_rust/releases/download/v${version}/br-${version}-darwin_arm64.tar.gz";
            sha256 = "sha256-RZrTqNFF9tp0OcUDdLqkFrdnds6R3LeL1TtR9z3PF5M=";
            name = "br";
            stripRoot = false; # tarball has br + README + LICENSE at top level
          };
        };
      in
        sources."${stdenv.hostPlatform.system}";

      phases = ["installPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp -p $src/br $out/bin/br
      '';

      meta = with lib; {
        description = "Agent-first issue tracker (SQLite + JSONL)";
        homepage = "https://github.com/Dicklesworthstone/beads_rust";
        # MIT with OpenAI/Anthropic rider (custom variant, not plain MIT)
        license = licenses.mit;
        platforms = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      };
    };

  # beads_viewer (bv) - companion viewer for beads_rust.
  # Statically linked Go binary, so no glibc concerns on any distro.
  # To update: bump `version`, then refresh the hashes (one per artifact) with:
  #   nix-prefetch-url --unpack "https://github.com/Dicklesworthstone/beads_viewer/releases/download/vX.Y.Z/bv_linux_amd64.tar.gz"
  bv = with super;
    stdenv.mkDerivation rec {
      name = "bv-${version}";
      version = "0.20.0";
      src = let
        sources = {
          "x86_64-linux" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_viewer/releases/download/v${version}/bv_linux_amd64.tar.gz";
            sha256 = "sha256-1seS7P7hKq2xzVJOiIL+YidIhfasf3OjZyg9tMY6Kck=";
            name = "bv";
            stripRoot = false; # tarball has bv + LICENSE + README + CHANGELOG at top level
          };
          "aarch64-linux" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_viewer/releases/download/v${version}/bv_linux_arm64.tar.gz";
            sha256 = "sha256-JYV0YPIRXk27ubs3gCWSsW33qBhFGyVl2v/d454PG4M=";
            name = "bv";
            stripRoot = false; # tarball has bv + LICENSE + README + CHANGELOG at top level
          };
          "x86_64-darwin" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_viewer/releases/download/v${version}/bv_darwin_amd64.tar.gz";
            sha256 = "sha256-7XgE44JPESlrVIb/AkBaYLEYg3PQc/y1kp0tNS4ZGlE=";
            name = "bv";
            stripRoot = false; # tarball has bv + LICENSE + README + CHANGELOG at top level
          };
          "aarch64-darwin" = fetchzip {
            url = "https://github.com/Dicklesworthstone/beads_viewer/releases/download/v${version}/bv_darwin_arm64.tar.gz";
            sha256 = "sha256-hBFAqI1rDYegknMYeA8mR2BGYPgQcfIOn9WZ3DPk3Ls=";
            name = "bv";
            stripRoot = false; # tarball has bv + LICENSE + README + CHANGELOG at top level
          };
        };
      in
        sources."${stdenv.hostPlatform.system}";

      phases = ["installPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp -p $src/bv $out/bin/bv
      '';

      meta = with lib; {
        description = "Companion viewer for the beads_rust issue tracker";
        homepage = "https://github.com/Dicklesworthstone/beads_viewer";
        # MIT with OpenAI/Anthropic rider (custom variant, not plain MIT)
        license = licenses.mit;
        platforms = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      };
    };
}
# See for fetching different archs
# https://github.com/jtacoma/nixpkgs/blob/42e09c2134add3ae66c6579478c474aeffd8443d/pkgs/development/interpreters/dart/default.nix


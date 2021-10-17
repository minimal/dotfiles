{ lib, stdenv }:

stdenv.mkDerivation {
  pname = "pragmatapro";
  version = "unstable-2021-05-19";

  src = builtins.fetchGit {
    url = "ssh://git@github.com/minimal/fonts.git";
    ref = "main";
    rev = "b82ce9c6addda74e96479d862d382d12d1c91712";
  };

  installPhase = ''
    install -m644 --target $out/share/fonts/truetype/pragmatapro -D $src/pragmata-pro-family/*.ttf
  '';

  meta = with lib; {
    homepage = "https://www.fsd.it/shop/fonts/pragmatapro/";
    description = "PragmataPro™ is a condensed monospaced font optimized for screen";
    maintainers = with maintainers; [ minimal ];
    # license = licenses.unfree;
    platforms = platforms.all;
  };
}

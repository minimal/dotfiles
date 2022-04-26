{
  lib,
  stdenv,
}:
stdenv.mkDerivation {
  pname = "pragmatapro";
  version = "unstable-2021-05-19";

  src = builtins.fetchGit {
    url = "ssh://git@github.com/minimal/fonts.git";
    ref = "main";
    rev = "df49ce64374327ba6262ac71a64ef437013fee2d";
  };

  installPhase = ''
    install -m644 --target $out/share/fonts/truetype/pragmatapro -D $src/pragmata-pro-family/*.ttf
  '';

  meta = with lib; {
    homepage = "https://www.fsd.it/shop/fonts/pragmatapro/";
    description = "PragmataPro™ is a condensed monospaced font optimized for screen";
    maintainers = with maintainers; [minimal];
    # license = licenses.unfree;
    platforms = platforms.all;
  };
}

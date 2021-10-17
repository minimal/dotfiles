{ lib, stdenv }:

stdenv.mkDerivation {
  pname = "equity";
  version = "unstable-2021-10";

  src = builtins.fetchGit {
    url = "ssh://git@github.com/minimal/fonts.git";
    ref = "main";
    rev = "df49ce64374327ba6262ac71a64ef437013fee2d";
  };

  installPhase = ''
    install -m644 --target $out/share/fonts/opentype/equity -D $src/equity/OpenType/*.otf
  '';

  meta = with lib; {
    homepage = "https://practicaltypography.com/equity.html";
    description = "Serif font";
    maintainers = with maintainers; [ minimal ];
    # license = licenses.unfree;
    platforms = platforms.all;
  };
}

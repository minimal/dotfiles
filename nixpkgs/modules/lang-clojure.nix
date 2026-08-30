{pkgs, ...}: {
  # Clojure / JVM toolchain.
  home.packages = with pkgs; [
    openjdk25
    clojure
    leiningen
    clj-kondo
    neil
    clojure-lsp
    rlwrap
  ];
}

{pkgs, ...}: {
  # Clojure / JVM toolchain.
  home.packages = with pkgs; [
    openjdk
    clojure
    leiningen
    clj-kondo
    neil
    clojure-lsp
    rlwrap
  ];
}

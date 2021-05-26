self: super:
rec {
  clojure = super.clojure.overrideAttrs (oldAttrs: {
    installPhase =
      let
        binPath = super.lib.makeBinPath [ super.rlwrap super.jdk ];
      in
      ''
        runHook preInstall
        clojure_lib_dir=$out
        bin_dir=$out/bin
        echo "Installing libs into $clojure_lib_dir"
        install -Dm644 deps.edn "$clojure_lib_dir/deps.edn"
        install -Dm644 example-deps.edn "$clojure_lib_dir/example-deps.edn"
        install -Dm644 exec.jar "$clojure_lib_dir/libexec/exec.jar"
        install -Dm644 clojure-tools-${oldAttrs.version}.jar "$clojure_lib_dir/libexec/clojure-tools-${oldAttrs.version}.jar"
        echo "Installing clojure and clj into $bin_dir"
        substituteInPlace clojure --replace PREFIX $out
        substituteInPlace clj --replace BINDIR $bin_dir
        install -Dm755 clojure "$bin_dir/clojure"
        install -Dm755 clj "$bin_dir/clj"
        wrapProgram $bin_dir/clojure --prefix PATH : $out/bin:${binPath}
        wrapProgram $bin_dir/clj --prefix PATH : $out/bin:${binPath}
        installManPage clj.1 clojure.1
        runHook postInstall
      '';
  });
}

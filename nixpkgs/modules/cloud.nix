{pkgs, ...}: {
  # Cloud / Kubernetes tooling.
  home.packages = with pkgs; [
    awscli2
    kubectl
    kubectx
    kubelogin-oidc
    krew
  ];
}

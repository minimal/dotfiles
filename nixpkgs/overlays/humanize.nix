# override to remove linux only restriction to allow pgcli install
self: super:
rec {
  python = super.python.override {
    packageOverrides = python-self: python-super: {
      humanize = python-super.humanize.overrideAttrs ( oldAttrs: {
      meta = {
        description = "Python humanize utilities";
        homepage = https://github.com/jmoiron/humanize;
        license = super.licenses.mit;
        };
      });
    };
  };
  pythonPackages = python.pkgs;
}

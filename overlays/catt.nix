final: prev: {
  catt = (prev.catt.overridePythonAttrs (drv: {
    doInstallCheck = false;
  })).override {
    PyChromecast = prev.python3Packages.PyChromecast.overridePythonAttrs (drv: rec {
      version = "6.0.0";
      src = prev.python3Packages.fetchPypi {
        inherit (drv) pname;
        inherit version;
        sha256 = "Fb6q/bFViFeURD2Z+mh6J4fYuti6RA7NoQu3K9bIyBU=";
      };
    });
  };
}

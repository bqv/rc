inputs@{...}: final: prev: {
  gitfs = prev.gitfs.overrideAttrs (drv: {
    doInstallCheck = true;
    installCheckPhase = "$out/bin/gitfs --help";
    installCheckInputs = [ final.cacert ];
    patchPhase = drv.patchPhase + ''
      sed -i 's/from pygit2.remote import RemoteCallbacks/from pygit2.callbacks import RemoteCallbacks/' gitfs/mounter.py
    '';
  });
}

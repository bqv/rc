args@{ pkgs ? import <nixpkgs> { inherit system; }
, system ? builtins.currentSystem
, stateDir ? "/var"
, runtimeDir ? "${stateDir}/run"
, logDir ? "${stateDir}/log"
, cacheDir ? "${stateDir}/cache"
, tmpDir ? (if stateDir == "/var" then "/tmp" else "${stateDir}/tmp")
, forceDisableUserChange ? false
, processManager
}: let
  inherit ((import ../.. {}).pkgs.withSources) processmgmt;
  exprFile = "${processmgmt}/examples/services-agnostic/processes.nix";
in import exprFile args

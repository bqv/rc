{ lib, pkgs }:

{
  writeFile = let
    formatted = code: code; # TODO: this
  in { name, description, lexical-binding ? true, text, ... }: pkgs.writeText "${name}.el" ''
    ;;; ${name}.el --- ${description} ${if ! lexical-binding then ""
                                        else "-*- lexical-binding: t -*-"}
    ;;; Commentary:
    ;;; Generated by Nix

    ;;; Code:
    ${formatted text}

    ;; Local Variables:
    ;; indent-tabs-mode: nil
    ;; End:
  '';
}

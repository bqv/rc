final: prev: {
  miraclecast = prev.miraclecast.overrideAttrs (super: {
    src = final.fetchFromGitHub {
      owner = "albfan";
      repo = "miraclecast";
      rev = "7eebb9ceb4bf87013da42d48aa659a515fa907f7";
      sha256 = "056474399praz9nsm2pa02gmcyvpgvr2kyw48vh3jcn2smg1j6nk";
    };
  });
}

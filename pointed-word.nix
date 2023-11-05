{ mkDerivation, base, base-unicode-symbols, composition, containers
, deepseq, lib, newtype-generics, nonempty-containers
}:
mkDerivation {
  pname = "pointed-word";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base-unicode-symbols composition containers deepseq
    newtype-generics nonempty-containers
  ];
  homepage = "https://github.com/emeinhardt/pointed-word";
  description = "A package for modeling /pointed words/: sequences with 0 or more distinguished individual elements";
  license = lib.licenses.mit;
}

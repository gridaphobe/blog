{ mkDerivation, base, blaze-html, blaze-markup, containers, css
, directory, filepath, highlighting-kate, mtl, old-locale, pandoc
, pandoc-types, scotty, stdenv, text, time, transformers, unix
, wai-extra, wai-middleware-static
}:
mkDerivation {
  pname = "blog";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base blaze-html blaze-markup containers css directory filepath
    highlighting-kate mtl old-locale pandoc pandoc-types scotty text
    time transformers unix wai-extra wai-middleware-static
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.bsd3;
}

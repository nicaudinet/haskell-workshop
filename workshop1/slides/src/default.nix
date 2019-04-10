{ stdenv
, fantasque-sans-mono
, fira-code
, fontconfig
, latinmodern-math
, lmodern
, makeFontsConf
, montserrat
, norwester-font
, pandoc
, texlive
, writeTextFile
, ncurses
, inotify-tools
}:

stdenv.mkDerivation {
  name = "slides-maker";

  buildInputs = [
    pandoc
    fontconfig
    ncurses
    (texlive.combine {
      inherit (texlive)
      scheme-small
      framed
      lm-math
      lualatex-math
      amsfonts
      amsmath
      babel
      booktabs
      ctablestack
      fancyvrb
      geometry
      hyperref
      ifluatex
      ifxetex
      latex-graphics-companion
      listings
      lm
      oberdiek
      setspace
      tools
      ulem
      xcolor
      ;
      })
  ];

  src = ./.;

  buildPhase = ''
    export HOME=$(pwd) # WTF luatex, you need a *writable* cache in HOME?
    pandoc \
      -t beamer \
      --standalone \
      -V theme:Warsaw \
      --pdf-engine=lualatex \
      --highlight-style=solarized.theme \
      --include-in-header=fonts/local.tex \
      -o slides.pdf \
      slides.md
      
  '';

  installPhase = ''
    mkdir -p $out
    cp slides.pdf $out
  '';

  shellHook = ''
    while true
    do
      pushd src
      pandoc \
        -t beamer+raw_tex \
        --standalone \
        -V theme:Warsaw \
        --pdf-engine=lualatex \
        --highlight-style=solarized.theme \
        --include-in-header=fonts/local.tex \
        -o slides.pdf \
        slides.md
      popd
      ${inotify-tools}/bin/inotifywait -e MODIFY src/slides.md
    done
  '';
}

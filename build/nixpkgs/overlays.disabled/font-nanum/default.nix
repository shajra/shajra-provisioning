final: prev:

let
    filename = "NanumFontSetup_TTF_ALL_totalsearch.exe";
in

prev.stdenv.mkDerivation rec {

    pname = "font-nanum";
    version = "1";
    name = "${pname}-${version}";

    src = prev.fetchurl {
        url = "http://appdown.naver.com/naver/font/NanumFont/setup/${filename}";
        sha256 = "0jd8qv79rnrnlhp99821kjjd2c7sl0q6p5rih3nrnl0axw7vxjv3";
    };

    builder = builtins.toFile "builder.sh"
        ''
        source "$stdenv/setup"
        mkdir -p "$out/share/fonts/truetype"
        7z x -y "$src" '$WINDIR/Fonts'
        cp '$WINDIR/Fonts'/*.ttf "$out/share/fonts/truetype"
        '';

    nativeBuildInputs = with prev; [ p7zip ];

}

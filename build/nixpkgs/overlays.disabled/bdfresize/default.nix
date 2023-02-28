self: super:

super.stdenv.mkDerivation rec {
    pname = "bdfresize";
    version = "1.5";
    name = "${pname}-${version}";
    src = super.fetchurl {
        url = "http://openlab.jp/efont/dist/tools/${pname}/${name}.tar.gz";
        sha256="19lp114yi57wzvg202lvfmck0gxa9ivs051c6m37w8x0403gq324";
    };
    patches = [ ./010_ftbfs-gcc4.patch  ./020_minus-sign.patch ];
}

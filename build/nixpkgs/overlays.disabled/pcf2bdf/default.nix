self: super:

super.stdenv.mkDerivation rec {
    pname = "pcf2bdf";
    version = "1.05";
    name = "${pname}-${version}";
    src = super.fetchFromGitHub {
        owner = "ganaware";
        repo = "${pname}";
        rev = "${version}";
        sha256="01jy5d8k25g38mk7nzfqvplsjz7a7zs7ggkg750fd9ckd4wvaa4n";
    };
    preConfigure = ''
        mv Makefile.gcc Makefile
        export PREFIX="$out"
    '';
}

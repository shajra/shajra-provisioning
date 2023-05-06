self: super:

super.lieer.overridePythonAttrs(old: rec {
    version = "1.4";
    src = super.fetchFromGitHub {
        owner = "gauteh";
        repo = "lieer";
        rev = "v${version}";
        sha256 = "sha256-2LujfvsxMHHmYjYOnLJaLdSlzDeej+ehUr4YfVe903U=";
    };
    propagatedBuildInputs = with super.python3Packages; [
        notmuch2  # changed, rest the same as for 1.3
        google-api-python-client
        oauth2client
        setuptools
        tqdm
    ];
})

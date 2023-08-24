self: super:

{
    lieer = super.lieer.overridePythonAttrs(old: {
        version = "latest";
        src = super.sources.lieer;
        propagatedBuildInputs = with super.python3Packages; [
            notmuch2
            google-api-python-client
            google-auth-oauthlib
            setuptools
            tqdm
        ];
    });
}

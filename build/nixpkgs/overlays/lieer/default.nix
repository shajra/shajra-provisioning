final: prev:

{
    lieer = prev.lieer.overridePythonAttrs(old: {
        version = "latest";
        src = prev.sources.lieer;
        propagatedBuildInputs = with prev.python3Packages; [
            notmuch2
            google-api-python-client
            google-auth-oauthlib
            setuptools
            tqdm
        ];
    });
}

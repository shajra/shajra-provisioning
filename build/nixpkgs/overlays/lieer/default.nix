_final: prev:

{
  lieer = prev.lieer.overridePythonAttrs (_old: {
    version = "latest";
    src = prev.shajra-sources.lieer;
    propagatedBuildInputs = with prev.python3Packages; [
      notmuch2
      google-api-python-client
      google-auth-oauthlib
      setuptools
      tqdm
    ];
  });
}

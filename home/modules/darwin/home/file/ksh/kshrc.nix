home:

''
fish_exe="${home}/.nix-profile/bin/fish"

if command -v "$fish_exe" > /dev/null
then exec "$fish_exe"
fi
''

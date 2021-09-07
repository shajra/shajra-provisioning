shared:

''
root = ${shared.hole.homeDirectory}
root = ssh://jelly/${shared.jelly.homeDirectory}

logfile = ${shared.hole.homeDirectory}/var/log/unison.log

path = doc/shared
ignore = Name *.sock
''

userConfig:

''
root = ${userConfig.hole.homeDirectory}
root = ssh://jelly/${userConfig.jelly.homeDirectory}

logfile = ${userConfig.hole.homeDirectory}/var/log/unison.log

path = doc/shared
ignore = Name *.sock
''

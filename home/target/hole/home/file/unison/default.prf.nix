userConfig:

''
root = ${userConfig.hole.homeDirectory}
root = ssh://cake/${userConfig.cake.homeDirectory}

logfile = ${userConfig.hole.homeDirectory}/var/log/unison.log

path = doc/shared
ignore = Name *.sock
''

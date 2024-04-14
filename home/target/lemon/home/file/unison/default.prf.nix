userConfig: hostname:

''
root = ${userConfig."${hostname}".homeDirectory}/Documents/cake
root = ssh://cake/${userConfig.hole.homeDirectory}/doc/shared

logfile = ${userConfig."${hostname}".homeDirectory}/var/log/unison.log

ignore = Name *.sock
''

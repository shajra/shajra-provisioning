userConfig: hostname:

''
root = ${userConfig."${hostname}".homeDirectory}/Documents/cake
root = ssh://cake/${userConfig.cake.homeDirectory}/doc/shared

logfile = ${userConfig."${hostname}".homeDirectory}/var/log/unison.log

ignore = Name *.sock
''

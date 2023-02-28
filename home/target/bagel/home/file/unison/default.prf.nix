userConfig: hostname:

''
root = ${userConfig."${hostname}".homeDirectory}/Documents/jelly
root = ssh://jelly/${userConfig.hole.homeDirectory}/doc/shared/safe

logfile = ${userConfig."${hostname}".homeDirectory}/var/log/unison.log

ignore = Name *.sock
''

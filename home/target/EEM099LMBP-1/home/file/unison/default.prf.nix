shared:

''
root = ${shared."EEM099LMBP-1".homeDirectory}/Documents/jelly
root = ssh://jelly/${shared.hole.homeDirectory}/doc/shared/safe

logfile = ${shared."EEM099LMBP-1".homeDirectory}/var/log/unison.log

ignore = Name *.sock
''

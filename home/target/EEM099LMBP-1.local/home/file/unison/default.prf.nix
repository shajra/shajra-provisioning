shared:

''
root = ${shared."EEM099LMBP-1.local".homeDirectory}/Documents/jelly
root = ssh://jelly/${shared.hole.homeDirectory}/doc/shared/safe

logfile = ${shared."EEM099LMBP-1.local".homeDirectory}/var/log/unison.log

ignore = Name *.sock
''

{
  nix-project-lib,
  coreutils,
  fd,
  gawk,
  git,
  gnused,
  jujutsu,
  openssh,
  starship,
  writers,
}:

let
  progName = "jj-projects";
  meta.description = "Status report for project state";
  awkScript = writers.writeText "jj-projects.awk" ''
    BEGIN {
        for (i = 3; i < ARGC; i++) {
            branches[i - 2] = ARGV[i]
        }
        ARGC = 2
    }

    function matches_all_args(line) {
        num_words = split(line, words, " ")
        for (i = 1; i in branches; i++) {
            arg_matched = 0
            for (j = 1; j <= num_words; j++) {
                if (branches[i] == words[j]) {
                    arg_matched = 1
                    break
                }
            }
            if (!arg_matched) {
                return 0
            }
        }
        return 1
    }

    {
        if ( /^Parent commit:/ ) {
            if (matches_all_args($0))
                print "\033[0;32m" $0 "\033[0m"
            else
                print "\033[0;33m" $0 "\033[0m"
        } else if ( /[Ww]orking copy/ ) {
            if ( /copy is clean/ )
                print "\033[0;32m" $0 "\033[0m"
            else if ( /copy changes/ )
                print "\033[0;33m" $0 "\033[0m"
            else print $0
        } else print $0
    }
  '';
in

nix-project-lib.writeShellCheckedExe progName
  {
    inherit meta;
    envKeep = [
      "HOME"
      "LANG"
      "LOCALE_ARCHIVE"
      "SSH_AUTH_SOCK"
    ];
    pathPackages = [
      coreutils
      fd
      gawk
      git
      gnused
      jujutsu
      openssh
      starship
    ];
  }
  ''
    set -eu
    set -o pipefail

    BRANCHES=()
    TARGETS=()

    print_usage()
    {
        cat - <<EOF
    USAGE: ${progName} [OPTION]...  [SEARCHED_DIR]...

    DESCRIPTION:

        Prints a report of found Git repositories using Jujutsu.
        The "resting" state of these repositories is expected to be
        a clean working directory with a Jujutsu commit whose
        parents are the head of all specified branches.

    OPTIONS:

        -h --help            print this help message
        -b --branch          Branch expecting pointing to parent
    EOF
    }

    main()
    {
        parse_args "$@"
        validate_args
        find_repositories | while read -r repo
        do print_report "$repo"
        done | indent
    }

    parse_args()
    {
        while ! [ "''${1:-}" = "" ]
        do
            case "$1" in
            -h|--help)
                print_usage
                exit 0
                ;;
            -b|--branch)
                if [ -z "''${2:-}" ]
                then die "$1 requires argument"
                fi
                BRANCHES+=("''${2:-}")
                shift
                ;;
            *)
                TARGETS+=("''${1:-}")
                ;;
            esac
            shift
        done
    }

    validate_args()
    {
        for target in "''${TARGETS[@]}"
        do
            if ! [ -d "$target" ]
            then die "Not a directory: $target"
            fi
        done
    }

    find_repositories()
    {
        fd --type d --hidden --no-ignore-vcs --glob .git "''${TARGETS[@]}"
    }

    print_report()
    {
        cd "$1/.."
        starship prompt | head -2
        if [ -d .jj ]
        then
            if [ "$(jj git remote list | wc -l)" -gt 0 ]
            then jj git fetch --all-remotes 2>/dev/null
            fi && jj st --no-pager \
            | awk -f "${awkScript}" - "''${BRANCHES[@]}" \
            | indent
        fi
    }

    indent()
    {
        sed -e 's/^/    /'
    }

    main "$@"
  ''

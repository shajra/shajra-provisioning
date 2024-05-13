config:

let gpgconf = "${config.programs.gpg.package}/bin/gpgconf";
in {
    interactiveShellInit = ''
        sft-update

        # DESIGN: We don't want the socket set by Ubuntu's `sshd`
        set -gx SSH_AUTH_SOCK "$("${gpgconf}" --list-dirs agent-ssh-socket)"
    '';

    functions = {
        sft-update = {
            description = "Use latest SFT socket";
            body = ''
                set --export --universal SFT_AUTH_SOCK (
                    find /var/run/sftd/client_trust_forwarding/shajra \
                    -type s -exec stat -c "%Y %n" {} \; \
                    | sort -n | tail -n 1 | awk '{print $2}'
                )
                set --erase --global SFT_AUTH_SOCK || true
            '';
        };
        groq-mr-slack = {
            description = "Notify team of merge request";
            body = ''
                argparse 'h/help' 's/send' -- $argv
                if set -ql _flag_help
                    echo "Usage: groq-mr-slack [-h|--help] [-s|--send]"
                    echo "  -h, --help: Display this help message"
                    echo "  -s, --send: Send message to Slack (otherwise dry run)"
                    return 0
                end
                set message (gr mr-get --format=short)
                if test -z "$message"
                    echo "ERROR: No message to send" >&2
                    return 1
                else
                    set message "shajra: $message"
                end
                if set -ql _flag_send
                    echo $message | slack '#infra-mr'
                else
                    echo "DRY RUN: $message"
                    echo "use --send to actually send"
                end
            '';
        };
    };

    shellAliases = {
        groq-reset =
            ''sft-update; gpg-pinentry-claim'';
        psql-prod-ro =
            ''PGPASSWORD=brakero psql --host=10.26.147.22 -U brakero brake'';
        psql-prod-rw =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-brake-brakerw) \
                psql --host=10.26.147.22 -U brakerw brake'';
        psql-prod-admin =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-brake-admin-postgres) \
                psql --host=10.26.147.22 -U postgres brake'';
        psql-test-tester =
            ''PGPASSWORD=tester psql --host=10.26.147.22 -U tester test'';
        psql-test-admin =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-brake-admin-postgres) \
                psql --host=10.26.147.22 -U postgres test'';
        psql-test2-tester =
            ''PGPASSWORD=tester psql --host=10.26.147.22 -U tester brake_test'';
        psql-load-admin =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-load-test-admin-postgres) \
                psql --host=10.26.147.35 -U postgres brake'';
        psql-load-tester =
            ''PGPASSWORD=tester psql --host=10.26.147.35 -U tester brake'';
    };
}

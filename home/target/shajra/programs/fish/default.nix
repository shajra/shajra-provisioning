{
    interactiveShellInit = ''
        sft-update
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
                echo "shajra: $(gr mr-get --format=short)" | slack '#infra-mr'
            '';
        };
    };

    shellAliases = {
        psql-brake-ro =
            ''PGPASSWORD=brakero psql --host=10.26.147.22 -U brakero brake'';
        psql-brake-rw =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-brake-brakerw) \
                psql --host=10.26.147.22 -U brakerw brake'';
        psql-brake-admin =
            ''PGPASSWORD=(berglas access gs://infra-secrets/pg-brake-admin-postgres) \
                psql --host=10.26.147.22 -U postgres brake'';
    };
}

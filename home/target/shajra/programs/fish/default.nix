{
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

GUILE_WARN_DEPRECATED=no guix shell \
    --container \
    --manifest=manifest.scm \
    --preserve='^GUILE_WARN_DEPRECATED$' \
    --preserve='^TERM$' \
    -- guile -e main gdotris.scm

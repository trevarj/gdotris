GUILE_WARN_DEPRECATED=no
LANG=en_US.utf-8

guix shell \
    --container \
    --emulate-fhs \
    --manifest=manifest.scm \
    --preserve='^GUILE_WARN_DEPRECATED$' \
    --preserve='^LANG$' \
    --preserve='^TERM$' \
    -- guile -e main gdotris.scm

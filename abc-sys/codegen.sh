#!/bin/sh
set -eu
crate_dir=$(dirname "$0")
cd "$crate_dir"

echo "cargo::rerun-if-changed=codegen.sh"

if [ -f src/generated/bindings.d -a -f src/generated/bindings.sha256 ]; then
    for file in $(cat src/generated/bindings.d); do
        openssl sha256 $file
    done | cmp src/generated/bindings.sha256 - && {
        for file in $(cat src/generated/bindings.d); do
            echo "cargo::rerun-if-changed=$file"
        done
        exit;
    }
fi


mkdir -p src/generated
bindgen \
    --allowlist-file src/bindings.h \
    --allowlist-file src/glucose2_bindings.h \
    $(find abc/src -name "*.h" | awk '{print "--allowlist-file", $1}') \
    --blocklist-item vnsprintf \
    --no-doc-comments \
    --no-recursive-allowlist \
    --experimental \
    --wrap-static-fns \
    --wrap-static-fns-path src/generated/bindings.c \
    --wrap-static-fns-suffix _imctk_abc_sys \
    --depfile src/generated/bindings.d \
    src/bindings.h \
    -- -I abc/src -D ABC_USE_STDINT_H \
    > src/generated/bindings.rs

mv src/generated/bindings.d src/generated/bindings.d.tmp
sed -Ee 's!(^[^:]*:| /[^ ]+)!!g' < src/generated/bindings.d.tmp > src/generated/bindings.d
rm src/generated/bindings.d.tmp

echo >> src/generated/bindings.d
echo "codegen.sh" >> src/generated/bindings.d

for file in $(cat src/generated/bindings.d); do
    openssl sha256 $file
done > src/generated/bindings.sha256
for file in $(cat src/generated/bindings.d); do
    echo "cargo::rerun-if-changed=$file"
done

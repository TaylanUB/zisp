#!/bin/bash

set -euo pipefail

if [ $# -eq 0 ]
then
    exec find . -name \*.md -exec "$0" {} +
fi

for file
do
    if ! [ -f "$file" ]
    then
        echo >&2 "File not found: $file"
        continue
    fi
    echo "$file"
    {
        title=$(sed '/^# / { s/# //; q }' "$file")
        sed "s/__TITLE__/$title/" prelude.html
        echo "<body>"
        markdown2 "$file" -x fenced-code-blocks,highlightjs-lang
        echo "</body>"
    } > "${file%.md}".html
done

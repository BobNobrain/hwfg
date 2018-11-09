#!/bin/bash

module_path=./src/Version.hs

v=`cat package.yaml | grep -E '^version:\s+[0-9.]+$' | grep -oE '[0-9]\.[0-9]\.[0-9]\.[0-9]'`
r=`git rev-parse HEAD`

echo "module Version"                 > "$module_path"
echo "    ( wfgVersion"              >> "$module_path"
echo "    ) where"                   >> "$module_path"
echo                                 >> "$module_path"
echo "wfgVersion = \"v${v} (${r})\"" >> "$module_path"

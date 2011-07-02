#!/bin/sh
ed README <<'EOF'
H
/Library summaries/+2,$d
$r !for f in `git ls-files *.el`; do head -1 $f | cut -b 5-; done
w
EOF

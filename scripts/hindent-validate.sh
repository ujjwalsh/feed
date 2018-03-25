#!/bin/bash
(set -o pipefail && find . -iname "*.hs" | xargs -I x hindent --validate x && echo "Everything is formatted")

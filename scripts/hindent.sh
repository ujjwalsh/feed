#!/bin/bash
(set -o pipefail && find . -iname "*.hs" | xargs -I x hindent x && echo "Everything is formatted")

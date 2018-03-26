#!/bin/bash
(set -o pipefail && find src tests -iname "*.hs" | xargs -I x hindent x && echo "Everything is formatted")

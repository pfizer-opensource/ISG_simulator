#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

export PATH="/opt/R/4.4.1/bin:$PATH"
export RSCONNECT_APPNAME="safety-pharm-design-simulater"

cd "$script_dir"
# Clear cached deployment metadata so deployApp does not pin to an older app id.
rm -rf rsconnect/documents/app.R/shinyapps.io
Rscript deploy_app.R
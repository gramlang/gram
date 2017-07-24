#!/usr/bin/env bash
set -eu -o pipefail

# This script downloads, builds, and installs Gram.

# Usage:
#   ./install.sh

echo "Installing Gram..."

rm -rf /tmp/gram
git clone https://github.com/gramlang/gram.git /tmp/gram
cd /tmp/gram && make && (make install || sudo make install)
rm -rf /tmp/gram

echo "Installation successful."
gram --version

#!/usr/bin/sh

# This script downloads, builds, and installs Gram. This is intended to be
# curled and piped into a shell. It should work with any POSIX-compatible
# shell.

# Usage:
#   ./install.sh

echo "Installing Gram..."

rm -rf /tmp/gram
git clone https://github.com/stepchowfun/gram.git /tmp/gram
cd /tmp/gram && make && (make install || sudo make install)
rm -rf /tmp/gram

echo "Installation successful."
gram --version

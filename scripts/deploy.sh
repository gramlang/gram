#!/usr/bin/env bash
set -eu -o pipefail

# This script is intended to be run on master builds in the continuous
# integration system. It does the following:
# - Upload the spec (gram.pdf) to the `static.gram.org` S3 bucket
# - Push the following Docker images:
#   - gramlang/gram:deps
#   - gramlang/gram:build
#   - gramlang/gram:latest
#
# Notes:
# - This script assumes the Docker images have already been built via the
#   appropriate make commands:
#   - make docker-gram-deps
#   - make docker-gram-build
#   - make docker-gram
# - Deploying the website (www.gram.org) happens automatically thanks to
#   GitHub Pages.

# Usage:
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   DOCKER_USERNAME=gram \
#   DOCKER_PASSWORD=KFHWNLDJGIAUEEXAMPLE \
#   ./deploy.sh

# Upload the spec
docker run \
  -e "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
  -e "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
  -e "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
  -it gramlang/gram:build aws s3 cp --acl public-read \
  /root/gram/build/common/spec/gram.pdf \
  s3://static.gram.org/gram.pdf

# Upload the Docker images
docker login -u "$DOCKER_USERNAME" -p "$DOCKER_PASSWORD"
docker push gramlang/gram:deps
docker push gramlang/gram:build
docker push gramlang/gram:latest

#!/usr/bin/env bash
set -eu -o pipefail

# This script is intended to be run in the continuous integration system.
# It does the following:
# - Upload the spec to the `static.gram.org` S3 bucket
# - Push the following Docker images:
#   - gramlang/gram:deps (optional, see SKIP_DOCKER_GRAM_DEPS_PUSH below)
#   - gramlang/gram:build
#   - gramlang/gram:latest
#
# Notes:
# - This script assumes the Docker images have already been built via the
#   appropriate make commands:
#   - make docker-gram-deps (optional, see SKIP_DOCKER_GRAM_DEPS_PUSH below)
#   - make docker-gram-build
#   - make docker-gram
# - Deploying the website (www.gram.org) happens automatically thanks to
#   GitHub Pages.

# Usage:
#   TRAVIS_BRANCH=master \
#   AWS_DEFAULT_REGION=us-east-1 \
#   AWS_ACCESS_KEY_ID=AKIAIOSFODNN7EXAMPLE \
#   AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY \
#   DOCKER_USERNAME=gram \
#   DOCKER_PASSWORD=KFHWNLDJGIAUEEXAMPLE \
#   SKIP_DOCKER_GRAM_DEPS_PUSH=YES \
#   ./deploy.sh

# Upload the spec
if [ "$TRAVIS_BRANCH" = 'master' ]; then
  S3_SPEC_DESTINATION='s3://static.gram.org/gram.pdf'
else
  S3_SPEC_DESTINATION="s3://static.gram.org/branch-$TRAVIS_BRANCH.pdf"
fi

docker run \
  -e "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
  -e "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
  -e "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
  gramlang/gram:build \
  sh -c \
    "aws s3 cp --acl public-read \
      /root/gram/build/common/spec/gram.pdf \
      $S3_SPEC_DESTINATION"

# Upload the Docker images (master branch only)
if [ "$TRAVIS_BRANCH" = 'master' ]; then
  docker login -u "$DOCKER_USERNAME" -p "$DOCKER_PASSWORD"
  if ! (echo "$SKIP_DOCKER_GRAM_DEPS_PUSH" | grep -qi '^YES$'); then
    docker push gramlang/gram:deps
  fi
  docker push gramlang/gram:build
  docker push gramlang/gram:latest
fi

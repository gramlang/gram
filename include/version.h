/*
  This header declares information about the version.
*/

#ifndef GRAM_VERSION_H
#define GRAM_VERSION_H

namespace gram {

  // The version string
  extern const char * const VERSION;

  // nullptr if not built from a clean commit
  extern const char * const COMMIT_HASH;

  // 'release' or 'debug'
  extern const char * const BUILD_TYPE;

}

#endif
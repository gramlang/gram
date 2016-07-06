/*
  This header declares information about the version.
  The corresponding source for this header is built from `../scripts/version.sh`.
*/

#ifndef GRAM_VERSION_H
#define GRAM_VERSION_H

namespace gram {

  extern const char * const VERSION;
  extern const char * const COMMIT_HASH; // NULL if not built from a clean commit
  extern const char * const BUILD_TYPE; // 'release' or 'debug'

}

#endif

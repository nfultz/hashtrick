#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>

/*
 * Adapted from example at: https://www.openssl.org/docs/crypto/EVP_DigestInit.html
 */

unsigned int digest_string(unsigned char *x, int len, SEXP key, const char *algo, unsigned char *md_value) {

  /* init openssl stuff */
  unsigned int md_len;
  const EVP_MD *md = EVP_get_digestbyname(algo);
  if(!md)
    error("Unknown cryptographic algorithm %s\n", algo);

  if(key == R_NilValue){
    EVP_Digest(x, len, md_value, &md_len, md, NULL);
  } else {
    HMAC(md, RAW(key), LENGTH(key), x, len, md_value, &md_len);
  }
  return md_len;
}




SEXP R_digest(SEXP x, SEXP mod, SEXP algo, SEXP key){
  if(!isString(x))
    error("Argument 'x' must be a character vector.");
  if(!isString(algo))
    error("Argument 'algo' must be a character vector.");
  if(!isInteger(mod))
    error("Argument 'mod' must be an integer vector.");



  int len = length(x);
  SEXP out = PROTECT(allocVector(INTSXP, len));
  int *is = INTEGER(out);

  BN_ULONG m = asInteger(mod);

  BIGNUM *val = BN_new();

  unsigned char md_value[EVP_MAX_MD_SIZE];

  for (int i = 0; i < len; i++) {
    /* check for NA */
    if(STRING_ELT(x, i) == NA_STRING) {
      SET_STRING_ELT(out, i, NA_STRING);
      continue;
    }
    /* create hash */
    const char* str = CHAR(STRING_ELT(x, i));
    int stringlen = LENGTH(STRING_ELT(x, i));
    unsigned int md_len = digest_string( (unsigned char*) str, stringlen, key, CHAR(asChar(algo)), md_value);

    BN_bin2bn(md_value, md_len, val);
    /* mod */
    is[i] = (int) BN_mod_word(val, m);

  }
  UNPROTECT(1);
  return out;
}


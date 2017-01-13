#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>




SEXP R_digest(SEXP x, SEXP mod, SEXP algo){
  const EVP_MD *md = EVP_get_digestbyname(CHAR(asChar(algo)));


  int len = length(x);
  SEXP out = PROTECT(allocVector(INTSXP, len));
  int *is = INTEGER(out);

  BN_ULONG m = asInteger(mod);

  BIGNUM *val = BN_new();

  unsigned char md_value[EVP_MAX_MD_SIZE];
  unsigned int md_len;
  for (int i = 0; i < len; i++) {
    /* check for NA */
    if(STRING_ELT(x, i) == NA_STRING) {
      is[i] = m-1;
      continue;
    }
    /* create hash */
    const char* str = CHAR(STRING_ELT(x, i));
    int stringlen = LENGTH(STRING_ELT(x, i));

    EVP_Digest((unsigned char*) str, stringlen, md_value, &md_len, md, NULL);

    BN_bin2bn(md_value, md_len, val);
    /* mod */
    is[i] = (int) BN_mod_word(val, m);

  }
  free(val);
  UNPROTECT(1);
  return out;
}

SEXP R_hmac(SEXP x, SEXP mod, SEXP algo, SEXP key){

  const EVP_MD *md = EVP_get_digestbyname(CHAR(asChar(algo)));


  int len = length(x);
  SEXP out = PROTECT(allocVector(INTSXP, len));
  int *is = INTEGER(out);

  const unsigned char* rawkey = RAW(key);
  const int keylen = LENGTH(key);



  BN_ULONG m = asInteger(mod);

  BIGNUM *val = BN_new();

  unsigned char md_value[EVP_MAX_MD_SIZE];
  unsigned int md_len;
  for (int i = 0; i < len; i++) {
    /* check for NA */
    if(STRING_ELT(x, i) == NA_STRING) {
      is[i] = m-1;
      continue;
    }
    /* create hash */
    const char* str = CHAR(STRING_ELT(x, i));
    int stringlen = LENGTH(STRING_ELT(x, i));

    HMAC(md, rawkey, keylen, (unsigned char*) str, stringlen, md_value, &md_len);

    BN_bin2bn(md_value, md_len, val);
    /* mod */
    is[i] = (int) BN_mod_word(val, m);

  }
  free(val);
  UNPROTECT(1);
  return out;
}


#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>



SEXP R_hmac(SEXP x, SEXP mod, SEXP algo, SEXP keys){

  const EVP_MD *md = EVP_get_digestbyname(CHAR(asChar(algo)));
  unsigned char md_value[EVP_MAX_MD_SIZE];
  unsigned int md_len;

  int len = length(x);
  int K = length(keys);
  SEXP out = PROTECT(allocMatrix(INTSXP, len, K));
  int *is = INTEGER(out);

  BIGNUM *val = BN_new();

  BN_ULONG m = INTEGER(mod)[0];


  for(int k = 0; k < K; k++){

    const unsigned char* rawkey = RAW(VECTOR_ELT(keys, k));
    const int keylen = LENGTH(VECTOR_ELT(keys, k));

    for (int i = 0; i < len; i++, is++) {
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
      *is = (int) BN_mod_word(val, m);
    }
  }
  free(val);

  /*/ Set dimension on output
  SEXP t = PROTECT(t = allocVector(INTSXP, 2));
  INTEGER(t)[0] = len;
  INTEGER(t)[1] = K;
  setAttrib(out, R_DimSymbol, t);
  UNPROTECT(1);*/
  UNPROTECT(1);
  return out;
}

SEXP R_mark_matrix(SEXP idx, SEXP out){

  int *dims;

  dims = INTEGER(coerceVector(getAttrib(idx, R_DimSymbol), INTSXP));
  int n = dims[0], p = dims[1];
  dims = INTEGER(coerceVector(getAttrib(idx, R_DimSymbol), INTSXP));
  int m = dims[1];

  int *x = INTEGER(out);
  int *q = INTEGER(idx);
  //Rprintf("%d %d %d\n", p, n, m);


  for(int j = 0; j < p; j++)
    for(int i = 0; i < n; i++, q++) {
      //Rprintf("%d %d %d %d\n", i, j, *q, *q * n + i);
      x[*q * m + i]++;
    }


  return(out);
}


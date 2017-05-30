#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP knmitransformer_DeterminePowerLawExponentCpp(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"knmitransformer_DeterminePowerLawExponentCpp", (DL_FUNC) &knmitransformer_DeterminePowerLawExponentCpp, 4},
  {NULL, NULL, 0}
};

void R_init_knmitransformer(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

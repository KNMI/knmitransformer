#include <Rcpp.h>
#include "float.h"
#include "math.h"

using namespace Rcpp;

double myFunc(double x, NumericVector Xm, double qfut,
              double qobs, double mfut) {

  int nx = Xm.length();

  double sum = 0;

  for(int i = 0; i < nx; i++) {
    if(Xm[i] < qobs) sum += pow(Xm[i], x);
    else sum += Xm[i] * pow(qobs, x - 1);
  }

  double result;
  result = qfut / mfut;
  result -= pow(qobs, x) / (sum / nx);
  return result;
}


// [[Rcpp::export]]
double DeterminePowLawExponentCpp(NumericVector Xm,
                                  double qfut, double qobs, double mfut)
{
  double ax = 0.1;
  double bx = 3.0;
  double tol = 0.0001;
  double a,b,c;				/* Abscissae, descr. see above	*/
  double fa;				/* f(a)				*/
  double fb;				/* f(b)				*/
  double fc;				/* f(c)				*/

  a = ax;
  b = bx;

  fa = (*myFunc)(a, Xm, qfut, qobs, mfut);
  fb = (*myFunc)(b, Xm, qfut, qobs, mfut);

  c = a;
  fc = fa;

  for(;;)		/* Main iteration loop	*/
  {
    double prev_step = b-a;		/* Distance from the last but one*/
    /* to the last approximation	*/
    double tol_act;			/* Actual tolerance		*/
    double p;      			/* Interpolation step is calcu- */
    double q;      			/* lated in the form p/q; divi- */
    /* sion operations is delayed   */
    /* until the last moment	*/
    double new_step;      		/* Step at this iteration       */

    if( fabs(fc) < fabs(fb) )
    {                         		/* Swap data for b to be the 	*/
    a = b;  b = c;  c = a;          /* best approximation		*/
    fa=fb;  fb=fc;  fc=fa;
    }
    tol_act = 2*FLT_EPSILON*fabs(b) + tol/2;
    new_step = (c-b)/2;

    if( fabs(new_step) <= tol_act || fb == (double)0 )
      return b;				/* Acceptable approx. is found	*/

    /* Decide if the interpolation can be tried	*/
    if( fabs(prev_step) >= tol_act	/* If prev_step was large enough*/
    && fabs(fa) > fabs(fb) )	/* and was in true direction,	*/
    {					/* Interpolatiom may be tried	*/
    register double t1,cb,t2;
      cb = c-b;
      if( a==c )			/* If we have only two distinct	*/
      {				/* points linear interpolation 	*/
    t1 = fb/fa;			/* can only be applied		*/
    p = cb*t1;
    q = 1.0 - t1;
      }
      else				/* Quadric inverse interpolation*/
      {
        q = fa/fc;  t1 = fb/fc;  t2 = fb/fa;
        p = t2 * ( cb*q*(q-t1) - (b-a)*(t1-1.0) );
        q = (q-1.0) * (t1-1.0) * (t2-1.0);
      }
      if( p>(double)0 )		/* p was calculated with the op-*/
    q = -q;			/* posite sign; make p positive	*/
    else				/* and assign possible minus to	*/
    p = -p;			/* q				*/

    if( p < (0.75*cb*q-fabs(tol_act*q)/2)	/* If b+p/q falls in [b,c]*/
    && p < fabs(prev_step*q/2) )	/* and isn't too large	*/
    new_step = p/q;			/* it is accepted	*/
    /* If p/q is too large then the	*/
    /* bissection procedure can 	*/
    /* reduce [b,c] range to more	*/
    /* extent			*/
    }

    if( fabs(new_step) < tol_act )	/* Adjust the step to be not less*/
    if( new_step > (double)0 )	/* than tolerance		*/
    new_step = tol_act;
    else
      new_step = -tol_act;

    a = b;  fa = fb;			/* Save the previous approx.	*/
    b += new_step;  fb = (*myFunc)(b, Xm, qfut, qobs, mfut);	/* Do step to a new approxim.	*/
    if( (fb > 0 && fc > 0) || (fb < 0 && fc < 0) )
    {                 			/* Adjust c for it to have a sign*/
    c = a;  fc = fa;                  /* opposite to that of b	*/
    }
  }

}



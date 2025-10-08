/**************************************************/
/*    matspec.c source for matSpecFunc class      */
/**************************************************/


/**************************************************/
/*            MatClass Source File                */
/*       Copyright of C. R. Birchenhall           */
/*       University of Manchester, UK.            */
/*   MatClass is freeware. This file should be    */
/* made freely available to users of any software */
/* whose creation is wholly or partly dependent   */
/*                on this file.                   */
/**************************************************/

#include "matspec.hpp"

#ifdef __cplusplus

const REAL matSpecFunc::zero  =  0.0 ;
const REAL matSpecFunc::half  =  0.5 ;
const REAL matSpecFunc::one   =  1.0 ;
const REAL matSpecFunc::two   =  2.0 ;
const REAL matSpecFunc::three =  3.0 ;
const REAL matSpecFunc::four  =  4.0 ;
const REAL matSpecFunc::five  =  5.0 ;
const REAL matSpecFunc::six   =  6.0 ;
const REAL matSpecFunc::seven =  7.0 ;
const REAL matSpecFunc::eight =  8.0 ;
const REAL matSpecFunc::nine  =  9.0 ;
const REAL matSpecFunc::ten   = 10.0 ;
const REAL matSpecFunc::root2 =  1.414213562 ;
const REAL matSpecFunc::pi    =  3.141592654 ;

matSpecFunc::matSpecFunc( void ) : matObject()
{
   error = NOERROR ;
   maxIter = 100 ;
   eps = 1.0E-7 ;
} // matSpecFunc

#else

static void matSpecInitial( void )
{
 if ( matSpecFunc::one == 1.0 )
   return ;
 matSpecFunc::zero  =  0.0 ;
 matSpecFunc::half  =  0.5 ;
 matSpecFunc::one   =  1.0 ;
 matSpecFunc::two   =  2.0 ;
 matSpecFunc::three =  3.0 ;
 matSpecFunc::four  =  4.0 ;
 matSpecFunc::five  =  5.0 ;
 matSpecFunc::six   =  6.0 ;
 matSpecFunc::seven =  7.0 ;
 matSpecFunc::eight =  8.0 ;
 matSpecFunc::nine  =  9.0 ;
 matSpecFunc::ten   = 10.0 ;
 matSpecFunc::root2 =  1.414213562 ;
 matSpecFunc::pi    =  3.141592654 ;
} // matSpecialInitial


matSpecFunc::matSpecFunc( void ) : ()
{
   error = 0 ;
   maxIter = 100 ;
   eps = 1.0E-7 ;
   matSpecInitial() ;
} // matSpecFunc

#endif

matSpecFunc::matSpecFunc( const matSpecFunc& fn )
{
   error = fn.error ;
   maxIter = fn.maxIter ;
   eps = fn.eps ;
} // matSpecFunc

matSpecFunc::~matSpecFunc( void ) {}

outFile& matSpecFunc::info( outFile& f ) M_CONST
{
    errorExit( "matSpec::info", NIMPL ) ;
    return f ;
} // matSpecFunc info

outFile& matSpecFunc::specInfo( outFile& f, const char* fName )
         M_CONST
{
   if ( matListCtrl() > 2 )
      return f ;
   objectInfo( f ) ;
   putName( "", f ) ;
   putName( fName, f ) ;
   f.newLine() ;
   return f ;
} // matSpecFunc specInfo

REAL matSpecFunc::value( REAL r )
{
   errorExit( "value", NIMPL ) ;
   return r ;
} // matSpecFunc value

REAL matSpecFunc::inv( REAL r )
{
   errorExit( "inv", NIMPL ) ;
   return r ;
} // matSpecFunc inv

REAL matSpecFunc::operator() ( REAL r )
{
   REAL v = value(r) ;
   if ( error )
      errorExit( "op()", error ) ;
   return v ;
} // matSpecFunc ()

matrix& matSpecFunc::transform( matrix& z, const matrix& x )
{
   static char *mName = "transform" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   z.reset( nr, nc ) ;
   error = NOERROR ;
   for ( j = 1 ; !error && j <= nc ; j++ )
      for ( i = 1 ; !error && i <= nr ; i++ ) {
         z(i,j) = value( x(i,j) ) ;
         if ( error ) {
            x.errorij( i, j ) ;
            errorExit( mName, error ) ;
         } // if
      } // for i
   return z ;
} // matSpecfunc transform

matrix& matSpecFunc::invTrans( matrix& z, const matrix& x )
{
   static char *mName = "invTrans" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   z.reset( nr, nc ) ;
   error = NOERROR ;
   for ( j = 1 ; !error && j <= nc ; j++ )
      for ( i = 1 ; !error && i <= nr ; i++ ) {
         z(i,j) = inv( x(i,j) ) ;
         if ( error ) {
            x.errorij( i, j ) ;
            errorExit( mName, error ) ;
         } // if
      } // for i
   return z ;
} // matSpecfunc invTrans

matrix matSpecFunc::operator () ( const matrix& x )
{
   static char *mName = "matSpecFunc" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   transform( z, x ) ;
   return z ;
} // matSpecFunc ()

matrix matSpecFunc::inv( const matrix& x )
{
   static char *mName = "matSpecFunc" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   invTrans( z, x ) ;
   return z ;
} // matSpecFunc ()

INDEX matSpecFunc::setIter( INDEX newMax )
{
   INDEX oldIter = maxIter ;
   if ( newMax > 0 )
      maxIter = newMax ;
   return oldIter ;
} // incomBetaFunc setIter

REAL matSpecFunc::setEps( REAL newEps )
{
   REAL oldEps = eps ;
   if ( newEps >= 0.0 )
     eps = newEps ;
   return oldEps ;
} // incomBeatFunc

/************************************************************/
/*                        logGamma class                    */
/************************************************************/


#ifdef __cplusplus

logGammaFunc::logGammaFunc( void ) : matSpecFunc()
{} // logGammaFunc

#else

logGammaFunc::logGammaFunc( void ) : ()
{} // logGammaFunc

#endif

logGammaFunc::~logGammaFunc( void ) {}

outFile& logGammaFunc::info( outFile& f ) M_CONST
{
   return specInfo( f, "logGam" ) ;
} // info

REAL logGammaFunc::value( REAL r )
{
   static REAL coeff[6] = { 76.18009173, -86.50532033,
                              24.01409822, -1.231739516,
                              0.120858003e-2, -0.536382e-5 } ;
   static char *mName = "logGammaFunc" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   REAL x, tmp, ser ;
   INDEX i ;
   error = NOERROR ;
   if ( r < zero ) {
      error = BDARG ;
      return zero ;
   } else if ( r == one )
      return zero ;
   else if ( r < one )
      x = one - r ;
   else // r > one
      x = r - one ;
   tmp = x + 5.5 ;
   tmp -= ( x + half ) * log( tmp ) ;
   ser = one ;
   for ( i = 0 ; i <= 5 ; i++ ) {
      x += one ;
      ser += coeff[i] / x ;
   } // for
   ser = -tmp + log( 2.50662827465 * ser ) ;
   if ( r > one )
      return ser ;
   else { // r < one
      x = one - r ;
      return log( ( pi * x ) / sin( pi * x ) ) - ser ;
   } // else
} // logGammaFunc value

logGammaFunc logGamma ;

/***********************************************************/
/*                   logBeta Function                      */
/***********************************************************/

REAL logBeta( REAL z, REAL w )
{
   static char *mName = "logBeta" ;
   matFunc func( mName ) ;
   return logGamma(z) + logGamma(w) - logGamma(z+w) ;
} // logBeta

/************************************************************/
/*                        incBeta class                    */
/************************************************************/


#ifdef __cplusplus
incBetaFunc::incBetaFunc( void ) : matSpecFunc()
#else
incBetaFunc::incBetaFunc( void ) : ()
#endif
{
   a = b = - 1.0 ;
   c = 0 ;
} // incBetaFunc

#ifdef __cplusplus
incBetaFunc::incBetaFunc( incBetaFunc& fn )
              : matSpecFunc(fn)
#else
incBetaFunc::incBetaFunc( incBetaFunc& fn )
              : ( fn )
#endif
{
   a = fn.a ;
   b = fn.b ;
   c = fn.c ;
} // incBetaFunc

void incBetaFunc::operator = ( incBetaFunc& fn )
{
   matSpecFunc::operator = ( fn ) ;
   a = fn.a ;
   b = fn.b ;
   c = fn.c ;
} // incBetaFunc

incBetaFunc::~incBetaFunc( void ) {}

outFile& incBetaFunc::info( outFile& f ) M_CONST
{
   return specInfo( f, "inBeta" ) ;
} // info

REAL incBetaFunc::fraction( REAL x, REAL a, REAL b, REAL c )
/************************************************************
   See Press et al NRC p.180

   Assumes c = logBeta( a, b )

   Calculate incomplete beta function from continued fraction

   incomplete_beta(a,b,x) = x^{a}.(1-x)^{b}.fraction /
                                   a * beta(a,b),
   where

               1    d1   d2   d3   d4
   fraction = ---  ---- ---- ---- ---- ....
               1+   1+   1+   1+   1+

   Use recurrence  fraction(n) = A(n) / B(n)

   where
         A(n) = ( s(n) * A(n-1) + r(n) * A(n-2) ) * factor
         B(n) = ( s(n) * B(n-1) + r(n) * B(n-2) ) * factor
   and
         A(-1) = 1, B(-1) = 0, A(0) = s(0), B(0) = 1.

   Here s(0) = 0 and for n >= 1 s(n) = 1, while r(1) = 1
   and subsequently, i >= 2

         r(i) =  m(b-m)x / (a+i-1)(a+i)       if i = 2m,
         r(i) = -(a+m)(a+b+m)x / (a+i-1)(a+i) if i = 2m+1,

   and factor is some scaling factor to avoid overflow.

   Hence A(0) = 0 , B(0) = 1,
         r(1) = -(a+b).x / (a+1)
         A(1) = A(0) + r(1).A(-1) = r(1) = 1
         B(1) = B(0) + r(1).B(-1) = 1
****************************************************************/
{
   REAL old_bta = zero , factor = one;
   REAL A0 = zero, A1 = one, B0 = one, B1 = one ;
   REAL bta = one, am = a, ai = a ;
   REAL m = zero, r ;
   char *mName = "fraction" ;
   matFunc func( mName ) ; debugInfo( func ) ;

   error = NOERROR ;
   if ( x < zero || x > one ) {
      error = BDARG ;
      return zero ;
   } else if ( x == zero )
      return zero ;
   else if ( x == one )
      return one ;
   else do {
      // start with i = 1, m = 0, subsequent loops i odd
      ai += one ;
      r = - am * ( am + b ) * x / ( ( ai - one ) * ai ) ;
      // two steps of recurrence replace A's & B's
      A0 = ( A1 + r * A0 ) * factor ; // i odd
      B0 = ( B1 + r * B0 ) * factor ;
      // start here with i = 2, m = 1, subsequently i even
      am += one ;
      m  += one ;
      ai += one ;
      r = m * ( b - m ) * x * factor / ( ( ai - one ) * ai ) ;
      A1 = A0 + r * A1 ; // i even, A0 & B0 already scaled
      B1 = B0 + r * B1 ;
      old_bta = bta ;
      factor = one / B1 ;
      bta = A1 * factor ;
   } while ( m < maxIter && fabs( bta - old_bta )
                               > eps * fabs( bta ) ) ;
   if ( m >= maxIter ) {
      error = NCONV ;
      return zero ;
   } // if
   return bta * exp( a * log(x) + b * log(one-x) - c ) / a ;
} // incBetaFunc fraction

REAL incBetaFunc::value( REAL r )
/**********************************************
   Approximation of Incomplete Beta, based on
   Press et al NRC p. 179.   Uses betaFraction.
   Assumes c = logBeta(a,b) ;
************************************************/
{
   static char *mName = "value" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   error = NOERROR ;
   if ( a < 0 ) {
      error = NGPAR ;
      return zero ;
   } else if ( r < zero || r > one ) {
      error = BDARG ;
      return zero ;
   } else if ( r == zero )
      return zero ;
   else if ( r == one )
      return one ;
   else if ( r < ( a + one ) / ( a + b + two ) )
      return fraction( r, a, b, c ) ;
   return one - fraction( one - r, b, a, c ) ;
} // incBetaFunc value

incBetaFunc& incBetaFunc::par( REAL newA, REAL newB )
{
   static char *mName = "par" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( newA <= zero || newB <= zero )
      errorExit( "par", NGPAR ) ;
   a = newA ;
   b = newB ;
   c = logBeta( a, b ) ;
   return *this ;
} // incBetaFunc par

matrix incBetaFunc::operator () ( const matrix& x, REAL newA, REAL newB )
{
   static char *mName = "incBeta(m)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   par( newA, newB ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z(nr,nc) ;
   transform(z,x) ;
   return z ;
} // incBeta op ()

REAL incBetaFunc::operator () ( REAL r, REAL newA, REAL newB )
{
   static char *mName = "incBeta(r)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   par( newA, newB ) ;
   REAL v = value(r) ;
   if ( error )
      errorExit( mName, error ) ;
   return v ;
} // incBeta op ()

incBetaFunc incBeta ;


/************************************************************/
/*                      incGamma class                    */
/************************************************************/


#ifdef __cplusplus
incGammaFunc::incGammaFunc( void ) : matSpecFunc()
#else
incGammaFunc::incGammaFunc( void ) : ()
#endif
{
   a = - 1.0 ;
   c = 0 ;
} // incGammaFunc

#ifdef __cplusplus
incGammaFunc::incGammaFunc( incGammaFunc& fn )
              : matSpecFunc(fn)
#else
incGammaFunc::incGammaFunc( incGammaFunc& fn )
              : ( fn )
#endif
{
   a = fn.a ;
   c = fn.c ;
} // incGammaFunc

void incGammaFunc::operator = ( incGammaFunc& fn )
{
   matSpecFunc::operator = ( fn ) ;
   a = fn.a ;
   c = fn.c ;
} // incGammaFunc

incGammaFunc::~incGammaFunc( void ) {}

outFile& incGammaFunc::info( outFile& f ) M_CONST
{
   return specInfo( f, "incGam" ) ;
} // info

REAL incGammaFunc::series( REAL r )
/*******************************************************
  return series representation incomplete gamma P(a,r).
  Assumes c = logGamma(a).
*******************************************************/
{
   static char *mName = "series" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   REAL ser, del, aplus ;
   int i ;
   error = NOERROR ;
   if ( r < zero ) {
      error = BDARG ;
      return zero ;
   } // if
   if ( r <= zero )
      return zero ;
   aplus = a ;
   del = ser = one / a ;
   i = 1 ;
   do {
      aplus += one ;
      del   *= r / aplus ;
      ser   += del ;
   } while ( ( i <= maxIter ) && ( fabs(del) >= fabs(ser) * eps ) ) ;
   if ( i >= maxIter ) {
      error= NCONV ;
      return zero ;
   } // if
   return ser *= exp( - r + a * log(r) - c ) ;
} // incGammaFunc series

REAL incGammaFunc::fraction( REAL r )
/**********************************************************
   Use continued fraction expression for complement of
   incomplete gamma function. See Press et al p.171.

   gammaComplement(a,r)=exp(-r).r^{a}.fraction/logGamma(a),

   where

               1    1-a   1   2-a   2
   fraction = ---  ----- --- ----- --- ....
               r+    1+   r+   1+   r+

   Use recurrence  fraction(n) = A(n) / B(n)

   where
         A(n) = ( s(n) * A(n-1) + r(n) * A(n-2) ) * factor
         B(n) = ( s(n) * B(n-1) + r(n) * B(n-2) ) * factor
   and
         A(-1) = 1, B(-1) = 0, A(0) = s(0), B(0) = 1.

   Here

         s(0) = 0, s(1) = r, r(0) = 0, r(1) = 1,

   so that

         A(1) = one * factor, B(1) = r * factor

   subsequently

         r(i) = k - a  if i = 2k,   k > 0
         r(i) = k      if i = 2k+1,
         s(i) = 1      if i = 2k,
         s(i) = x      if i = 2k+1

   and factor is some scaling factor to avoid overflow

   Assumes c = logGamma(a).

*************************************************************/
{
   static char *mName = "fraction" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   REAL old_gam = zero , factor = one;
   REAL A0 = zero, A1 = one, B0 = one, B1 = r ;
   REAL gam = one / r, z = zero, ma = zero - a, rfact ;
   REAL maxZ = REAL(maxIter) ;

   do {
      z  += one ;
      ma += one ;
      // two steps of recurrence replace A's & B's
      A0 = ( A1 + ma * A0 ) * factor ; // i even
      B0 = ( B1 + ma * B0 ) * factor ;
      rfact = z * factor ;
      A1 = r * A0 + rfact * A1 ; // i odd, A0 already rescaled
      B1 = r * B0 + rfact * B1 ;
      if ( B1 ) {
         factor = one / B1 ;
         old_gam = gam ;
         gam = A1 * factor ;
      } // if
   } while ( z < maxZ && fabs( gam - old_gam ) > eps * fabs( gam ) ) ;
   if ( z >= maxZ ) {
      error = NCONV ;
      return zero ;
   } // if
   return exp( -r + a * log(r) - c ) * gam ;
} // incGammaFunc::fraction

REAL incGammaFunc::value( REAL r )
/**********************************************
   Approximation of Incomplete gamma, based on
   Press et al NRC p. 172.
************************************************/
{
   static char *mName = "value" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( a < 0 ) {
      error =  NGPAR ;
      return zero ;
   } else if ( r < zero ) {
      error = BDARG ;
      return zero ;
   } else if ( r == zero )
      return zero ;
   else if ( r < ( a + one ) )
      return series( r ) ;
   return one - fraction( r ) ;
} // incGammaFunc value

incGammaFunc& incGammaFunc::par( REAL newA )
{
   static char *mName = "par" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( newA <= zero )
      errorExit( "par", NGPAR ) ;
   a = newA ;
   c = logGamma( a ) ;
   return *this ;
} // incGammaFunc par

matrix incGammaFunc::operator () ( const matrix& x, REAL newA )
{
   static char *mName = "incGamma(m)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   par( newA ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z(nr,nc) ;
   transform(z,x) ;
   return z ;
} // incGamma op ()

REAL incGammaFunc::operator () ( REAL r, REAL newA )
{
   static char *mName = "incGamma(r)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   par( newA ) ;
   REAL v = value(r) ;
   if ( error )
      errorExit( mName, error ) ;
   return v ;
} // incGamma op ()

incGammaFunc incGamma ;



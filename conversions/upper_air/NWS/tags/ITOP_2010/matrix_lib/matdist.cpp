/**************************************************/
/*  matdist.c source for distribution classes     */
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

/****************************************************/
/*        Poisson Distribtion Functions             */
/****************************************************/

REAL poissonDist( REAL k, REAL mean )
{
   static char* mName = "poissonDist(r)" ;
   matFunc func( mName ) ;
   return 1.0 - incGamma( k, mean ) ;
} // poissonDist


#ifdef __cplusplus
matrix poissonDist( const matrix& x, REAL mean )
#else
matrix poissonDistM( const matrix& x, REAL mean )
#endif
{
   static char* mName = "poissonDist(m)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z = incGamma( x, mean ) ;
   z.linear( -1.0, z, 1.0 ) ;
   return z ;
} // PoissonDist( const matrix&, REAL )


/****************************************************/
/*      Chi-squared Distribtion Functions           */
/****************************************************/

REAL chi2Dist( REAL r, INDEX dof )
{
   static char* mName = "chi2Dist(r)" ;
   matFunc func( mName ) ;
   return incGamma( 0.5 * r , 0.5 * REAL(dof)  ) ;
} // chi2Dist


#ifdef __cplusplus
matrix chi2Dist( const matrix& x, INDEX dof )
#else
matrix chi2DistM( const matrix& x, INDEX dof )
#endif
{
   static char* mName = "chi2Dist(m)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z = x ;
   z *= 0.5 ;
   incGamma.par( 0.5 * REAL(dof) ) ;
   return incGamma.transform( z, z ) ;
} // chi2Dist


/****************************************************/
/*         Student Distribtion Functions            */
/****************************************************/

REAL studentDist( REAL r, INDEX dof )
{
   static char* mName = "student(r)" ;
   matFunc func( mName ) ;
   REAL d = REAL(dof), A ;
   if ( r == 0.0 )
      return 0.5 ;
   A = 0.5 * incBeta( d / ( d + r * r ), 0.5 * d, 0.5 ) ;
   if ( r > 0.0 )
      return 1.0 - A ;
   return A ;
} // studentDist REAL

#ifdef __cplusplus
matrix studentDist( const matrix& x, INDEX dof )
#else
matrix studentDistM( const matrix& x, INDEX dof )
#endif
{
   static char* mName = "student(m)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   matrix z( nr, nc ) ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = studentDist( x(i,j), dof ) ;
   return z ;
} // studentDist matrix&


/****************************************************/
/*             F-Distribtion Functions              */
/****************************************************/

REAL FDist( REAL r, INDEX dof1, INDEX dof2 )
{
   static char* mName = "FDist" ;
   matFunc func( mName ) ;
   REAL d1 = REAL(dof1), d2 = REAL(dof2) ;
   if ( r < 0.0 )
      matErrNumExit( mName, BDARG ) ;
   return 1.0 - incBeta( d2 /( d2 + d1 * r ), 0.5 * d2, 0.5 * d1 ) ;
} // FDist

#ifdef __cplusplus
matrix FDist( const matrix& x, INDEX dof1, INDEX dof2 )
#else
matrix FDistM( const matrix& x, INDEX dof1, INDEX dof2 )
#endif
{
   static char* mName = "FDist" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   matrix z( nr, nc ) ;
   REAL r ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ ) {
         r = x(i,j) ;
         z(i,j) = FDist( r, dof1, dof2 ) ;
      } // for
   return z ;
} // FDist

/************************************************************/
/*                      normal class                    */
/************************************************************/


#ifdef __cplusplus
normalFunc::normalFunc( void ) : matSpecFunc()
#else
normalFunc::normalFunc( void ) : ()
#endif
{
   // !!! these values need more thought !!!
   lower =  8 ;
   upper = 18.66 ;
   calcUpper = 0 ;
} // normalFunc

#ifdef __cplusplus
normalFunc::normalFunc( normalFunc& fn )
              : matSpecFunc(fn)
#else
normalFunc::normalFunc( normalFunc& fn )
              : ( fn )
#endif
{
   lower = fn.lower ;
   upper = fn.upper ;
} // normalFunc

void normalFunc::operator = ( normalFunc& fn )
{
   matSpecFunc::operator = ( fn ) ;
   lower = fn.lower ;
   upper = fn.upper ;
} // normalFunc

normalFunc::~normalFunc( void ) {}

outFile& normalFunc::info( outFile& f ) M_CONST
{
   return specInfo( f, "incGam" ) ;
} // info

REAL normalFunc::value( REAL r )
/*****************************************************
   See Hill pp.126-129 in Applied Stats Algorithms
******************************************************/
{
   static REAL a[7]  = { 0.398942280444,  0.399903438504,
                           5.75885480458,  29.8213557808,
                           2.62433121679,  48.6959930692,
                           5.92885724438 } ;
   static REAL b[12] = {   0.398942280385,  3.8052E-8,
                           1.00000615302,   3.98064794E-4,
                           1.98615381364,   0.151679116635,
                           5.29330324926,   4.8385912808,
                          15.1508972451,    0.742380924027,
                          30.789933034,     3.990194417011 } ;
   static char *mName = "value" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   REAL y, z, term ;
   int isUpper = calcUpper ;
   error = NOERROR ;
   if ( r < zero ) {
      isUpper = !isUpper ;
      z = - r ;
   } else
      z = r ;
   if ( z > lower || ( isUpper && z > upper ) )
      return zero ;
   y = half * z * z ;
   if ( z <= 1.28 ) {
      term = half - z * ( a[0] - a[1] * y / ( y + a[2] - a[3] /
                         ( y + a[4] + a[5] / ( y + a[6] ) ) ) ) ;
   } else {
      term = b[0] * exp( -y ) / ( z - b[1] + b[2] / ( z + b[3] + b[4] /
                    ( z - b[5] + b[6] / ( z + b[7] -b[8] /
                    ( z + b[9] + b[10] / ( z + b[11] ) ) ) ) ) ) ;
   } // else
   if ( isUpper )
      return term ;
   // else
   return one - term ;
} // normalFunc value

REAL normalFunc::inv( REAL p )
/************************************************
   Adapted from Wichura's PPND16, Algorithm AS241
   Applied Statistics Vol 37 1988 pp 477 - 484
*************************************************/
{

   const REAL SPLIT1 = 0.425,
              SPLIT2 = 5.0,
              CONST1 = 0.180625,
              CONST2 = 1.6;

   static const REAL a[8] = {
                           3.3871328727963666080E0,
                           1.3314166789178437745E2,
                           1.9715909503065514427E3,
                           1.3731693765509461125E4,
                           4.5921953931549871457E4,
                           6.7265770927008700853E4,
                           3.3430575583588128105E4,
                           2.5090809287301226727E3
                        } ;

   static const REAL b[7] = {
                           4.2313330701600911252E1,
                           6.8718700749205790830E2,
                           5.3941960214247511077E3,
                           2.1213794301586595867E4,
                           3.9307895800092710610E4,
                           2.8729085735721942674E4,
                           5.2264952788528545610E3
                        } ;

   static const REAL c[8] = {
                           1.42343711074968357734E0,
                           4.63033784615654529590E0,
                           5.76949722146069140550E0,
                           3.64784832476320460504E0,
                           1.27045825245236838258E0,
                           2.41780725177450611770E-1,
                           2.27238449892691845833E-2,
                           7.74545014278341407640E-4
                        } ;

   static const REAL d[7] = {
                           2.05319162663775882187E0,
                           1.67638483018380384940E0,
                           6.89767334985100004550E-1,
                           1.48103976427480074590E-1,
                           1.51986665636164571966E-2,
                           5.47593808499534494600E-4,
                           1.05075007164441684324E-9
                        } ;

   static REAL e[8] = {
                           6.65790464350110377720E0,
                           5.46378491116411436990E0,
                           1.78482653991729133580E0,
                           2.96560571828504891230E-1,
                           2.65321895265761230930E-2,
                           1.24266094738807843860E-3,
                           2.71155556874348757815E-5,
                           2.01033439929228813265E-7
                        } ;

   static const REAL f[7] = {
                           5.99832206555887937690E-1,
                           1.36929880922735805310E-1,
                           1.48753612908506148525E-2,
                           7.86869131145613259100E-4,
                           1.84631831751005468180E-5,
                           1.42151175831644588870E-7,
                           2.04426310338993978564E-15
                        } ;

   REAL q = p - half, r, x ;
   error = NOERROR ;
   if ( fabs( q ) < SPLIT1 ) {
      r = CONST1 - q * q ;
      return q * ((((((( a[7] * r + a[6] ) * r + a[5] ) * r + a[4] ) * r
                        + a[3] ) * r + a[2] ) * r + a[1] ) * r + a[0] ) /
                 ((((((( b[6] * r + b[5] ) * r + b[4] ) * r + b[3] ) * r
                        + b[2] ) * r + b[1] ) * r + b[0] ) * r + one ) ;
   } else {
      if ( q < zero )
         r = p ;
      else
         r = one - p ;
      if ( r <= zero ) {
         error = BDARG ;
         return zero ;
      } // if
      r = sqrt( -log( r ) ) ;
      if ( r <= SPLIT2 ) {
         r -= CONST2 ;
         x = ((((((( c[7] * r + c[6] ) * r + c[5] ) * r + c[4] ) * r
                     + c[3] ) * r + c[2] ) * r + c[1] ) * r + c[0] ) /
               ((((((( d[6] * r + d[5] ) * r + d[4] ) * r + d[3] ) * r
                     + d[2] ) * r + d[1] ) * r + d[0] ) * r + one ) ;
      } else {
         r -= SPLIT2 ;
         x =  ((((((( e[7] * r + e[6] ) * r + e[5] ) * r + e[4] ) * r
                     + e[3] ) * r + e[2] ) * r + e[1] ) * r + e[0] ) /
               ((((((( f[6] * r + f[5] ) * r + f[4] ) * r + f[3] ) * r
                     + f[2] ) * r + f[1] ) * r + f[0] ) * r + one ) ;
      } // else
      if ( q < zero )
         x = -x ;
      return x ;
   } // else
} // normalFunc inv

matrix normalFunc::inv( const matrix& x )
{
   static char *mName = "inv" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   return matSpecFunc::inv(x) ;
} // normalFunc inv( matrix )

normalFunc& normalFunc::par( REAL newLower, REAL newUpper )
{
   static char *mName = "par" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( newLower <= zero || newUpper <= zero)
      errorExit( "par", NGPAR ) ;
   lower = newLower ;
   upper = newUpper ;
   return *this ;
} // normalFunc par

matrix normalFunc::operator () ( const matrix& x )
{
   static char *mName = "normal(m)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z(nr,nc) ;
   transform(z,x) ;
   return z ;
} // normal op ()

REAL normalFunc::operator () ( REAL r )
{
   static char *mName = "normal(r)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   return value(r) ;
} // normal op ()

normalFunc normal ;


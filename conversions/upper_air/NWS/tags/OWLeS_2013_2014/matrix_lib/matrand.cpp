/**************************************************/
/*      matrand.c source for matRandom class      */
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

#include "matrand.hpp"
#include <math.h>


static const REAL  zero = 0.0, half = 0.5, one = 1.0 , two = 2.0 ,
             pi = 3.141592654 ;

/**************************************************************/
/*                       matRandom methods                    */
/**************************************************************/

matRandom::matRandom( int newX, int newY, int newZ )
{
	seed( newX, newY, newZ ) ;
	normalOk = FALSE ;
	normal0  = 0.0 ;
} // matRandom

matRandom::matRandom( matRandom& ran )
{
	x = ran.x ;
	y = ran.y ;
	z = ran.z ;
	normalOk = ran.normalOk ;
	normal0  = ran.normal0 ;
} // matRandom copy constr

void matRandom::operator = ( matRandom& ran )
{
   x = ran.x ;
   y = ran.y ;
   z = ran.z ;
   normalOk = ran.normalOk ;
   normal0  = ran.normal0 ;
} // matRandom =

matRandom::~matRandom( void ) {} ;

void matRandom::seed( int newX, int newY, int newZ )
{
	static char *mName = "seed" ;
	matFunc func( mName ) ; debugInfo( func ) ;
	if ( newX < 1 || newX > 30000 ||
		  newY < 1 || newY > 30000 ||
		  newZ < 1 || newZ > 30000 )
      errorExit( mName, NGPAR ) ;
   x = newX ;
   y = newY ;
   z = newZ ;
} // matRandom seed

outFile& matRandom::put( outFile& f ) M_CONST
{
   f.putIndex(x).putIndex(y).putIndex(z).newLine() ;
   return f ;
} // matRandom put

inFile& matRandom::get( inFile& f )
{
   INDEX x1, y1, z1 ;
   f.getIndex(x1).getIndex(y1).getIndex(z1).nextLine() ;
	x = (INDEX)(x1) ;
   y = (INDEX)(y1) ;
	z = (INDEX)(z1) ;
   return f ;
} // matRandom get

outFile& matRandom::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 3 )
      return f ;
   objectInfo(f) ;
   putField( x, f ) ;
   putField( y, f ) ;
   putField( z, f ) ;
   f.newLine() ;
   return f ;
} // matRandom info

REAL matRandom::uniform( void )
/*
   Returns random double between 0 and 1.
   Based on Wichmann & Hill Applied Stats AS183
   1982 Vol 31 188-190.
*/
{
   // static char *mName = "uniform" ;
   // matFunc func( mName ) ; debugInfo( func ) ;
   REAL ran;

   x = 171 * ( x % 177 ) - 2 * ( x / 177 );
   if ( x < 0 )
      x += 30269;
   y = 172 * ( y % 176 ) - 35 * ( y / 176 );
   if ( y < 0 )
      y += 30307;
   z = 170 * ( z % 178 ) - 63 * ( z / 178 );
   if ( z < 0 )
      z += 30323;
	ran = REAL( x / 30269.0 ) + REAL( y / 30307.0 )
	 + REAL( z / 30323.0 ) ;
	ran -= floor( ran );
	return( ran );
} // uniform

REAL matRandom::normal( REAL mean, REAL std )
{
	// static char *mName = "normal" ;
	// matFunc func( mName ) ; debugInfo( func ) ;
	REAL normal1, normal2, v1, v2, s ;
	if ( !normalOk ) {
		do {
			v1 = 2.0 * uniform() - 1.0;
			v2 = 2.0 * uniform() - 1.0;
			s  = v1 * v1 + v2 * v2;
      } while ( s >= (REAL) 1.0 );
      normal1 = normal2 = sqrt( (REAL) -2.0 * log(s) / s );
      normal1 = v1 * normal1 ;
      normal2 = v2 * normal2 ;
      normal0 = normal2 ;
      normalOk = TRUE ;
      return mean + std * normal1 ;
   } else {
      normalOk = FALSE ;
      return mean + std * normal0 ;
   } // else
} // normal

#include "matspec.hpp"

REAL matRandom::expon( REAL mean )
{
   // static char *mName = "expon" ;
   // matFunc func( mName ) ; debugInfo( func ) ;
   return -log( uniform() ) * mean ;
} // expon

REAL matRandom::gamma( REAL a )
{
   // See Press et al pp.220-1
   static char *mName = "gamma" ;
   matFunc func( mName ) ; // debugInfo( func ) ;
   REAL gamma, v1, v2, s, t, b, ratio ;
   INDEX i ;
   if ( a < 1 ) {
      errorExit( mName, NGPAR ) ;
   } else if ( a < 6 ) {
      gamma = one ;
      for ( i = 1 ; i <= a ; i++ )
         gamma *= uniform() ;
      gamma = -log( gamma ) ;
   } else {
      do {
         do {
            do {
               v1 = two * uniform() - one ;
               v2 = two * uniform() - one ;
            } while ( v1 * v1 + v2 * v2 > one ) ;
            t = v2 / v1 ;  // ?? small v1 ??
            b = a - 1 ;
            s = sqrt( two * b + one ) ;
            gamma = s * t + b ;
         } while ( gamma <= zero ) ;
         ratio = ( one + t * t ) * exp( b * log( gamma/ b ) - s * t ) ;
      } while ( uniform() > ratio ) ;
   } // else
   return gamma ;
} // matRandom gamma


REAL matRandom::poisson( REAL mean )
{
   // static char *mName = "poisson" ;
   // matFunc func( mName ) ; debugInfo( func ) ;
   static REAL sq, logm, g, oldm = (-1.0) ;
   // matError error ;
   REAL pois, test, y ;
   if ( mean < 12.0 ) {
      if ( mean != oldm ) {
         oldm = mean ;
         g = exp( -mean ) ;
      } // if
      pois = -1 ;
      test = one ;
      do {
         pois += one ;
         test *= uniform() ;
      } while ( test > g ) ;
   } else {
      if ( mean != oldm ) {
         oldm = mean ;
         sq = sqrt( two * mean ) ;
         logm = log( mean ) ;
         g = mean * logm - logGamma( mean + one ) ;
      } // if
      do {
         do {
            y = tan( pi * uniform() ) ;
            pois = sq * y + mean ;
         } while ( pois < zero ) ;
         pois = floor( pois ) ;
         test = 0.9 * ( one + y * y ) *
               exp( pois * logm - logGamma( pois + one ) - g ) ;
      } while ( uniform() > test ) ;
   } // else
   return pois ;
} // matRandom poisson

REAL matRandom::binomial( int n, REAL p )
{
   static char *mName = "binomial" ;
   matFunc func( mName ) ; // debugInfo( func ) ;
   static REAL oldp = (-1.0), q, logp, logpc, olddn, oldg ;
   static int oldn = (-1) ;
   REAL bnl, mean, cmp, g, g1, g2, angle, prob, sq, test, y ;
   // matError error ;
   int j ;
   if ( n <= 0 )
      errorExit( mName, NGPAR ) ;
   prob = ( p <= half ? p : one - p ) ;
   mean = n * prob ;
   if ( n < 25 ) {
      bnl = zero ;
      for ( j = 0; j <= n; j++ )
         if ( uniform() < prob )
            bnl += one ;
   } else if ( mean < one ) {
      g = exp( -mean ) ;
      test = one ;
      for ( j = 0; j <= n && test >= g ; j++ )
         test *= uniform() ;
      bnl = ( j <= n ? j : n ) ;
   } else {
      if ( n != oldn ) {
         oldn = n ;
         olddn = (REAL) oldn ;
         oldg = logGamma( olddn + one ) ;
      } // if
      if ( prob != oldp ) {
         q = one - prob ;
         logp = log( prob ) ;
         logpc = log( q ) ;
         oldp = prob ;
      } // if
      sq = sqrt( two * mean * q ) ;
      do {
         do {
            angle = pi * uniform() ;
            y = tan( angle ) ;
            cmp = sq * y + mean ;
         } while ( cmp < zero || cmp >= ( olddn + one ) ) ;
         cmp = floor( cmp ) ;
         g1 = logGamma( cmp + one ) ;
         g2 = logGamma( olddn - cmp + one ) ;
         test = 1.2 * sq * ( one + y * y ) *
                exp( oldg - g1 - g2 + cmp * logp
                     + ( olddn - cmp ) * logpc ) ;
      } while ( uniform() > test ) ;
      bnl = cmp ;
   } // else
   if ( prob != p )
      bnl = n - bnl ;
   return bnl ;
} // binomial

matrix& matRandom::operator()( matrix& x, distribution dist,
                               REAL p1, REAL p2 )
{
   static char* mName = "op()" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error = NOERROR ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   int n ;
   if ( ( dist == GAMMA ) || ( dist == BINOMIAL ) )
      n = (int) p1 ;
   for ( j = 1 ; j <= nc && error == NOERROR ; j++ ) {
      for ( i = 1 ; i <= nr && error == NOERROR ; i++ ) {
         switch( dist ) {
         case UNIFORM :
            x(i,j) = (REAL) uniform() ;
            break ;
         case NORMAL :
            x(i,j) = (REAL) normal( p1, p2 ) ;
            break ;
         case EXPON :
            x(i,j) = (REAL) expon( p1 ) ;
            break ;
         case GAMMA :
            x(i,j) = (REAL) gamma( n ) ;
            break ;
         case POISSON :
            x(i,j) = (REAL) poisson( p1 ) ;
            break ;
         case BINOMIAL :
            x(i,j) = (REAL) binomial( n, p2 ) ;
            break ;
         default :
            matErrNumExit( "rand() ", NRANG ) ;
            break ;
         } // switch
      } // for j
   } // for i
   if ( error )
      errorExit( mName, error ) ;
   return x ;
} // matrandom()

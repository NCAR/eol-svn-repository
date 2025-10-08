/**************************************************/
/*        olssvd.c source for olsSvd class        */
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

#include "olssvd.hpp"
#include "matsvd.hpp"
#include <math.h>

/*************************************************************/
/*                      olsSvd methods                       */
/*************************************************************/

#ifdef __cplusplus
olsSvd::olsSvd( void ) : matOls( )
#else
olsSvd::olsSvd( void ) : ()
#endif
{
   status = 0 ;
   static char *mName = "olsSvd" ;
   matFunc func( mName ) ; 
   SV.name( "olsSV" ) ;
   U.name( "olsU" ) ;
   W.name( "olsW" ) ;
   func.trace(TRUE) ;
} // olsSvd

#ifdef __cplusplus
olsSvd::olsSvd( olsSvd& ols ) : matOls( ols )
#else
olsSvd::olsSvd( olsSvd& ols  ) : ( ols )
#endif
{} // olsSvd

void olsSvd::operator = ( olsSvd& ols ) 
{
      matOls::operator = ( ols ) ;
} // olsSvd =

#ifdef __cplusplus
olsSvd::olsSvd( const matrix& y, const matrix& x )
     : matOls( y, x )
#else
olsSvd::olsSvd( const matrix& y, const matrix& x )
     : ( y, x )
#endif
{
   static char *mName = "olsSvd(y,x)" ;
   matFunc func( mName ) ; 
   SV.name( "olsSV" ) ;
   U.name( "olsU" ) ;
   W.name( "olsW" ) ;
   func.trace(TRUE) ;
} // olsSvd( matrix& )

olsSvd::~olsSvd( void )
{
   static char *mName = "~olsSvd" ;
   matFunc func( mName ) ; debugInfo( func ) ;
} // ~olsSvd

void olsSvd::initial( void )
{
   matOls::initial() ;
   SV.reset( nVars ) ;
   U.reset( nObs, nVars ) ;
   W.reset( nVars, nVars ) ;
} // olsSvd::initial

void olsSvd::clear( void )
{
   matOls::clear() ;
   SV.clear() ;
   U.clear() ;
   W.clear() ;
} // olsSvd::clear

void olsSvd::zeroSV( void )
{
   static char *mName = "zero" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX  i, n = SV.nRows() ;
   REAL   svLimit ;
   for ( svLimit = SV(1), i = 2 ; i <= n ; i++ ) {
      if ( SV(i) > svLimit )
         svLimit = SV(i) ;
   } // for
   svLimit *= tol ;
   for ( i = SV.nRows() ; i ; i-- ) {
      if ( SV(i) < svLimit )
	 SV(i) = 0.0 ;
   } // for
} // olsSvd zero

void olsSvd::formBeta( void )
{
   static char *mName = "formBeta" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   refMatrix yRef( Y ), bRef( beta ) ;
   INDEX nc = Y.nCols(), j ;
   for ( j = 1 ; j <= nc ; j++ ) {
       yRef.refCol(j) ;
       bRef.refCol(j) ;
       svdBackSub( U, SV, W, yRef, bRef, error ) ;
       if ( error )
	  errorExit( mName, error ) ;
   } // for 
} // olsSvd formBeta

void olsSvd::decompose( void )
{
   static char *mName = "decomp" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      U = X ;
      if ( !svdcmp( U, SV, W, error ) )
         errorExit( mName, error ) ;
      zeroSV() ;
      formBeta() ;
      status |= DECOMPOSED ;
   } // if
} // olsSvd::decompose

matrix& olsSvd::sv( matrix& s )
{
   static char *mName = "sv" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   decompose() ;
   s = SV ;
   return s ;
} // olsSvd sv

void olsSvd::setSV( const matrix& newSV )
{
   static char *mName = "sv" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   decompose() ;
   SV = newSV ;
   formBeta() ;
   formV() ;
} // olsSvd setSV

outFile& olsSvd::info( outFile& f ) M_CONST
{
   return matOls::olsInfo( f, "olsSvd" ) ;
} // olsSvd::info

REAL olsSvd::varAdd( const matrix& z, INDEX i )
/***************************************************
  Return modified RSS when vars z added to nth eq.
***************************************************/
{
   static char *mName = "addVar" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   z.errorExit( mName, NIMPL ) ;
   return (REAL) i ;
} // olsSvd::varAdd


REAL olsSvd::cond( void )
/************************************
   Condition estimate based on SVD
************************************/
{
   static char *mName = "cond" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   decompose() ;
   if ( !(status & CONDITIONED ) ) {
      REAL min, max ;
      min = max = SV(1) ;
      for ( INDEX i = 2; i <= nVars ; i++ ) {
         if ( SV(i) < min )
            min = SV(i) ;
         if ( SV(i) > max )
            max = SV(i) ;
      } // for i
      if ( min < tol )
         condition = -1 ;
      else
         condition = max/min ;
   } // if 
   return condition ;
} // olsSvd::cond


void olsSvd::formV( void )
{
   static char *mName = "formV" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   if ( !(status & VMATRIX ) ) {
      INDEX n = nVars, i ;
      REAL r ;
      matrix d(n) ;
      for ( i = 1 ; i <= n ; i++ ) {
          r = SV(i) ;
          if ( r!= 0.0 )
             d(i) = 1.0 / ( r * r ) ;
          else
             d(i) = 0.0 ;
      } // for
      // R = W * diag(d) ;
      R.colMultOf( W, d ) ;
      // V = R * W' = W * diag(d) * W'
      V.multTOf( W, R ) ;
      for ( i = 1 ; i <= n ; i++ )
          VSqrt(i) = sqrt( V(i,i) ) ;
      status |= VMATRIX ;
   } // if
} // olsSvd::formV

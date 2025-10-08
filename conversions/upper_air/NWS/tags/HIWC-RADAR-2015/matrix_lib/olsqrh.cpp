/**************************************************/
/*        olsqrh.c source for olsQrh class        */
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

#include "olsqrh.hpp"
#include "matqrh.hpp"

/*************************************************************/
/*                      olsQrh methods                       */
/*************************************************************/

void olsQrh::setNames( void )
{
   U.name( "olsU" ) ;
   Q.name( " olsQ" ) ;
   w.name( "olsW" ) ;
} // olsprh setNames

#ifdef __cplusplus
olsQrh::olsQrh( void ) : matOls( )
#else
olsQrh::olsQrh( void ) : ()
#endif
{
   static char *mName = "olsQrh" ;
   matFunc func( mName ) ; 
   status = 0 ;
   setNames() ;
   func.trace(TRUE) ;
} // olsQrh

#ifdef __cplusplus
olsQrh::olsQrh( olsQrh& ols ) : matOls( ols )
#else
olsQrh::olsQrh( olsQrh& ols  ) : ( ols )
#endif
{} // olsQrh

void olsQrh::operator = ( olsQrh& ols )
{
      matOls::operator = ( ols ) ;
} // olsQrh =

#ifdef __cplusplus
olsQrh::olsQrh( const matrix& y, const matrix& x )
     : matOls( y, x )
#else
olsQrh::olsQrh( const matrix& y, const matrix& x )
     : ( y, x )
#endif
{
   static char *mName = "olsQrh(y,x)" ;
   matFunc func( mName ) ; 
   setNames() ;
   func.trace(TRUE) ;
} // olsQrh( matrix& )

olsQrh::~olsQrh( void )
{
   static char *mName = "~olsQrh" ;
   matFunc func( mName ) ; debugInfo( func ) ;
} // ~olsQrh

void olsQrh::initial( void )
{
   matOls::initial() ;
   U.reset( nObs, nDep ) ;
   Q.reset( nObs, nVars ) ;
   w.reset( nVars ) ;
} // olsQrh::initial

void olsQrh::clear( void )
{
   matOls::clear() ;
   U.clear() ;
   Q.clear() ;
   w.clear() ;
} // olsQrh::clear

void olsQrh::decompose( void )
{
   static char *mName = "decomp" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      U = Y ;
      Q = X ;
      // QR decomposition and formation of OLS coeffs
      // leaving U = Q'Y, projection of Y onto X-space
      if ( hProject( U, Q, R, w, beta, tol, error ) ) {
         // generate R inverse
         triuInv( R, Rinv, tol, error ) ;
      } else
         status |= SINGULAR ;
      status |= DECOMPOSED ;
   } // if
} // olsQrh::decompose

outFile& olsQrh::info( outFile& f ) M_CONST
{
   return matOls::olsInfo( f, "olsQrh" ) ;
} // olsQrh::info

REAL olsQrh::varAdd( const matrix& z, INDEX i )
/***************************************************
  Return modified RSS when vars z added to nth eq.
***************************************************/
{
   static char *mName = "addVar" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   matrix Z, u, q, dg, b ;
   INDEX nc = z.nCols() ;
   REAL s ;
   if ( nc >= dof )
      errorExit( mName, GTDIM ) ;
   // form Q'z
   Z = z ;
   if ( !qReflect( Z, Q, w, error ) )
      errorExit( mName, error ) ;
   // decompose lower Z into qr form
   q = Z.sub( nVars+1, nObs, 1, nc ) ;
   if ( !qrh( q, dg, b, tol, error ) )
      errorExit( mName, error ) ;
   // u = copy of original LUS residuals
   u = U.sub( nVars+1, nObs, i, i ) ;
   // project u onto residual space of new vars
   if ( !qReflect( u, q, b, error ) )
      errorExit( mName, error ) ;
   // estimate new rss from new LUS residuals in u
   s = u.smpl( nc + 1, u.nRows() ).sumsq() ;
   return s ;
} // olsQrh::varAdd

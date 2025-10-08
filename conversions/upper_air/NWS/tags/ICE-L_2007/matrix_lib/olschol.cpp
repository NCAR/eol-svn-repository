/**************************************************/
/*      olschol.c source for olsChol class        */
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

#include "olschol.hpp"
#include "matchol.hpp"

/*************************************************************/
/*                      olsChol methods                       */
/*************************************************************/

void olsChol::setNames( void )
{
   XTX.name( "olsXTX" ) ;
   XTY.name( "olsXTY" ) ;
} // olsChol setNames

#ifdef __cplusplus
olsChol::olsChol( void ) : matOls( )
#else
olsChol::olsChol( void ) : ()
#endif
{
   static char *mName = "olsChol" ;
   matFunc func( mName ) ; 
   status = 0 ;
   setNames() ;
   func.trace(TRUE) ;
} // olsChol

#ifdef __cplusplus
olsChol::olsChol( olsChol& ols ) : matOls( ols )
#else
olsChol::olsChol( olsChol& ols ) : ( ols )
#endif
{} // olsChol

void olsChol::operator = ( olsChol& ols )
{
      matOls::operator = ( ols ) ;
} // olsChol =

#ifdef __cplusplus
olsChol::olsChol( const matrix& y, const matrix& x )
     : matOls( y, x )
#else
olsChol::olsChol( const matrix& y, const matrix& x )
     : ( y, x )
#endif
{
   static char *mName = "olsChol(y,x)" ;
   matFunc func( mName ) ; 
   setNames() ;
   func.trace(TRUE) ;
} // olsChol( matrix& )

olsChol::~olsChol( void )
{
   static char *mName = "~olsChol" ;
   matFunc func( mName ) ; debugInfo( func ) ;
} // ~olsChol

void olsChol::initial( void )
{
   matOls::initial() ;
   XTX.reset( nVars, nVars ) ;
   XTY.reset( nVars, nDep ) ;
} // olsChol::initial

void olsChol::clear( void )
{
   matOls::clear() ;
   XTX.clear() ;
   XTY.clear() ;
} // olsChol::clear

void olsChol::decompose( void )
{
   static char *mName = "decomp" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      XTX.TMultOf( X, X ) ;
      XTY.TMultOf( X, Y ) ;
      if ( !cholesky( XTX, error ) )
	 status |= SINGULAR ;
      else {
         // Copy transposed cholesky factor into R
         R.triuOf( XTX.trans() ) ;
	 triuInv( R, Rinv, tol, error ) ;
	 beta = XTY ;
	 cholSolve( XTX, beta, tol, error ) ;
      } // else
      status |= DECOMPOSED ;
   } // if
} // olsChol::decompose

outFile& olsChol::info( outFile& f ) M_CONST
{
   return matOls::olsInfo( f, "olsChol" ) ;
} // olsChol::info

REAL olsChol::varAdd( const matrix& z, INDEX i )
/***************************************************
  Return modified RSS when vars z added to nth eq.
***************************************************/
{
   static char *mName = "addVar" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   z.errorExit( mName, NIMPL ) ;
   return (REAL) i ;
} // olsChol::varAdd


/**************************************************/
/*     choldec.c source for cholDec class         */
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

#include "choldec.hpp"

/***************************************************/
/*                  cholDec methods                */
/***************************************************/

#ifdef __cplusplus
cholDec::cholDec( void ) : matDec()
#else
cholDec::cholDec( void ) : ()
#endif
{} // cholDec( void )


#ifdef __cplusplus
cholDec::cholDec( const matrix& x ) : matDec(x)
#else
cholDec::cholDec( const matrix& x ) : (x)
#endif
{} // cholDec( matrix )


#ifdef __cplusplus
cholDec::cholDec( const cholDec& cd ) : matDec( cd )
#else
cholDec::cholDec( const cholDec& cd ) : ( cd )
#endif
{} // cholDec( cholDec& )

cholDec::~cholDec( void ) {} // ~cholDec

cholDec& cholDec::operator = ( const cholDec& cd )
{
   matDec::operator = ( cd ) ;
   return *this ;
} // cholDec& =

void cholDec::chol( matrix& x )
{
   x = m ;
} // cholDec::chol

void cholDec::release( matrix& x )
{
   x.refer( m ) ;
   clear() ;
} // cholDec::chol

void cholDec::decompose( void )
{
   static char *mName = "decompose" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      cholesky( m, error ) ;
      status |= DECOMPOSED ;
   } // if
} // cholDec::decompose

void cholDec::solve( matrix& b )
{
   static char *mName = "solve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      cholSolve( m, b, tol, error ) ;
      if ( error )
         errorExit( mName, error ) ;
   } // else
} // cholDec::solve

void cholDec::transSolve( matrix& b )
{
   solve(b) ;
} // cholDec::transSolve

void cholDec::det( REAL& d1, REAL& d2 )
{
   // determinant is square of diagProd of cholesky factor
   static char *mName = "det" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & DETERMINED ) ) {
      matDec::det(d1,d2) ;
      // square det as calculated by above
      det1 *= det1 ;
      det2 += det2 ;
   } // if
   d1 = det1 ;
   d2 = det2 ;
} // cholDec::det

outFile& cholDec::info( outFile& f ) M_CONST
{
   return matDec::decInfo( f, "cholDec" ) ;
} // cholDec::info

outFile& cholDec::print( char* decName, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "Cholesky Decomposition : " ) ;
   if ( decName != 0 )
      f.write( decName ) ;
   f.newLine(2) ;
   return matDec::print( "cholDec", f ) ;
} // cholDec::print

/**************************************************/
/*     ludec.c source for luDec class         */
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

#include "ludec.hpp"

/****************************************************/
/*                  luDec methods                   */
/****************************************************/


#ifdef __cplusplus
luDec::luDec( void ) : matDec()
#else
luDec::luDec( void ) : ()
#endif
{
   indexp.name( "decInd" ) ;
} // luDec


#ifdef __cplusplus
luDec::luDec( luDec& lu ) : matDec(lu)
#else
luDec::luDec( luDec& lu ) : (lu)
#endif
{
   indexp = lu.indexp ;
   sign = lu.sign ;
} // luDec

#ifdef __cplusplus
luDec::luDec( const matrix& x ) : matDec(x)
#else
luDec::luDec( const matrix& x ) : (x)
#endif
{
   static char *mName = "luDec(x)" ;
   matFunc func( mName ) ;
   indexp.reset( x.nRows() ) ;
   indexp.name( "decInd" ) ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // luDec( matrix& )

luDec::~luDec( void )
{
   static char *mName = "~luDec" ;
   matFunc func( mName ) ; debugInfo( func ) ;
} // ~luDec

void luDec::assign( const matrix& x )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::assign(x) ;
   indexp.reset( x.nRows() ) ;
} // luDec::assign

void luDec::operator = ( const luDec& lu )
{
   static char *mName = "op = lu&" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::operator = ( lu ) ;
   indexp.reset( lu.m.nRows() ) ;
   sign = lu.sign ;
} // luDec = lu&

void luDec::capture( matrix& x )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::capture(x) ;
   indexp.reset( x.nRows() ) ;
} // luDec::capture

void luDec::clear( void )
{
   static char* mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::clear() ;
   indexp.clear() ;
   sign = 0.0 ;
} // clear

void luDec::decompose( void )
{
   static char *mName = "decompose" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      crout( m, indexp, sign, tol, error ) ;
      status |= DECOMPOSED ;
   } // if
} // luDec::decompose

void luDec::lu( matrix& x, indexArray& index )
{
   static char *mName = "lu" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) { 
      x = m ;
      index = indexp ;
   } // if
   return ;
} // luDec::lu

void luDec::release( matrix& x, indexArray& index )
{
   static char *mName = "release" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) { 
      x.refer(m) ;
      index = indexp ;
      clear() ;
   } // if
   return ;
} // luDec::release

void luDec::solve( matrix& b )
{
   static char *mName = "solve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      luSolve( m, indexp, b, tol, error ) ;
      if ( error )
         errorExit( mName, error ) ;
   } // else
} // luDec::solve

void luDec::transSolve( matrix& b )
{
   static char *mName = "transSolve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      lutSolve( m, indexp, b, tol, error ) ;
      if ( error )
	 errorExit( mName, error ) ;
   } // else
} // luDec::transSolve

void luDec::det( REAL& d1, REAL& d2 )
{
   static char *mName = "det" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & DETERMINED ) ) {
      matDec::det(d1,d2) ;
      // sign det as calculated by above
      det1 *= sign ;
   } // if
   d1 = det1 ;
   d2 = det2 ;
} // luDec::det

outFile& luDec::info( outFile& f ) M_CONST
{
   return matDec::decInfo( f, "luDec" ) ;
} // luDec::info

outFile& luDec::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::put(f) ;
   indexp.put(f) ;
   f.putReal( sign ).newLine() ;
   return f ;
} // luDec::put

outFile& luDec::print( char* decName, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "LU decomposition : " ) ;
   if ( decName != 0 )
      f.write( decName ) ;
   f.newLine(2) ;
   matDec::print( "LU", f ) ;
   indexp.print( "Pivot Map", f ) ;
   f.write( "Sign : " ).writeReal( sign ).newLine(2) ;
   return f ;
} // luDec::print

inFile& luDec::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::get(f) ;
   indexp.reset( m.nRows() ) ;
   indexp.get(f) ;
   f.getReal( sign ) ;
   return f ;
} // luDec::get

matrix squareEqn( const matrix& x, const matrix& b )
{
   static char *mName = "squareEqn" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( b.nRows(), b.nCols() ) ;
   z = b ;
   luDec xlu( x ) ;
   xlu.multiSolve( z ) ;
   return z ;
} // squareEqn

matrix operator / ( const matrix& b, const matrix& x )
{
   static char *mName = "matrix op /" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   if ( x.nRows() == x.nCols() )
      return squareEqn( x, b ) ;
   else
      return normalEqn( x, b ) ;
} // matrix "projection" /

matrix matrix::inv( void )
{
   static char *mName = "inv" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z( nRows(), nCols() ) ;
   luDec xlu( *this ) ;
   xlu.inverse( z ) ;
   return z ;
} // matrix inv




/**************************************************/
/*        svddec.c source for svdDec class        */
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

#include "svddec.hpp"

/*******************************************************/
/*                    svdDec methods                   */
/*******************************************************/

void svdDec::setNames( void )
{
   S.name( "decSV" ) ;
   V.name( "decV" ) ;
} // svdDec setNames

#ifdef __cplusplus
svdDec::svdDec( void ) : matDec()
#else
svdDec::svdDec( void ) : ()
#endif
{
   setNames() ;
} // svdDec( void )


#ifdef __cplusplus
svdDec::svdDec( const matrix& x ) : matDec(x)
#else
svdDec::svdDec( const matrix& x ) : (x)
#endif
{
   INDEX nc = x.nCols() ;
   S.reset( nc ) ;
   V.reset( nc, nc ) ;
   setNames() ;
} // svdDec( matrix )

#ifdef __cplusplus
svdDec::svdDec( const svdDec& sd ) : matDec( sd )
#else
svdDec::svdDec( const svdDec& sd ) : ( sd )
#endif
{
   S = sd.S ;
   V = sd.V ;
} // svdDec( svdDec& )

void svdDec::operator = ( const svdDec& sd )
{
   static char *mName = "op = &" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::operator = (sd) ;
   S = sd.S ;
   V = sd.V ;
} // svdDec = svdDec

svdDec::~svdDec( void ) {} // ~svdDec

void svdDec::svd( matrix& u, matrix& sv, matrix& v )
{
   static char *mName = "svd" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      u  = m ;
      sv = S ;
      v  = V ;
   } //if
} // svdDec svd

void svdDec::release( matrix& u, matrix& sv, matrix& v )
{
   static char *mName = "release" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      u.capture( m ) ;
      sv.capture( S ) ;
      v.capture( V ) ;
      clear() ;
   } //if
} // svdDec svd

void svdDec::clear( void )
{
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::clear() ;
   S.clear() ;
   V.clear() ;   
} // svdDec clear

void svdDec::assign( const matrix& x )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::assign( x ) ;
   INDEX nc = x.nCols() ;
   S.reset( nc ) ;
   V.reset( nc, nc ) ;
} // svdDec assign

void svdDec::capture( matrix& x )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::capture( x ) ;
   INDEX nc = m.nCols() ;
   S.reset( nc ) ;
   V.reset( nc, nc ) ;
} // svdDec capture 

outFile& svdDec::info( outFile& f ) M_CONST
{
   return matDec::decInfo( f, "svdDec" ) ;
} // qrhDec::info

inFile& svdDec::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::get(f) ;
   INDEX n = m.nCols() ;
   S.reset( n ) ;
   V.reset( n, n ) ;
   S.get(f) ;
   V.get(f) ;
   return f ;
} // svdDec::get

outFile& svdDec::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::put(f) ;
   S.put(f) ;
   V.put(f) ;
   return f ;
} // svdDec::put

outFile& svdDec::print( char* decName, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "SVD Decomposition : " ) ;
   if ( decName != 0 )
      f.write( decName ) ;
   f.newLine(2) ;
   matDec::print( "`U'" , f ) ;
   S.print( "Singular Values : ", f ) ;
   V.print( "`V' : ", f ) ;
   return f ;
} // svdDec::print

void svdDec::decompose( void )
{
   static char *mName = "decompose" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matError error ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      if ( !svdcmp( m, S, V, error ) )
         errorExit( mName, error ) ;
      status |= DECOMPOSED ;
   } // if
} // svdDec::decompose

void svdDec::zero( REAL min )
{
   static char *mName = "zero" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = S.nRows(), i ;
   if ( min <= 0.0 )
      min = tol ;
   if ( ok( mName ) ) {
      for ( i = 1 ; i <= n ; i++ ) {
         if ( S(i) < min )
	    S(i) = 0.0 ;
      } // for
   } // if
} // svdDec:zero

void svdDec::sv( matrix& s )
{
   static char *mName = "sv" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) 
      s = S ;
} // svdDec sv

void svdDec::setSV( matrix& s )
{
   static char *mName = "setSV" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( S.nRows() != s.nRows() )
      errorExit( mName, NEDIM ) ;
   S = s ;
} // svdDec sv

void svdDec::solve( matrix& b )
{
   static char *mName = "solve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      svdBackSub( m, S, V, b, b, error ) ;
      if ( error )
	 errorExit( mName, error ) ;
   } // else
} // svdDec::solve

void svdDec::transSolve( matrix& b )
{
   static char *mName = "transSolve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      svdBackSub( V, S, m, b, b, error ) ;
      if ( error )
	 errorExit( mName, error ) ;
   } // else
} // svdDec::transSolve

REAL svdDec::cond( void )
{
   static char *mName = "cond" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      INDEX n = S.nRows() ;
      REAL max = S(1) ;
      REAL min = S(n) ;
      if ( min == 0.0 )
         condition = -1 ;
      else
         condition = max / min ;              
      status |= CONDITION ;
   } // if
   return condition ;
} // matDec::cond

INDEX svdDec::rank( void )
{
   static char *mName = "rank" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX rankEst = 0 ;
   if ( ok( mName ) ) {
      for ( INDEX n = S.nRows() ; n ; n-- ) {
         if ( S(n) > 0.0 )
            rankEst++ ;
      } // for
   } // if
   return rankEst ;
} // svdDec rank

void svdDec::det( REAL& d1, REAL& d2 )
{
   static char *mName = "det" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      if ( S( S.nRows() ) > 0 ) {
         dProduct( S, det1, det2, 0.0, error ) ;
         if ( error )
            errorExit( mName, error ) ;
      } else {
         det1 = 0.0 ;
         det2 = 0.0 ;
      } // else
      status |= DETERMINED ;
   } // if
   d1 = det1 ;
   d2 = det2 ;   
} // svdDec::det

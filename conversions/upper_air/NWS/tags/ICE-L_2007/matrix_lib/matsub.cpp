/**************************************************/
/*     matsub.c source for submatrix functions    */
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

#include "matrix.hpp"


/*********************************************************/
/*   reference subMatrix methods and functions           */
/*********************************************************/


void matrix::setSub( INDEX r1, INDEX r2, INDEX c1, INDEX c2,
                     const matrix& y )
{
   static char *mName = "setSub" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( r1 < 1 || r1 > r2 || r2 > nRows() ||
        c1 < 1 || c1 > c2 || c2 > nCols() )
      errorExit( mName, NRANG ) ;
   INDEX nr = r2 - r1 + 1 , nc = c2 - c1 + 1 ;
   if ( nr != y.nRows() || nc != y.nCols() )
      errorExit( mName, NEDIM ) ;
   INDEX i, j, p, q ;
   for ( j = 1, q = c1 ; j <= nc ; j++, q++ )
      for ( i = 1, p = r1 ; i <= nr ; i++, p++ )
	 mat(p,q) = y(i,j) ; 
} // matrix::setSub

void matrix::setRow( INDEX i, const matrix& y )
{
   static char *mName = "setRow" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   setSub( i, i, 1, nCols(), y ) ;
} // matrix::setRow

void matrix::setCol( INDEX j, const matrix& y )
{
   static char *mName = "setCol" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   setSub( 1, nRows(), j, j, y ) ;
} // matrix::setCol

void matrix::refer( const matrix& x )
{
   static char *mName = "refer" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pm != 0 && --pm->ref == 0 )
      delete pm ;
   pm = x.pm ;
   pm->ref++ ;
} // matrix:refer

matrix matrix::sub( INDEX r1, INDEX r2, 
                       INDEX c1, INDEX c2  ) M_CONST
{
   static char *mName = "sub" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ; 
   z.pm = new matMap( *pm, r1, r2, c1, c2 ) ;
   return z ;
} // matrix::sub

matrix matrix::row( INDEX r1, INDEX r2 ) M_CONST
{
   static char *mName = "row" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( r2 == 0 )
      r2 = r1 ;
   return sub( r1, r2, 1, nCols() ) ;
} // matrix::row

matrix matrix::col( INDEX c1, INDEX c2 ) M_CONST
{
   static char *mName = "col" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( c2 == 0 )
      c2 = c1 ;
   return sub( 1, nRows(), c1, c2 ) ;
} // matrix::col


/*********************************************************/
/*        non-reference methods and functions            */
/*********************************************************/


void matrix::setDiag( matrix& dg, int k )
{
   static char *mName = "setDiag" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   int nr = nRows(), nc = nCols(), nd, offc, offr ;
   if ( dg.nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   if ( ( k > nc - 1 ) || ( nr + k - 1 < 0 ) )
      errorExit( mName, NRANG ) ;
   offr = k < 0 ? -k : 0 ;
   offc = k > 0 ?  k : 0 ;
   nd = (INDEX) ( nr + k <= nc ? nr - offr : nc - offc ) ;
   if ( nd != dg.nRows() )
      errorExit( mName, NEDIM ) ;
   for ( int j = 1 ; j <= nd ; j++ )
      mat( offr + j, offc + j ) = dg(j) ;
} // matrix::setDiag()

void matrix::setDiag( REAL r, int k )
{
   static char *mName = "setDiag(r)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   int nr = nRows(), nc = nCols(), nd, offc, offr ;
   if ( ( k > nc - 1 ) || ( nr + k - 1 < 0 ) )
      errorExit( mName, NRANG ) ;
   offr = k < 0 ? -k : 0 ;
   offc = k > 0 ?  k : 0 ;
   nd = (INDEX) ( nr + k <= nc ? nr - offr : nc - offc ) ;
   for ( int j = 1 ; j <= nd ; j++ )
      mat( offr + j, offc + j ) = r ;
} // matrix::setDiag( REAL )

matrix& matrix::diagOf( const matrix& x, int k )
{
   static char *mName = "diagOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   int nr = x.nRows(), nc = x.nCols(), nd, offr, offc ;
   if ( ( k > nc - 1 ) || ( nr + k - 1 < 0 ) )
      errorExit( mName, NRANG ) ;
   offr = k < 0 ? -k : 0 ;
   offc = k > 0 ?  k : 0 ;
   nd = (INDEX) ( nr + k <= nc ? nr - offr : nc - offc ) ;
   reset( nd ) ;
   for ( int j = 1 ; j <= nd ; j++ )
      mat(j) = x( offr + j, offc + j ) ;
   return *this ;
} // matrix::diagOf

matrix matrix::diag( int k ) M_CONST
{
   static char *mName = "diag" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.diagOf( *this, k ) ;
   return z ;
} // matrix::diag

matrix& matrix::subOf( const matrix& x, INDEX r1, INDEX r2, 
                         INDEX c1, INDEX c2 ) 
{
   static char *mName = "subOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( r1 < 1 || r1 > r2 || r2 > x.nRows() || 
        c1 < 1 || c1 > c2 || c2 > x.nCols() )
      errorExit( mName, NRANG ) ;
   INDEX nr = r2 - r1 + 1, nc = c2 - c1 + 1, i, j, p, q ;
   reset( nr, nc ) ;
   for ( j = 1, q = c1 ; j <= nc ; j++, q++ )
      for ( i = 1, p = r1 ; i <= nr ; i++, p++ )
         mat(i,j) = x( p, q ) ;
   return *this ;
} // matrix::subOf

matrix& matrix::rowOf( const matrix& x, INDEX i, INDEX j )
{
   static char *mName = "rowOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( j == 0 )
      j = i ;
   return subOf( x, i, j, 1, x.nCols() ) ;
} // matrix::rowOf

matrix& matrix::colOf( const matrix& x, INDEX i, INDEX j )
{
   static char *mName = "colOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( j == 0 )
      j = i ;
   return subOf( x, 1, x.nRows(), i, j ) ;
} // matrix::colOf

matrix& matrix::triuOf( const matrix& x, int k )
{
   static char *mName = "triuOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   reset( nr, nc ) ;
   int h ;
   for ( i = 1 ; i <= nr ; i++ ) {
      h = ( i + k <= nc ) ? i + k : nc + 1 ;
      h = ( h > 1 ) ? h : 1 ;
      for ( j = 1 ; j < (INDEX) h ; j++ )
         mat(i,j) = 0.0 ;
      for ( j = (INDEX) h ; j <= nc ; j++ )
         mat(i,j) = x(i,j) ;
   } // for i
   return *this ;
} // matrix triuOf

matrix& matrix::trilOf( const matrix& x, int k )
{
   static char *mName = "trilOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   reset( nr, nc ) ;
   int h ;
   for ( i = 1 ; i <= nr ; i++ ) {
      h = ( i + k < 1 ) ? 0 : i + k ;
      h = ( h <= nc ) ? h : nc ;
      for ( j = 1 ; j <= (INDEX) h ; j++ )
         mat(i,j) = x(i,j) ;
      for ( j = (INDEX) h + 1 ; j <= nc ; j++ )
         mat(i,j) = 0.0 ;
   } // for i
   return *this ;
} // matrix trilOf

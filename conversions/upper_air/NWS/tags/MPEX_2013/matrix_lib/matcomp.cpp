/***************************************************/
/* matcomp.c source for MatClass logical functions */
/***************************************************/


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

/********************************************************/
/*                  Comparison Methods                  */
/********************************************************/

matrix matrix::and( const matrix& y ) M_CONST
{
   static char *mName = "and" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName ) ;
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( ( mat(i,j) != 0 ) && ( y(i,j) != 0 ) ) ;
   return z ;
} // matrix:and

matrix matrix::or( const matrix& y ) M_CONST
{
   static char *mName = "or" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName ) ;
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( ( mat(i,j) != 0 ) || ( y(i,j) != 0 ) ) ;
   return z ;
} // matrix:or

matrix not( const matrix& x )
{
   static char *mName = "not" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) != 0 ) ;
   return z ;
} // matrix::not

matrix operator == ( const matrix& x, const matrix& y )
{
   static char *mName = "op ==" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   x.checkDims( y, mName  ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   matrix z( nr, nc ) ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) == y(i,j) ) ;
   return z ;
} // matrix op ==

matrix operator > ( const matrix& x, const matrix& y )
{
   static char *mName = "op >" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   x.checkDims( y, mName  ) ;
   INDEX nr = x.nRows(), nc = x.nCols(),i, j ;
   matrix z( nr, nc ) ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) > y(i,j) ) ;
   return z ;
} // matrix op >

matrix operator >= ( const matrix& x, const matrix& y )
{
   char *mName = "op >" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return not( y > x ) ;
} // matrix op >=

matrix operator <  ( const matrix& x, const matrix& y )
{
   char *mName = "op >" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return ( y > x ) ;
} // matrix op <

matrix operator <= ( const matrix& x, const matrix& y )
{
   char *mName = "op <=" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return not( x > y ) ;
} // matrix op <=

matrix operator != ( const matrix& x, const matrix& y )
{
   static char *mName = "op <=" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return not( x == y ) ;
} // matrix op !=

INDEX any( const matrix& x )
{
   static char *mName = "any" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   INDEX zero = TRUE ;
   for ( j = 1 ; zero && j <= nc; j++ )
      for ( i = 1 ; zero && i <= nr ; i++ )
         zero = ( x(i,j) == 0 ) ;
   return !zero ;
} // any()

INDEX all( const matrix& x )
{
   static char *mName = "all" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   INDEX nonzero = TRUE ;
   for ( j = 1 ; nonzero && j <= nc; j++ )
      for ( i = 1 ; nonzero && i <= nr ; i++ )
         nonzero = ( x(i,j) != 0 ) ;
   return nonzero ;
} // matrix::all()

comparison matrix::compare( const matrix& y ) M_CONST
{
   static char *mName = "compare" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   int comp = 0 ;
   REAL r, s ;
   for ( j = 1 ; j <= nc ; j++ ) {
      for ( i = 1 ; i <= nr ; i++ ) {
         s = mat(i,j) ;
         r = y(i,j) ;
         if ( s < r )
            comp |= LESS ;
         else if ( s > r )
            comp |= GREATER ;
         else
            comp |= EQUAL ;
      } // for j
   } // for i
   return (comparison) comp ;
} // compare matrix

matrix operator == ( const matrix& x, REAL r )
{
   static char *mName = "op == r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) == r ) ;
   return z ;
} // matrix op == r

matrix operator > ( const matrix& x, REAL r )
{
   static char *mName = "op > r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) > r ) ;
   return z ;
} // matrix op > r

matrix operator < ( const matrix& x, REAL r )
{
   static char *mName = "op < r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   INDEX i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         z(i,j) = (REAL) ( x(i,j) < r ) ;
   return z ;
} // matrix op < r

matrix operator >= ( const matrix& x, REAL r )
{
   static char *mName = "op >= r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return  not( x < r ) ;
} // matrix op >= r

matrix operator <= ( const matrix& x, REAL r )
{
   static char *mName = "op <= r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return not( x > r ) ;
} // matrix op <= r

matrix operator != ( const matrix& x, REAL r )
{
   static char *mName = "op != r" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   return not( x == r ) ;
} // matrix op != r

comparison matrix::compare( const REAL r ) M_CONST
{
   char *mName = "compare(r)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   int comp = 0 ;
   REAL s ;
   for ( j = 1 ; j <= nc ; j++ ) {
      for ( i = 1 ; i <= nr ; i++ ) {
         s = mat(i,j) ;
         if ( s < r )
            comp |= LESS ;
         else if ( s > r )
            comp |= GREATER ;
         else
            comp |= EQUAL ;
      } // for j
   } // for i
   return (comparison) comp ;
} // compare REAL

INDEX matrix::countTrue( void )  M_CONST
{
   static char *mName = "countTrue" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = 0, nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1; i <= nr; i++ )
         if ( mat(i,j) != 0.0 ) ++n ;
   return n ;
} // countTrue

indexArray matrix::mapTrue( void )  M_CONST
{
   // needs rethinking !!!
   static char *mName = "mapTrue" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName , NOTVC ) ;
   INDEX nr = nRows(), n = countTrue(), i, j = 0 ;
   indexArray map( n ) ;
   for ( i = 1; i <= nr && j < n ; i++ ) {
      if ( mat(i,1) != 0.0 )
         map(++j) = i ;
   } // for
   return map ;
} // matrix::mapTrue

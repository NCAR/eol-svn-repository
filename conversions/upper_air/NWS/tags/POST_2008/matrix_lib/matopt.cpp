/**************************************************/
/* matopt.c source for MatClass optima functions  */
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
#include <math.h>

INDEX matrix::rowOpti( INDEX i, int option ) M_CONST
/*
   return column INDEX of optimum in ith row
*/
{
   INDEX j, optj, xnc = nCols() ;
   REAL  r, optimum ;
   if ( option & ABS )
      optimum = (REAL) fabs( (DOUBLE) mat(i,1) ) ;
   else
      optimum = mat(i,1) ;
   optj = 1 ;
   for ( j = 2; j <= xnc; j++ ) {
      r = mat(i,j) ;
      if ( option & ABS )
         r = (REAL) fabs( (DOUBLE) r ) ;
      if ( ( option & MINIMUM ) ? ( r < optimum ) : ( r > optimum ) ) {
         optimum = r ;
         optj = j ;
      } // if
   } // for j
   return optj ;
} // matrix::rowOpti

INDEX matrix::colOpti( INDEX j, int option ) M_CONST
/*
   return row INDEX of optimum in jth column
*/
{
   INDEX i, opti, xnr = nRows() ;
   REAL  r, optimum ;
   if ( option & ABS )
      optimum = (REAL) fabs( (DOUBLE) mat(1,j) ) ;
   else
      optimum = mat(1,j) ;
   opti = 1 ;
   for ( i = 2; i <= xnr; i++ ) {
      r = mat(i,j) ;
      if ( option & ABS )
         r = (REAL) fabs( (DOUBLE) r ) ;
      if ( ( option & MINIMUM ) ? ( r < optimum ) : ( r > optimum ) ) {
         optimum = r ;
         opti = i ;
      } // if
   } // for i
   return opti ;
} // matrix::colOpti

matrix& matrix::rowOptOf( const matrix& x, int option )
{
   static char *mName = "rowOptOf" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX xnr = x.nRows(), i ;
   reset( xnr ) ;
   for ( i = 1; i <= xnr; i++ )
      mat(i) = x( i, x.rowOpti( i, option ) ) ;
   return *this ;
} // matrix::rowOptOf

matrix& matrix::colOptOf( const matrix& x, int option )
{
   static char *mName = "colOptOf" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX xnc = x.nCols(), j ;
   reset( xnc ) ;
   for ( j = 1; j <= xnc; j++ )
      mat(j) = x( x.colOpti( j, option ), j ) ;
   return *this ;
} // matrix::colOptima

indexArray matrix::optimaMap( indexArray& map, int option ) M_CONST
{
   static char *mName = "optimaMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   if ( option & ROWWISE ) {
      if ( nr != map.length() )
         map.reset( nr ) ;
      for ( i = 1; i <= nr; i++ )
         map(i) = rowOpti( i, option ) ;
   } else {
      if ( nc != map.length() )
         map.reset( nc ) ;
      for ( j = 1; j <= nc; j++ )
         map(j) = colOpti( j, option ) ;
   } // else
   return map ;
} // matrix::optimaMap

indexArray matrix::optimaMap( int option ) M_CONST
{
   static char *mName = "optimaMap()" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() ) ;
   optimaMap( map, option ) ;
   return map ;
} // matrix::optimaMap

matrix matrix::rowMax( int opt ) M_CONST
{
   static char *mName = "rowMax" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.rowOptOf( *this, opt ) ;
   return z ;
} // matrix::rowMax

matrix matrix::rowMin( int opt ) M_CONST
{
   static char *mName = "rowMin" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.rowOptOf( *this, opt | MINIMUM ) ;
   return z ;
} // matrix::rowMin

matrix matrix::colMax( int opt ) M_CONST
{
   static char *mName = "colMax" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.colOptOf( *this, opt ) ;
   return z ;
} // matrix::colMax

matrix matrix::colMin( int opt ) M_CONST
{
   static char *mName = "colMin" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.colOptOf( *this, opt | MINIMUM  ) ;
   return z ;
} // matrix::colMin

indexArray matrix::rowMaxMap( int opt ) M_CONST
{
   static char *mName = "rowMaxMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() );
   optimaMap( map, opt | ROWWISE ) ;
   return map ;
} // matrix::rowMaxMap

indexArray matrix::rowMinMap( int opt ) M_CONST
{
   static char *mName = "rowMinMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() );
   optimaMap( map, opt | MINROW ) ;
   return map ;
} // matrix::rowMini

indexArray matrix::colMaxMap( int opt  ) M_CONST
{
   static char *mName = "colMaxMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() );
   optimaMap( map, opt  ) ;
   return map ;
} // matrix::colMaxi

indexArray matrix::colMinMap( int opt  ) M_CONST
{
   static char *mName = "colMinMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() );
   optimaMap( map, opt | MINIMUM ) ;
   return map ;
} // matrix::colMini

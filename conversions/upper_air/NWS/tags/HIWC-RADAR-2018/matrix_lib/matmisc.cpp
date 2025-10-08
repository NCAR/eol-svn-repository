/**************************************************/
/*  matmsic.c source for misc MatClass functions  */
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

matrix matrix::trans( void ) M_CONST
{
   static char *mName = "trans" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   INDEX nr = nCols(), nc = nRows(), i, j ;
   matrix z( nr, nc ) ;
   for ( i = 1; i <= nr; i++ )
      for ( j = 1; j <= nc; j++ )
	 z(i,j) = mat(j,i) ;
   return z ;
} //trans

matrix matrix::operator ! ( void ) M_CONST
/*****************************************
  Generate temporary copy of this that
  has type TRANSPOSED.
*****************************************/
{
   static char *mName = "op !" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   matrix z ;
   z.pm = pm ;
   if ( pm != 0 )
      pm->ref++ ;
   z.type = TRANSPOSED ;
   return z ;
} // matrix op !

REAL matrix::trace( void ) M_CONST
{
   static char *mName = "trace" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = nRows() ;
   if ( n > nCols() )
      n = nCols() ;
   DOUBLE r = 0.0 ;
   for ( INDEX i = 1 ; i <= n ; i++ )
      r += (DOUBLE) mat(i,i) ;
   return (REAL) r ;
} // matrix::trace

REAL matrix::sum( void ) M_CONST
{
   static char *mName = "sum" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nc = nCols(), nr = nRows(), i, j ;
   DOUBLE  r = 0.0 ;
   for ( j = 1; j <= nc; j++ )
      for ( i = 1; i <= nr; i++ )
	 r += (DOUBLE) mat(i,j) ;
   return (REAL) r ;
} // matrix::sum

REAL matrix::aver( void ) M_CONST
{
   static char *mName = "aver" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = nRows() * nCols() ;
   if ( n < 1 )
      errorExit( mName, NPDIM );
   return ( sum() / n ) ;
} // matrix::aver

REAL matrix::sumsq( void ) M_CONST
{
   static char *mName = "sumsq" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nc = nCols(), nr = nRows() ;
   DOUBLE  sum = 0.0, r ;
   for ( INDEX j = 1; j <= nc; j++ ) {
      for ( INDEX i = 1; i <= nr; i++ ) {
	 r = (DOUBLE) mat(i,j) ;
	 sum += r * r ;
      } // for i
   } // for j
   return (REAL) sum ;
} // sumsq

REAL matrix::norm1( void ) M_CONST
{
   static char *mName = "norm1" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols() ;
   DOUBLE maxs = 0.0, s ;
   for ( INDEX j = 1 ; j <= nc ; j++ ) {
      s = 0.0 ;
      for ( INDEX i = 1 ; i <= nr ; i++ )
	 s += fabs( (double) mat(i,j) ) ;
      if ( s > maxs )
	 maxs = s ;
   } // for j
   return (REAL) maxs ;
} // matrix::norm1

REAL matrix::normi( void ) M_CONST
{
   static char *mName = "normi" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   DOUBLE maxs = 0.0, s ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ ) {
	 s = fabs( (double) mat(i,j) ) ;
	 if ( s > maxs )
	    maxs = s ;
      } // for i
   return (REAL) maxs ;
} // matrix::normi

REAL matrix::normf( void ) M_CONST
/*
   !!! Rewrite to cover over- and under- flow
*/
{
   char *mName = "normf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   return (REAL) sqrt( (DOUBLE) sumsq() ) ;
} // matrix::normf


matrix eye( INDEX nr, INDEX nc )
{
   static char *mName = "eye" ;
   matrix z( nr, nc ) ;
   matFunc func( mName ) ; z.debugInfo( func ) ;
   INDEX i, j ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ ) {
	 if ( i == j )
	    z(i,j) = 1.0 ;
	 else
	    z(i,j) = 0.0 ;
      } // for
   return z ;
} // eye

matrix zeros( INDEX nr, INDEX nc )
{
   static char *mName = "zeros" ;
   matrix z( nr, nc ) ;
   matFunc func( mName ) ; z.debugInfo( func ) ;
   INDEX i, j ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ )
	 z(i,j) = 0 ;
   return z ;
} // zeros

matrix ones( INDEX nr, INDEX nc )
{
   static char *mName = "ones" ;
   matrix z( nr, nc ) ;
   matFunc func( mName ) ; z.debugInfo( func ) ;
   INDEX i, j ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ )
	 z(i,j) = 1.0 ;
   return z ;
} // ones

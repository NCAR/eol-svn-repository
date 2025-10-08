/***************************************************/
/* matsumm.c source for MatClass summary functions */
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
#include <math.h>

/*************************************************/
/*               Summary Methods                 */
/*************************************************/

matrix& matrix::rowSumOf( const matrix& x, int opt )
{
   static char *mName = "rowSumOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   DOUBLE sum ;
   reset( nr ) ;
   for ( i = 1; i <= nr; i++ ) {
      sum = 0.0 ;
      for ( j = 1; j <= nc; j++ ) {
         if ( opt & ABS )
            sum += fabs( (DOUBLE) x(i,j) ) ;
         else
            sum += (DOUBLE) x(i,j) ;
      } // for j
      mat(i) = (REAL) sum ;
   } // for i
   return *this ;
} // matrix::rowSumOf

matrix matrix::rowSum( int opt ) M_CONST
{
   static char *mName = "rowSum" ;
   matFunc func( mName ) ; debugInfo( func ) ;  
   matrix z( nRows() ) ;
   z.rowSumOf( *this, opt ) ;
   return z ;
} // matrix rowSum

matrix& matrix::colSumOf( const matrix& x, int opt )
{
   static char *mName = "colSumOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   DOUBLE sum ;
   reset( nc ) ;
   for ( j = 1; j <= nc; j++ ) {
      sum = 0.0 ;
      for ( i = 1; i <= nr; i++ ) {
         if ( opt & ABS )
            sum += fabs( (DOUBLE) x(i,j) ) ;
         else
            sum += (DOUBLE) x(i,j) ;
      } // for j
      mat(j) = (REAL) sum ;
   } // for i
   return *this ;
} // matrix::colSumOf

matrix matrix::colSum( int opt ) M_CONST
{
   static char *mName = "colSum" ;
   matFunc func( mName ) ; debugInfo( func ) ;  
   matrix z( nCols() ) ;
   z.colSumOf( *this, opt ) ;
   return z ;
} // matrix colSum

matrix& matrix::rowMeanOf( const matrix& x, int opt ) 
{
   static char *mName = "rowMeanOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = x.nCols() ;
   if ( n == 0 )
      errorExit( mName, NPDIM ) ;
   rowSumOf( x, opt ) ;
   *this /= REAL(n) ;
   return *this ;
} // matrix::rowMeanOf

matrix matrix::rowMean( int opt ) M_CONST
{
   static char *mName = "rowMean" ;
   matFunc func( mName ) ; debugInfo( func ) ;  
   matrix z( nRows() ) ;
   z.rowMeanOf( *this, opt ) ;
   return z ;
} // matrix rowMean

matrix& matrix::colMeanOf( const matrix& x, int opt )
{
   static char *mName = "colMeanOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = x.nRows() ;
   if ( n == 0 )
      errorExit( mName, NPDIM ) ;
   colSumOf( x, opt ) ;
   *this /= REAL(n) ;
   return *this ;
} // matrix::colMeanOf

matrix matrix::colMean( int opt ) M_CONST
{
   static char *mName = "colMean" ;
   matFunc func( mName ) ; debugInfo( func ) ;  
   matrix z( nCols() ) ;
   z.colMeanOf( *this, opt ) ;
   return z ;
} // matrix colMean

matrix& matrix::rowSqrOf( const matrix& x )
{
   static char *mName = "rowSqrOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() , i, j ;
   REAL r, sum ;
   reset( nr ) ;   
   for ( i = 1; i <= nr; i++ ) {
      sum = 0.0 ;
      for ( j = 1; j <= nc; j++ ) {
	 r = x(i,j) ;
         sum += (DOUBLE) r * r ;
      } // for j
      mat(i) = (REAL) sum ;
   } // for i
   return *this ;
} // matrix::rowSqrOf

matrix& matrix::colSqrOf( const matrix& x )
{
   static char *mName = "colSqrOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() , i, j ;
   REAL r, sum ;
   reset( nc ) ;
   for ( j = 1; j <= nc; j++ ) {
      sum = 0.0 ;
      for ( i = 1; i <= nr; i++ ) {
         r = x(i,j) ;
         sum += (DOUBLE) r * r ;
      } // for j
      mat(j) = (REAL) sum ;
   } // for j
   return *this ;
} // matrix::colSqrOf

matrix& matrix::rowSqDevOf( const matrix& x, const matrix& avg ) 
{
   static char *mName = "rowSqDevOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   DOUBLE sum ;
   REAL d, m ;
   reset( nr ) ;
   if ( avg.nRows() != nr )
      errorExit( mName, NEDIM ) ;
   for ( i = 1; i <= nr; i++ ) {
      sum = 0.0 ;
      m = avg(i) ;
      for ( j = 1; j <= nc; j++ ) {
         d = x(i,j) - m ;
         sum += d * d ;
      } // for j
      mat(i) = (REAL) sum ;
   } // for i
   return *this ;
} // matrix::rowSqDevOf

matrix& matrix::colSqDevOf( const matrix& x, const matrix& avg ) 
{
   static char *mName = "colSqDevOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   DOUBLE sum ;
   REAL d, m ;
   reset( nc ) ;
   if ( avg.nCols() != nc )
      errorExit( mName, NEDIM ) ;
   for ( j = 1; j <= nc; j++ ) {
      sum = 0.0 ;
      m = avg(j) ;
      for ( i = 1; i <= nr; i++ ) {
         d = x(i,j) - m ;
         sum += (DOUBLE) d * d ;
      } // for j
      mat(j) = (REAL) sum ;
   } // for i
   return *this ;
} // matrix::colSqDevOf

void matrix::colPlus( const matrix& m )
{
   static char *mName = "colPlus" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows() , nc = nCols(), i, j ;
   REAL r ;
   if ( m.nRows() != nc )
      errorExit( mName, NEDIM ) ;
   for ( j = 1 ; j <= nc ; j++ ) {
      r = m(j) ;
      for ( i = 1 ; i <= nr ; i++ )
	 mat(i,j) += r ;
   } // for j
} // matrix::colPlus

void matrix::rowPlus( const matrix& m )
{
   static char *mName = "rowPlus" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows() , nc = nCols(), i, j ;
   REAL r ;
   if ( m.nRows() != nr )
      errorExit( mName, NEDIM ) ;
   for ( i = 1 ; i <= nr ; i++ )
      r = m(i) ;
      for ( j = 1 ; j <= nc ; j++ ) {
	 mat(i,j) += r ;
   } // for j
} // matrix::rowPlus

void matrix::colMinus( const matrix& m )
{
   static char *mName = "colMinus" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows() , nc = nCols(), i, j ;
   REAL r ;
   if ( m.nRows() != nc )
      errorExit( mName, NEDIM ) ;
   for ( j = 1 ; j <= nc ; j++ ) {
      r = m(j) ;
      for ( i = 1 ; i <= nr ; i++ )
	 mat(i,j) -= r ;
   } // for j
} // matrix::colMinus

void matrix::rowMinus( const matrix& m )
{
   static char *mName = "rowMinus" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows() , nc = nCols(), i, j ;
   REAL r ;
   if ( m.nRows() != nr )
      errorExit( mName, NEDIM ) ;
   for ( i = 1 ; i <= nr ; i++ )
      r = m(i) ;
      for ( j = 1 ; j <= nc ; j++ ) {
	 mat(i,j) -= r ;
   } // for j
} // matrix::rowMinus

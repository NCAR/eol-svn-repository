/**************************************************/
/*     matchol.c source for Cholesky Functions    */
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

#include "matchol.hpp"

/***************************************************/
/*                   cholesky                      */
/***************************************************/

#include <math.h>
// for sqrt

INDEX cholesky( matrix& x, matError& error  )
/********************************************
  Cholesky decomposition. Lower triang of x
  overwritten by decomposition. Nonsquare
  and non-definite matrices trapped as
  errors.
********************************************/
{
   static char *mName = "cholesky" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX i, j, k, n = x.nRows() ;
   DOUBLE g, h ;
   error = NOERROR ;
   if ( n != x.nCols() ) {
      error = NTSQR ;
      return 0 ;
   } // if
   for ( j = 1 ; j <= n; j++ ) {
      g = x(j,j) ;
      for ( k = 1 ; k < j ; k++ )
        g -= x(j,k) * x(j,k) ;
      if ( g <= 0.0 ) {
         error = NOTPD ;
         return 0 ;
      } // if
      x(j,j) = g = sqrt( g ) ;
      for ( i = j + 1 ; i <= n ; i++ ) {
         h = x(i,j) ;
         for ( k = 1 ; k < j ; k++ )
            h -= x(i,k) * x(j,k) ;
         x(i,j) = h / g ;
      } // for i
   } // for j
   return OK ;
} // cholesky

INDEX cholSolve( const matrix& A, matrix& b, REAL tol,
                 matError& error )
/****************************************************************
  Solve equations Ax=b assuming A has been factored by
  cholesky. The lower triang factor G is assumed to be
  in lower triang of A. The real tol is used to determine
  if diagonal element is zero. The solution is returned in b.
*****************************************************************/
{
   static char *mName = "cholSolve" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX i, j, n = A.nRows() ;
   DOUBLE r ;
   error = NOERROR ;
   if ( n != A.nCols() ) {
      error = NTSQR ;
      return 0 ;
   } // if
   if ( n != b.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if
   // Use forward substitution on G
   for ( i = 1 ; i <= n ; i++ ) {
      if ( A(i,i) < tol ) {
         error = NOTPD ;
         return 0 ;
      } // if
      r = b(i) ;
      for ( j = 1 ; j < i ; j++ )
         r -= A(i,j) * b(j) ;
      b(i) = r / A(i,i) ;
   } // for i
   // Use backward substitution on G transpose
   for ( i = n ; i ; i-- ) {
      // no need to check diagonals again
      r = b(i) ;
      for ( j = n ; j > i ; j-- )
         r -= A(j,i) * b(j) ;
      b(i) = r / A(i,i) ;
   } // for i
   return OK ;
} // cholSolve

matrix normalEqn( const matrix& x, const matrix& b )
{
   static char *mName = "normalEqn" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   if ( b.nRows() != x.nRows() )
      b.errorExit( mName, NEDIM ) ;
   matError error ;
   REAL tol = matrixTol() ;
   INDEX nr = x.nCols(), nc = b.nCols() ;
   matrix z( nr, nc ) ;
   z.TMultOf( x, b ) ;
   matrix xTx( nr, nr ) ; ;
   xTx.TMultOf( x, x ) ;
   cholesky( xTx, error ) ;
   refMatrix zc( z ) ;
   for ( INDEX i = 1; !error && i <= nc ; i++ ) {
      zc.refCol(i) ;
      cholSolve( xTx, zc, tol, error ) ;
   } //for
   if ( error )
      xTx.errorExit( mName, error ) ;
   return z ;
} // normalEqn


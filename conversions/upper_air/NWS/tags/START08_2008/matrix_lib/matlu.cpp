/**************************************************/
/*         matlu.c source for LU functions        */
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

#include "matlu.hpp"
#include <math.h>

/****************************************************
            crout.cxx

 Crout's Method of LU decomposition of square matrix,
 with implicit partial pivoting.
 A is overwritten : U is explicit in the upper triag
 and L is in multiplier form in the subdiagionals
 i.e. subdiag A.mat(i,j) is the multiplier used to
 eliminate the (i,j) term. Row permutations are
 mapped out in index. sign is used for calculating
 the determinant, see determinant function below.

 Adapted from Numerical Recipes in C by Press et al.
*****************************************************/

INDEX crout( matrix& A, indexArray& index, REAL& sign, REAL tol,
             matError& error  )
{
   static char *mName = "crout" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX n = A.nRows() ;
   error = NOERROR ;
   if ( n != A.nCols() ) {
      error = NTSQR ;
      return 0 ;
   } // if
   if ( n != index.length() )
      index.reset( n ) ;

   INDEX    i, imax, j, k, zeros = 0 ;
   DOUBLE   sum ;
   REAL     big, temp ;
   matrix   scale(n) ;

   sign = 1.0 ;

   /*****************************
    Find implicit scaling factors
   ******************************/
   for ( i = 1; i <= n ; i++ ) {
      big = 0.0 ;
      for ( j = 1; j <= n; j++ ) {
         temp = (REAL) fabs( (double) A.mat(i,j) ) ;
         if ( temp > big )
            big = temp ;
      } // for
      scale(i) = ( big == 0.0 ? 0.0 : 1.0 / big ) ;
   } // for i

   for ( j = 1; j <= n; j++ ) {
      /*************************************
       Run down jth column from top to diag,
       to form the elements of U.
      **************************************/
      for ( i = 1; i < j; i++ ) {
         sum = (DOUBLE) A(i,j) ;
         for ( k = 1; k < i; k++ )
            sum -= A(i,k) * A(k,j) ;
         A(i,j) = (REAL) sum ;
      } // for i
      /******************************************
       Run down jth subdiag to form the residuals
       after the elimination of the first j-1
       subdiags.  These residuals divided by the
       appropriate diagonal term will become the
       multipliers in the elimination of the jth.
       subdiag. Find index of largest scaled term
       in imax.
      *******************************************/
      big = 0.0 ;
      for ( i = j; i <= n; i++ ) {
         sum = (DOUBLE) A(i,j) ;
         for ( k = 1; k < j; k++ )
            sum -= A(i,k) * A(k,j) ;
         A(i,j) = (REAL) sum ;
         temp = scale(i) * ( (REAL) fabs( sum ) ) ;
         if ( temp >= big ) {
            big = temp ;
            imax = i ;
         } // if
      }  // for i
      /*****************************
       Permute current row with imax
      ******************************/
      if ( j != imax ) {
         for ( k = 1; k <= n; k++ ) {
            temp = A( imax, k ) ;
            A( imax, k ) = A(j,k) ;
            A(j,k) = temp ;
         } // for k
         sign = - sign ;
         scale( imax ) = scale(j) ;
      } // if
      index(j) = imax;
      /***************************************
       If diag term is not zero divide subdiag
       to form multipliers.
      ****************************************/
      if ( fabs( A(j,j) ) < tol )
         zeros++ ;
      else if ( j != n ) {
         temp = 1.0 / A(j,j) ;
         for ( i = j+1; i <= n; i++ )
            A(i,j) *= temp;
      } // if
   } // for j
   if ( zeros )
      error = SINGM ;
   return zeros ;
} // crout()


/***************************************************
                    luSolve.cxx

 Solve Ax=b assuming A is in the LU form, but
 assume b has *not* been transformed.  Solution
 returned in b.  A is unchanged.

 Adapted from Numerical Recipes in C by Press et al.
****************************************************/

INDEX luSolve( const matrix& A, const indexArray& index,
	       matrix& b, REAL tol, matError& error )
{
   static char *mName = "luSolve" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX  n = A.nRows() ;
   error = NOERROR ;
   if ( b.nCols() != 1 ) {
      error = NOTVC ;
      return 0 ;
   } // if
   if ( n != b.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if

   INDEX  i, nonzero=0, iperm, j ;
   DOUBLE sum, zero = 0.0 ;

   /*************************
    Check for zero diagonals
   **************************/
   for ( i = 1; i <= n ; i++ )
      if ( fabs( A(i,i) ) < tol )
         return i ;

   /**************************************
    Transform b allowing for leading zeros
   ***************************************/

   for ( i = 1; i <= n; i++ ) {
      iperm = index(i) ;
      sum = (DOUBLE) b( iperm ) ;
      b( iperm ) = b(i) ;
      if ( nonzero )
         for ( j = nonzero; j <= i-1; j++ )
            sum -= A(i,j) * b(j) ;
      else if ( sum != zero )
         nonzero = i ;
      b(i) = (REAL) sum ;
   } // for i

   /****************
    backsubstitution
   *****************/

   for ( i = n; i >= 1; i-- ) {
      sum = (DOUBLE) b(i) ;
      for ( j = i+1; j <= n; j++ )
         sum -= A(i,j) * b(j) ;
      b(i) = (REAL) ( sum / A(i,i) ) ;
   } // for i
   return 0 ;
} // luSolve



/***************************************************
                  luInverse.cxx

 Returns inverse of A in invA assuming A in LU form.
****************************************************/


INDEX luInverse( const matrix& A, const indexArray& index,
		 matrix& invA, REAL tol, matError& error )
{
   static char *mName = "luInverse" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX  nc = A.nCols(), nr = A.nRows() ;
   if ( ( nc != invA.nCols() ) || ( nr != invA.nRows() ) )
      invA.reset( nr, nc ) ;
   INDEX j, exit = 0 ;
   invA = 0.0 ;
   invA.setDiag( 1.0 ) ;
   refMatrix col( invA ) ;
   error = NOERROR ;
   for ( j = 1 ; ( exit == 0 ) && !error && ( j <= nc ) ; j++ ) {
      col.refCol(j) ;
      exit = luSolve( A, index, col, tol, error ) ;
   } // for
   return exit ;
} // luInverse


/***************************************************
                    lutSolve.cxx

 Solve A.trans()x=b assuming A is in the LU form,
 but assume b has *not* been transformed.
 Solution returned in b.  A is unchanged.

 Adapted from luSolve above and based on LINPACK
 USER's GUIDE p1.14.
****************************************************/

INDEX lutSolve( const matrix& A, const indexArray& index,
	        matrix& b, REAL tol, matError& error )
{
   static char *mName = "lutSolve" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX  n = A.nCols() ;
   error = NOERROR ;
   if ( b.nCols() != 1 ) {
      error = NOTVC ;
      return 0 ;
   } // if
   if ( n != b.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if

   INDEX  i, nonzero=0, iperm, j ;
   DOUBLE sum, zero = 0.0;

   /*************************
    Check for zero diagonals
   **************************/
   for ( i = 1; i <= n ; i++ )
      if ( fabs( A(i,i) ) < tol )
         return i ;

   /********************
    forward substitution
   *********************/

   for ( i = 1; i <= n ; i++ ) {
      sum = (DOUBLE) b(i) ;
      for ( j = 1; j < i ; j++ )
         sum -= A(j,i) * b(j) ;
      b(i) = (REAL) ( sum / A(i,i) ) ;
   } // for i

   /***************************************
    Transform b allowing for trailing zeros
   ****************************************/

   for ( i = n ; i >= 1 ; i-- ) {
      sum = (DOUBLE) b(i) ;
      if ( nonzero ) {
         for ( j = i+1 ; j <= nonzero ; j++ )
            sum -= A(j,i) * b(j) ;
      } else if ( sum != zero )
         nonzero = i ;
      iperm = index(i) ;
      b(i) = b( iperm ) ;
      b(iperm) = (REAL) sum ;
   } // for i
   return 0 ;
} // lutSolve

INDEX luHager( const matrix& A, const indexArray& index, REAL& est,
	       REAL tol, matError& error, INDEX iter )
/****************************************************************
   Estimates lower bound for norm1 of inverse of A.  Assumes A in
   LU form e.g. has been processed by crout. Returns norm estimate
   in est.  iter sets the maximum number of iterations to be used.
   The return value indicates the number of iterations remaining
   on exit from loop, hence if this is non-zero the processed
   "converged".  This routine uses Hager's Convex Optimisation
   Algorithm. See Applied Numerical Linear Algebra, p139 &
   SIAM J Sci Stat Comp 1984 pp 311-16
****************************************************************/
{
   static char *mName = "luHager" ;
   matFunc func( mName ) ; A.debugInfo( func ) ;
   INDEX i , n, imax ;
   DOUBLE maxz, absz, product, ynorm1, inv_norm1 = 0.0 ;
   INDEX stop ;
   n = A.nRows() ;
   matrix b(n), y(n), z(n) ;
   error = NOERROR ;
   b = (REAL) ( 1.0 / n ) ;
   est = -1.0 ;
   do {
      y = b ;
      if ( luSolve( A, index, y, tol, error ) || error )
         return iter ;
      ynorm1 = y.norm1() ;
      if ( ynorm1 <= inv_norm1 ) {
         stop = TRUE ;
      } else {
         inv_norm1 = ynorm1 ;
         for ( i = 1 ; i <= n ; i++ )
            z(i) = ( y(i) >= 0.0 ? 1.0 : -1.0 ) ;
         if ( lutSolve( A, index, z, tol, error ) || error )
            return iter ;
         imax = 1 ;
         maxz = fabs( (double) z(1) ) ;
         for ( i = 2 ; i <= n ; i++ ) {
            absz = fabs( (double) z(i) ) ;
            if ( absz > maxz ) {
               maxz = absz ;
               imax = i ;
            } // if
         } // for i
         product = (DOUBLE) b.inner(z) ;
         stop = ( maxz <= product ) ;
         if ( !stop ) {
            b = (REAL) 0.0 ;
            b( imax ) = 1.0 ;
         } // if
      } // else
      iter-- ;
   } while ( !stop && iter ) ;
   est = (REAL) inv_norm1 ;
   return iter ;
} // luHager

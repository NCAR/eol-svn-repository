/**************************************************/
/*        matqrh.c source for matQrh class        */
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

#include "matqrh.hpp"
#include <math.h>

/****************************************************/
/*                 qrhDec methods                   */
/****************************************************/

INDEX hReflector( matrix& v, REAL& alpha, REAL& beta,
                  REAL& vMax, REAL tol, matError& error )
/***************************************************************
   Reflector is I - beta * u * u' where

                  u = w + alpha * e(1)
                  w = v / max(abs(v)).
              alpha = sign(v(1)) ||w||
               beta = 1 / ( alpha^2 + alpha * w(1) )
                    = 1 / ( alpha * u(1) ).

   The use of the scaled w avoids overflow & underflow.
   See Golub and Loan first edition pp 38-41.
   v is overwritten with u. max(abs(v)) is returned in vMax,
   if this is less than tol then proc returns singularity
   error. Proc insists v is a column vector.
***************************************************************/
{
   static char *mName = "hReflector" ;
   matFunc func( mName ) ; v.debugInfo( func ) ;
   DOUBLE sum ;
   REAL s ;
   error = NOERROR ;
   if ( v.nCols() != 1 ) {
      error = NOTVC ;
      return 0 ;
   } // if
   INDEX i, n = v.nRows() ;
   vMax = REAL( fabs( double( v(1) ) ) ) ;
   for ( i = 2 ; i <= n ; i++ ) {
       s = REAL( fabs( double( v(i) ) ) ) ;
       if ( s > vMax )
          vMax = s;
   } // for i
   if ( vMax < tol ){
      error = SINGM ;
      return 0 ;
   } // if
   // overwrite v with w and form alpha
   sum = 0.0 ;
   for ( i = 1 ; i <= n ; i++ ) {
      v(i) /= vMax ;
      sum += v(i) * v(i) ;
   } // for
   alpha = REAL( sqrt( sum ) ) ;
   if ( v(1) < 0.0 )
      alpha = - alpha ;
   v(1) += alpha ;
   beta = 1.0 / ( alpha * v(1) ) ;
   return OK ;
} // hReflector

INDEX hReflect( matrix& y, const matrix& u, REAL beta,
                matError& error )
/**************************************************
   Reflect y in plane with normal u. Assume
   beta contains 2/u'u. Assumes y and u has same
   number of rows and that u is a column vector.
   u and y must have the same number of rows and
   u must be a column vector.
**************************************************/
{
   static char *mName = "hReflect" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   error = NOERROR ;
   if ( u.nCols() != 1 ) {
      error = NOTVC ;
      return 0 ;
   } // if
   if ( u.nRows() != y.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if
   INDEX j, n = y.nCols() ;
   refMatrix yCol( y ) ;
   for ( j = 1; j <= n ; j++ ) {
      // yCol reference to jth col of y
      yCol.refCol( j ) ;
      // overwrite column with column - beta * (u'column) * u
      yCol.linear( - beta * u.inner( yCol ), u, yCol ) ;
   } // for j
   return OK ;
} // hReflect

INDEX qrh( matrix& x, matrix& diagR, matrix& b, REAL tol,
           matError& error )
/****************************************************************
   QR decomposition of x by Householder transformations. See
   Golub & Loan first edition p41 & Sec 6.2. R is returned in
   upper triang of x and diagR.  Q returned in `u-form' in
   lower triang of x and b, the latter containing the
   "Householder betas". diagR and b are reset if required.
   Errors arise from formation of reflectors i.e. singularity,
   and the reflections, though the latter should not lead to
   difficulties. Note it attempts to handle the cases where
   the number of rows is less than or equal to the number of
   columns.
****************************************************************/
{
   static char *mName = "qrh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), n, k ;
   refMatrix kCol(x), subMat(x) ;
   REAL colMax, alpha, beta ;
   if ( nr <= nc ) {
      n = nr - 1 ;
      diagR.reset( nr ) ;
      b.reset( nr ) ;
   } else {
      n = nc ;
      diagR.reset( nc ) ;
      b.reset( nc ) ;
   } // else
   for ( k = 1 ; k <= n ; k++ ) {
      kCol.refSub( k, nr, k, k ) ;
      if ( !hReflector( kCol, alpha, beta, colMax, tol, error ) )
         return 0 ;
      diagR(k) = - alpha * colMax ;
      b(k) = beta ;
      if ( k < nc ) {
         // set subMat to sub matrix to right of kCol
         subMat.refSub( k, nr, k+1, nc ) ;
         // apply Householder reflection to subMat
         if ( !hReflect( subMat, kCol, beta, error ) )
            return 0 ;
      } // if
   } // for k
   if ( nr <= nc ) {
      diagR( nr ) = x( nr, nr ) ;
      b( nr ) = 0 ;
   } // if
   return 1 ;
} // qrh

INDEX backSub( matrix& y, const matrix& x, REAL tol,
               matError& error )
/*********************************************
   Solve xb = y assuming x is upper triang
   Assume number of equations equals number
   of columns in x. This allows x to have
   more rows than columns e.g. after QR
   decomposition. Proc requires the number
   of rows be at least as great as the
   number of equations.
*********************************************/
{
   static char *mName = "backSub" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   INDEX i, j, k, n = x.nCols(), nc = y.nCols() ;
   DOUBLE r ;
   error = NOERROR ;
   if ( n > x.nRows() || n > y.nRows() ) {
      error = GTDIM ;
      return 0 ;
   } // if
   for ( k = 1 ; k <= nc ; k++ ) {
      for ( i = n ; i ; i-- ) {
         r = (DOUBLE) y(i,k) ;
         // upper tri of x is upper tri of R
         for ( j = i+1 ; j <= n ; j++ )
            r -= x(i,j) * y(j,k) ;
         if ( fabs( double( x(i,i) ) ) < tol ) {
            error = ZEROD ;
            return 0 ;
         } // if
         y(i,k) = (REAL) ( r / x(i,i) ) ;
      } // for j
   } // for k
   return OK ;
} // backSub

INDEX backSubT( matrix& y, const matrix& x, REAL tol,
	        matError& error )
/*********************************************
   Solve x'b = y assuming x is upper triang
   Assume number of equations equals number
   of columns in x. This allows x to have
   more rows than columns e.g. after QR
   decomposition. Proc requires the number
   of rows be at least as great as the
   number of equations.
   Essentially the procedure is to use
   forward substitution taking into account
   x is not in transposed form.
*********************************************/
{
   static char *mName = "backSubT" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   INDEX i, j, k, n = x.nCols(), nc = y.nCols() ;
   DOUBLE r ;
   error = NOERROR ;
   if ( n > x.nRows() || n > y.nRows() ) {
      error = GTDIM ;
      return 0 ;
   } // if
   for ( k = 1 ; k <= nc ; k++ ) {
      for ( i = 1 ; i <= n ; i++ ) {
         r = (DOUBLE) y(i,k) ;
         // upper tri of x is upper tri of R
         for ( j = 1 ; j < i ; j++ )
            r -= x(j,i) * y(j,k) ;
         if ( fabs( double( x(i,i) ) ) < tol ) {
            error = ZEROD ;
            return 0 ;
         } // if
         y(i,k) = (REAL) ( r / x(i,i) ) ;
      } // for j
   } // for k
   return OK ;
} // backSubT

INDEX triuInv( const matrix& R, matrix& Rinv, REAL tol,
	       matError& error )
{
   static char *mName = "triuInv" ;
   matFunc func( mName ) ; R.debugInfo( func ) ;
   INDEX i, j, k, n = R.nCols() ;
   REAL sum ;
   if ( n > R.nRows() ) {
      error = GTDIM ;
      return 0 ;
   } // if
   Rinv.reset( n, n ) ;
   for ( j = 1 ; j <= n ; j++ ) {
      if ( fabs( (double) R(j,j) ) < tol ) {
         error = SINGM ;
         return 0 ;
      } // if
      Rinv(j,j) = 1.0 / R(j,j) ;
   } // for j
   for ( j = n ; j ; j-- ) {
      for ( i = j - 1 ; i ; i-- ) {
         // find Rinv(i,j)
         sum = 0.0 ;
         for ( k = i + 1 ; k <= j ; k++ )
            sum -= R(i,k) * Rinv(k,j) ;
         Rinv(i,j) = Rinv(i,i) * sum ;
      } // for i
      for ( i = j + 1 ; i <= n ; i++ )
         Rinv(i,j) = 0.0 ;
   } // for j
   return OK ;
} // triuInv

INDEX qReflect( matrix& y, const matrix& q, matrix& w,
                matError& error )
/******************************************************
   Assume q and w are output from a Householder
   QR decomposition of a matrix e.g. qrh. Now apply
   reflectors in q to y. Insists on the number of rows
   in q and y be equal. It also checks q and w have
   the same number of rows. Note it handles the case
   where the number of rows is less than or equal to
   the number of columns.
******************************************************/
{
   static char *mName = "qReflect" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   INDEX nr = q.nRows(), qnc = q.nCols(), ync = y.nCols() ;
   refMatrix qCol(q), ySub(y) ;
   if ( ( nr != y.nRows() ) || ( qnc != w.nRows() ) ) {
      error = NEDIM ;
      return 0 ;
   } // if
   INDEX k, n = ( nr <= qnc ) ? nr - 1 : qnc ;
   for ( k = 1 ; k <= n ; k++ ) {
      qCol.refSub( k, nr, k, k ) ;
      ySub.refSub( k, nr, 1, ync ) ;
      if ( !hReflect( ySub, qCol, w(k), error ) )
         return 0 ;
   } // for k
   return OK ;
} // qReflect

INDEX qTReflect( matrix& y, const matrix& q, matrix& w,
		 matError& error )
/******************************************************
   Assume q and w are output from a Householder
   QR decomposition of a matrix e.g. qrh. Now apply
   reflectors in q to y ***in reverse order***.
   This is the inverse of qReflect.
   Insists on q being square and the number of rows
   in q and y be equal. It also checks q and w have
   the same number of rows.
******************************************************/
{
   static char *mName = "qTReflect" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   INDEX nr = q.nRows(), qnc = q.nCols(), ync = y.nCols() ;
   refMatrix qCol(q), ySub(y) ;
   if ( nr != qnc ) {
      error = NTSQR ;
      return 0 ;
   } // if
   if ( ( nr != y.nRows() ) || ( qnc != w.nRows() ) ) {
      error = NEDIM ;
      return 0 ;
   } // if
   INDEX k ;
   for ( k = nr - 1 ; k ; k-- ) {
      qCol.refSub( k, nr, k, k ) ;
      ySub.refSub( k, nr, 1, ync ) ;
      if ( !hReflect( ySub, qCol, w(k), error ) )
         return 0 ;
   } // for k
   return OK ;
} // qTReflect

INDEX hProject( matrix& y, matrix& x, matrix& R, matrix& w,
                matrix& beta, REAL tol, matError& error )
/*************************************************************
     Project y onto span(x). Use Householder QR of x.
     Overwrite x with Q in partition form, R is returned
     explicitly. w has the scales for the reflectors i.e.
     2/u'u. y overwritten by projected y, beta with OLS
     coeffs.
*************************************************************/
{
   static char *mName = "hProject" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   INDEX nr = x.nRows(), xnc = x.nCols(), ync = y.nCols() ;
   matrix diag ;
   error = NOERROR ;
   if ( nr != y.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if
   // form QR decomposition of x
   // Q returned in lower x and w, R is upper x and diag
   if ( !qrh( x, diag, w, tol, error ) )
      return 0 ;
   // Apply householder transforms to y
   if ( !qReflect( y, x, w, error ) )
      return 0 ;
   // form R explicitly
   INDEX n = ( nr <= xnc ) ? nr - 1 : xnc ;
   R.triuOf( x.sub( 1, n, 1, n ) ) ;
   R.setDiag( diag ) ;
   beta = y.sub( 1, n, 1, ync ) ;
   // Apply backSub to solve Rb = beta.
   // leaving OLS coeffs in upper beta
   if ( !backSub( beta, R, tol, error ) )
      return 0 ;
   return OK ;
} // hProject

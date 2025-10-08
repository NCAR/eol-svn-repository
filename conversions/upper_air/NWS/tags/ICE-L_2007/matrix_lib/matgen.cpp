/**************************************************/
/* matgen.c source for general MatClass functions */
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

matrix& matrix::linear( REAL a, const matrix& x,
                        const matrix& y )
/***********************************************
         return   a * x + y  in this
************************************************/
{
   static char *mName = "linear(a,x,y)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   x.checkDims( y, mName ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   reset( nr, nc ) ;
   if ( a == 1.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = x(i,j) + y(i,j) ;
   } else if ( a == -1.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = y(i,j) - x(i,j) ;
   } else if ( a == 0.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = y(i,j) ;
   } else {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = a * x(i,j) + y(i,j) ;
   } // else
   return *this ;
} // matrix::linear(a,x,y)

matrix& matrix::linear( REAL a, const matrix& x,
                        REAL b )
/***********************************************
        return  a * x + b  in  this
***********************************************/
{
   static char *mName = "linear(a,x,b)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   reset( nr, nc ) ;
   if ( a == 1.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = x(i,j) + b ;
   } else if ( a == -1.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = b - x(i,j) ;
   } else if ( a == 0.0 ) {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = b ;
   } else {
      for ( j = 1; j <= nc ; j++ )
         for ( i = 1; i <= nr ; i++ )
            mat(i,j) = a * x(i,j) + b ;
   } // else
   return *this ;
} // linear(a,x,b)

matrix& matrix::minus( void )
{
   static char *mName = "minus" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1; j <= nc ; j++ )
      for ( i = 1; i <= nr ; i++ )
         mat(i,j) = - mat(i,j) ;
   return *this ;
} // matrix minus

matrix& matrix::operator += ( const matrix &y )
{
   static char *mName = "op +=" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1; j <= nc ; j++ )
      for ( i = 1; i <= nr ; i++ )
         mat(i,j) += y(i,j) ;
   return *this ;
} // matrix +=

matrix& matrix::operator -= ( const matrix& y )
{
   static char *mName = "op -=" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1; j <= nc ; j++ )
      for ( i = 1; i <= nr ; i++ )
         mat(i,j) -= y(i,j) ;
   return *this ;
} // matrix op -=

matrix operator - ( const matrix& x )
{
   static char *mName = "unary -" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( -1.0, x, 0.0 ) ;
   return z ;
} // matrix unary -

matrix operator + ( const matrix& x, const matrix &y )
{
   static char *mName = "op +" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( 1.0, x, y ) ;
   return z ;
} // matrix op +

matrix operator - ( const matrix& x, const matrix &y )
{
   static char *mName = "op -" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( -1.0, y, x ) ;
   return z ;
} // matrix op -


matrix& matrix::operator  = ( REAL r )
{
   static char *mName = "matrix=REAL" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         mat(i,j) = r ;
   return *this ;
} // matrix = REAL

matrix& matrix:: operator += ( REAL r )
{
   static char *mName = "matrix+=REAL" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         mat(i,j) += r ;
   return *this ;
} // matrix += REAL

matrix& matrix::operator -= ( REAL r )
{
   static char *mName = "matrix-=REAL" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   *this += (-r) ;
   return *this ;
} // matrix -= REAL

matrix& matrix::operator *= ( REAL r )
{
   static char *mName = "matrix*=REAL" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   for ( j = 1 ; j <= nc ; j++ )
      for ( i = 1 ; i <= nr ; i++ )
         mat(i,j) *= r ;
   return *this ;
} // matrix *= REAL

matrix& matrix::operator /= ( REAL r )
{
   static char *mName = "matrix/=REAL" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( r == 0.0 )
      errorExit( mName, ZEROD ) ;
   *this *= (1/r) ;
   return *this ;
} // matrix *= REAL

matrix operator + ( const matrix& x, REAL r )
{
   static char *mName = "matrix+REAL" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( 1.0, x, r ) ;
   return z ;
} // matrix + REAL

matrix operator * ( const matrix& x, REAL r )
{
   static char *mName = "matrix*REAL" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( r, x, 0.0 ) ;
   return z ;
} // matrix * REAL

matrix operator - ( const matrix& x, REAL r )
{
   static char *mName = "matrix-REAL" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   z.linear( 1.0, x, -r ) ;
   return z ;
} // matrix - REAL

matrix operator / ( const matrix& x, REAL r )
{
   static char *mName = "matrix/REAL" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   if ( r == 0.0 )
      x.errorExit( mName, ZEROD ) ;
   matrix z ;
   z.linear( 1.0 / r, x, 0.0 ) ;
   return z ;
} // matrix - REAL

matrix operator + ( REAL r, const matrix &y )
{
   static char *mName = "REAL+matrix" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   matrix z ;
   z.linear( 1.0, y, r ) ;
   return z ;
} // REAL + matrix

matrix operator - ( REAL r, const matrix &y )
{
   static char *mName = "REAL-matrix" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   matrix z ;
   z.linear( -1.0, y, r ) ;
   return z ;
} // REAL - matrix

matrix operator * ( REAL r, const matrix &y )
{
   static char *mName = "REAL*matrix" ;
   matFunc func( mName ) ; y.debugInfo( func ) ;
   matrix z ;
   z.linear( r, y, 0.0 ) ;
   return z ;
} // REAL * matrix

matrix operator / ( REAL r, const matrix& x )
/*
   Element wise division of r by x
*/
{
   static char *mName = "REAL/matrix" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   REAL s ;
   INDEX nr = x.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   if ( x.isNull() )
      x.errorExit( mName, NULRF ) ;
   for ( INDEX j = 1 ; j <= nc ; j++ ) {
      for ( INDEX i = 1 ; i <= nr ; i++ ) {
         if ( ( s = x(i,j) ) == 0.0 ) {
            x.errorij( i, j ) ;
            x.errorExit( mName, ZEROD ) ;
         } // if
         z.mat(i,j) = r / s ;
      } // for j
   } // for i
   return z ;
} // REAL / matrix

matrix& matrix::multOf( const matrix& x,
                        const matrix& y )
/******************************************
     Return multiple of x and y in this
******************************************/
{
   static char* mName = "multOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n, nr, nc, i, j, k ;
   DOUBLE r ;
   if ( x.nCols() != y.nRows() )
      x.errorExit( mName, NEDIM ) ;
   n  = x.nCols() ;
   nr = x.nRows() ;
   nc = y.nCols() ;
   reset( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         r = 0.0 ;
         for ( k = 1 ; k <= n ; k++ )
            r += x(i,k) * y(k,j) ;
         mat(i,j) =  (REAL) r ;
      } // for j
   } // for i
   return *this ;
} // matrix multOf

matrix& matrix::TMultOf( const matrix& x,
                         const matrix& y )
/*******************************************
    Return multiple of x' and y in this
*******************************************/
{
   static char* mName = "TMultOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n, nr, nc, i, j, k ;
   DOUBLE r ;
   if ( x.nRows() != y.nRows() )
      x.errorExit( mName, NEDIM ) ;
   n  = x.nRows() ;
   nr = x.nCols() ;
   nc = y.nCols() ;
   reset( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         r = 0.0 ;
         for ( k = 1 ; k <= n ; k++ )
            r += x(k,i) * y(k,j) ;
         mat(i,j) =  (REAL) r ;
      } // for j
   } // for i
   return *this ;
} // TMultOf

matrix matrix::TMult( const matrix& y ) M_CONST
{
   static char* mName = "TMult" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.TMultOf( *this, y ) ;
   return z ;
} // matrix::TMult

matrix& matrix::multTOf( const matrix& x,
                         const matrix& y )
/****************************************
   Return multiple of x and y' in z
****************************************/
{
   static char* mName = "multTOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n, nr, nc, i, j, k ;
   DOUBLE r ;
   if ( x.nCols() != y.nCols() )
      x.errorExit( mName, NEDIM ) ;
   n  = x.nCols() ;
   nr = x.nRows() ;
   nc = y.nRows() ;
   reset( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         r = 0.0 ;
         for ( k = 1 ; k <= n ; k++ )
            r += x(i,k) * y(j,k) ;
         mat(i,j) =  (REAL) r ;
      } // for j
   } // for i
   return *this ;
} // multTOf

matrix matrix::multT( const matrix& y ) M_CONST
{
   static char* mName = "multT" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.multTOf( *this, y ) ;
   return z ;
} // matrix::multT

matrix& matrix::TMultTOf( const matrix& x,
                          const matrix& y )
/******************************************
   Return multiple of x' and y' in z
******************************************/
{
   static char* mName = "TMultTOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n, nr, nc, i, j, k ;
   DOUBLE r ;
   if ( x.nRows() != y.nCols() )
      x.errorExit( mName, NEDIM ) ;
   n  = x.nRows() ;
   nr = x.nCols() ;
   nc = y.nRows() ;
   reset( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         r = 0.0 ;
         for ( k = 1 ; k <= n ; k++ )
            r += x(k,i) * y(j,k) ;
         mat(i,j) =  (REAL) r ;
      } // for j
   } // for i
   return *this ;
} // TMultTOf

matrix matrix::TMultT( const matrix& y ) M_CONST
{
   static char* mName = "TMultT" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.TMultTOf( *this, y ) ;
   return z ;
} // matrix::TMultT

matrix operator * ( const matrix& x,
                    const matrix &y )
{
   static char* mName = "op *" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z ;
   if ( x.isTrans() ) {
      if ( y.isTrans() )
         z.TMultTOf(x,y) ;
      else
         z.TMultOf(x,y) ;
   } else if ( y.isTrans() )
      z.multTOf(x,y) ;
   else
      z.multOf( x, y ) ;
   return z ;
} // matrix operator *

matrix matrix::multij( const matrix& y ) M_CONST
/***********************************************
   Element wise multiplication of this by y
***********************************************/
{
   static char *mName = "multijOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName  );
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   for ( INDEX j = 1 ; j <= nc ; j++ )
      for ( INDEX i = 1 ; i <= nr ; i++ )
         z(i,j) = mat(i,j) * y(i,j) ;
   return z ;
} // matrix::multij

matrix& matrix::multijEq( const matrix& y )
{
   static char* mName = "multijEq" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName  );
   INDEX nr = nRows(), nc = nCols() ;
   for ( INDEX j = 1 ; j <= nc ; j++ )
      for ( INDEX i = 1 ; i <= nr ; i++ )
         mat(i,j) *= y(i,j) ;
   return *this ;
} // matrix::multijEq

matrix matrix::divij( const matrix& y ) M_CONST
/**********************************************
      Element wise division of this by y
**********************************************/
{
   char *mName = "divijOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName  ) ;
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   REAL r ;
   for ( INDEX j = 1 ; j <= nc ; j++ ) {
      for ( INDEX i = 1 ; i <= nr ; i++ ) {
         if ( ( r = y(i,j) ) == 0.0 ) {
            y.errorij( i, j ) ;
            y.errorExit( mName, ZEROD ) ;
         } // if
         z(i,j) = mat(i,j) / r ;
      } // for j
   } // for i
   return z ;
} // matrix::divij

matrix& matrix::divijEq( const matrix& y )
{
   static char* mName = "divijEq" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName  ) ;
   INDEX nr = nRows(), nc = nCols() ;
   REAL r ;
   for ( INDEX j = 1 ; j <= nc ; j++ ) {
      for ( INDEX i = 1 ; i <= nr ; i++ ) {
         if ( ( r = y(i,j) ) == 0.0 ) {
            y.errorij( i, j ) ;
            y.errorExit( mName, ZEROD ) ;
         } // if
         mat(i,j) /= r ;
      } // for j
   } // for i
   return *this ;
} // matrix::divijEq

REAL matrix::inner( const matrix &y ) M_CONST
/********************************************
       Inner product of this and y
********************************************/
{
   char *mName = "inner" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   checkDims( y, mName ) ;
   INDEX nc = nCols(), nr = nRows() ;
   if ( nc != y.nCols() || nr != y.nRows() )
      errorExit( mName, NEDIM ) ;
   DOUBLE sum = 0.0 ;
   for ( INDEX j = 1; j <= nc ; j++ ) {
      for ( INDEX i = 1; i <= nr ; i++ )
         sum  += mat(i,j) * y(i,j) ;
   } // for j
   return (REAL) sum ;
} // REAL matrix::inner

matrix& matrix::rowMultOf( const matrix& x,
                           const matrix& dg )
/*********************************************
  Multiply the rows of x by the elements of
  column dg, this effectively treats dg as
  a diagonal matrix and forms the pre-
  product diag(dg) * x.
*********************************************/
{
   static char* mName = "rowMultOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   reset( nr, nc ) ;
   REAL r ;
   if ( dg.nCols() != 1 )
      dg.errorExit( mName, NOTVC ) ;
   if ( dg.nRows() != nr )
      dg.errorExit( mName, NEDIM ) ;
   for ( i = 1 ; i <= nr ; i++ ) {
      r = dg(i) ;
      for ( j = 1 ; j <= nc ; j++ )
         mat(i,j) = r * x(i,j) ;
   } // for i
   return *this ;
} // matrix::rowMultOf

matrix matrix::rowMult( const matrix& dg ) M_CONST
/*************************************************
   Multiply the rows of this by the elements of
   column dg, this effectively treats dg as a
   diagonal matrix and forms the pre-product
   diag(dg) * (*this).
*************************************************/
{
   static char* mName = "rowMult" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   z.rowMultOf( *this, dg ) ;
   return z ;
} // matrix::rowMult

matrix& matrix::colMultOf( const matrix& x,
                           const matrix &dg )
/***********************************************
  Multiply the columns of this by the elements
  of column dg, this effectively treats dg as
  a diagonal matrix and forms the post-product
  x * diag(dg).
***********************************************/
{
   static char *mName = "colMultOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   reset( nr, nc ) ;
   REAL r ;
   if ( dg.nCols() != 1 )
      dg.errorExit( mName, NOTVC ) ;
   if ( dg.nRows() != nc )
      dg.errorExit( mName, NEDIM ) ;
   for ( j = 1 ; j <= nc ; j++ ){
      r = dg(j) ;
      for ( i = 1 ; i <= nr ; i++ )
         mat(i,j) = r * x(i,j) ;
   } // for j
   return *this ;
} // matrix::colMultOf

matrix matrix::colMult( const matrix &dg ) M_CONST
/*************************************************
   Multiply the columns of this by the elements
   of column dg, this effectively treats dg as
   a diagonal matrix and forms the post-product
   (*this) * diag(dg).
*************************************************/
{
   static char *mName = "colMultOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols() ;
   matrix z( nr, nc ) ;
   z.colMultOf( *this, dg ) ;
   return z ;
} // matrix::colMult

#include <math.h>

matrix& matrix::step( REAL first, REAL increment )
{
   char *mName = "step" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), i, j ;
   REAL r = first ;
   for ( j = 1 ; j <= nc ; j++ )
      for (  i = 1; i <= nr; i++ ) {
         mat(i,j) = r ;
         r += increment ;
       } // for
   return *this ;
} // step matrix

matrix& joinOf( matrix& z, const matrix& x,
                const matrix& y )
{
   static char *mName = "joinOf" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX xnr = x.nRows(), xnc = x.nCols() ;
   INDEX ynr = y.nRows(), ync = y.nCols(), nc ;
   if ( ynr != xnr )
      x.errorExit( mName, NEDIM ) ;
   nc = xnc + ync ;
   z.reset( xnr, nc ) ;
   z.setSub( 1, xnr, 1, xnc, x ) ;
   z.setSub( 1, xnr, xnc+1, nc, y ) ;
   return z ;
} // joinOf

matrix& stackOf( matrix& z, const matrix& x,
                 const matrix& y )
{
   static char *mName = "stackOf" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX xnr = x.nRows(), xnc = x.nCols() ;
   INDEX ynr = y.nRows(), ync = y.nCols(), nr ;
   if ( ync != xnc )
      x.errorExit( mName, NEDIM ) ;
   nr = xnr + ynr ;
   z.reset( nr, xnc ) ;
   z.setSub( 1, xnr, 1, xnc, x ) ;
   z.setSub( xnr+1, nr, 1, xnc, y ) ;
   return z ;
} // stackOf

matrix operator | ( const matrix& x, const matrix &y )
{
   static char *mName = "op |" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols() + y.nCols() ;
   matrix z( nr, nc ) ;
   joinOf( z, x, y ) ;
   return z ;
} // matrix op |

matrix operator || ( const matrix& x, const matrix &y )
{
   static char *mName = "op ||" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows() + y.nRows(), nc = x.nCols() ;
   matrix z( nr, nc ) ;
   stackOf( z, x, y ) ;
   return z ;
} // matrix op ||


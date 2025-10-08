/**************************************************/
/*      matsvd.c source for SVD functions         */
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

#include "matsvd.hpp"

#include <math.h>

static DOUBLE at, bt, ct ;

DOUBLE PYTHAG( DOUBLE a, DOUBLE b )
{
  return
    ( ( at = fabs(a) )  > ( bt = fabs(b) ) )     ?
    ( ct = bt/at, at * sqrt( 1.0 + ct * ct ) )   :
    ( bt ? ( ct = at/bt, bt * sqrt( 1.0 + ct * ct ) ) : 0.0 )
  ;
} // inline pythag

static void biDiag( matrix& a, matrix& w, matrix& rv1, REAL& anorm )
{
   REAL f, g = 0.0, h, r, s = 0.0, scale = 0.0 ;
   INDEX m = a.nRows(), n = a.nCols(), i, j, k, l ;
   anorm = 0.0 ;
   for ( i = 1 ; i <= n ; i++ ) {
      l = i+1;
      rv1(i) = scale * g ;
      g = s = scale = 0.0 ;
      if ( i <= m ) {
         for ( k = i ; k <= m ; k++ )
            scale += fabs( a(k,i) ) ;
         if ( scale ) {
            for ( k = i ; k <= m ; k++ ) {
               a(k,i) /= scale;
               s += a(k,i) * a(k,i) ;
            } // for k
            f = a(i,i) ;
            g = sqrt(s) ;
            if ( f >= 0.0 )
               g = - g ;
            h = f * g - s ;
            a(i,i) = f - g ;
            if ( i != n ) {
               for ( j = l ; j <= n ; j++ ) {
                  for ( s = 0.0 , k = i ; k <= m ; k++ )
                      s += a(k,i) * a(k,j) ;
                  f = s / h ;
                  for ( k = i ; k <= m ; k++ )
                      a(k,j) += f * a(k,i) ;
               } // for j
            } // if i != n
            for ( k = i ; k <= m ; k++ )
                a(k,i) *= scale ;
         } // if scale
      } // if i <= m
      w(i) = scale * g ;
      g = s = scale = 0.0 ;
      if ( i <= m && i != n ) {
         for ( k = l ; k <= n ; k++ )
             scale += fabs( a(i,k) ) ;
         if ( scale ) {
            for ( k = l ; k <= n ; k++ ) {
               a(i,k) /= scale ;
               s += a(i,k) * a(i,k) ;
            } // for k
            f = a(i,l) ;
            g = sqrt(s) ;
            if ( f >= 0.0 )
               g = -g ;
            h = f * g - s ;
            a(i,l) = f - g ;
            for ( k = l ; k <= n ; k++ )
                rv1(k) = a(i,k) / h ;
            if ( i != m ) {
               for ( j = l ; j <= m ; j++ ) {
                  for ( s = 0.0 , k = l ; k <= n ; k++ )
                      s += a(j,k) * a(i,k);
                  for ( k = l ; k <= n ; k++ )
                      a(j,k) += s * rv1(k);
               } // for j
            } // if i != m
            for ( k = l ; k <= n ; k++ )
                a(i,k) *= scale;
         } // if scale
      } // if i != m && i != n
      r = fabs( w(i) ) + fabs( rv1(i) ) ;
      if ( r > anorm )
         anorm = r ;
   } // for i
} // biDiag

static void initialWV( matrix& a, matrix& w, matrix& v, matrix& rv1 )
{
   INDEX m = a.nRows(), n = a.nCols(), i, j, k, l ;
   REAL f, g = 0.0, s ;
   for ( i = n ; i >= 1 ; i-- ) {
      l = i + 1 ;
      if ( i < n ) {
         if ( g ) {
            for ( j = l ; j <= n ; j++ )
               v(j,i) = ( a(i,j) / a(i,l) ) / g ;
               // double division to reduce underflow
            for ( j = l ; j <= n ; j++ ) {
               for ( s = 0.0, k =l ; k <= n ; k++ )
                  s += a(i,k) * v(k,j) ;
               for ( k = l ; k <= n ; k++ )
                  v(k,j) += s * v(k,i) ;
            } // for j
         } // if g
         for ( j = l ; j <= n ; j++ )
            v(i,j) = v(j,i) = 0.0 ;
      } // if i < n
      v(i,i) = 1.0 ;
      g = rv1(i) ;
   } // for i
   for ( i = n ; i >= 1 ; i-- ) {
      l = i + 1 ;
      g = w(i) ;
      if ( i < n ) {
         for ( j = l ; j <= n ; j++ )
            a(i,j)=0.0 ;
      } // if
      if ( g ) {
         g = 1.0 / g ;
         if ( i != n ) {
            for ( j = l ; j <= n ; j++ ) {
               for ( s = 0.0 , k = l ; k <= m ; k++ )
                  s += a(k,i) * a(k,j);
               f = ( s / a(i,i) ) * g ;
               for ( k = i ; k <= m ; k++ )
                  a(k,j) += f * a(k,i) ;
            } // for j
         } // if i != n
         for ( j = i ; j <= m ; j++ )
            a(j,i) *= g ;
      } else {
         for ( j = i ; j <= m ; j++ )
            a(j,i) = 0.0 ;
      } // else
      a(i,i) += 1.0 ;
   } // for i
} // initialWV

INDEX svdcmp( matrix& a, matrix& w, matrix& v, matError& error )
{
   static char *mName = "svdcmp" ;
   matFunc func( mName ) ; a.debugInfo( func ) ;
   INDEX m = a.nRows(), n = a.nCols() ;
   INDEX  flag, i, its, j, jj, k, l, nm ;
   REAL   c, f, h, s, x, y, z ;
   REAL   anorm=0.0, g=0.0, r ;
   matrix rv1(n) ;

   error = NOERROR ;
   if ( m < n ) {
      error = NRANG ;
      return 0 ;
   } // if

   w.reset( n ) ;
   v.reset( n, n ) ;

   biDiag( a, w, rv1, anorm ) ;
   initialWV( a, w, v, rv1 ) ;

/* 

   for ( i = 1 ; i <= n ; i++ ) {
      l = i+1;
      rv1(i) = scale * g ;
      g = s = scale = 0.0 ;
      if ( i <= m ) {
         for ( k = i ; k <= m ; k++ )
            scale += fabs( a(k,i) ) ;
         if ( scale ) {
            for ( k = i ; k <= m ; k++ ) {
               a(k,i) /= scale;
               s += a(k,i) * a(k,i) ;
            } // for k
            f = a(i,i) ;
            g = sqrt(s) ;
            if ( f >= 0.0 )
               g = - g ;
            h = f * g - s ;
            a(i,i) = f - g ;
            if ( i != n ) {
               for ( j = l ; j <= n ; j++ ) {
                  for ( s = 0.0 , k = i ; k <= m ; k++ )
                      s += a(k,i) * a(k,j) ;
                  f = s / h ;
                  for ( k = i ; k <= m ; k++ )
                      a(k,j) += f * a(k,i) ;
               } // for j
            } // if i != n
            for ( k = i ; k <= m ; k++ )
                a(k,i) *= scale ;
         } // if scale
      } // if i <= m
      w(i) = scale * g ;
      g = s = scale = 0.0 ;
      if ( i <= m && i != n ) {
         for ( k = l ; k <= n ; k++ )
             scale += fabs( a(i,k) ) ;
         if ( scale ) {
            for ( k = l ; k <= n ; k++ ) {
               a(i,k) /= scale ;
               s += a(i,k) * a(i,k) ;
            } // for k
            f = a(i,l) ;
            g = sqrt(s) ;
            if ( f >= 0.0 )
               g = -g ;
            h = f * g - s ;
            a(i,l) = f - g ;
            for ( k = l ; k <= n ; k++ )
                rv1(k) = a(i,k) / h ;
            if ( i != m ) {
               for ( j = l ; j <= m ; j++ ) {
                  for ( s = 0.0 , k = l ; k <= n ; k++ )
                      s += a(j,k) * a(i,k);
                  for ( k = l ; k <= n ; k++ )
                      a(j,k) += s * rv1(k);
               } // for j
            } // if i != m
            for ( k = l ; k <= n ; k++ )
                a(i,k) *= scale;
         } // if scale
      } // if i != m && i != n
      r = fabs( w(i) ) + fabs( rv1(i) ) ;
      if ( r > anorm )
         anorm = r ;
   } // for i
   for ( i = n ; i >= 1 ; i-- ) {
      l = i + 1 ;
      if ( i < n ) {
         if ( g ) {
            for ( j = l ; j <= n ; j++ )
               v(j,i) = ( a(i,j) / a(i,l) ) / g ;
               // double division to reduce underflow
            for ( j = l ; j <= n ; j++ ) {
               for ( s = 0.0, k =l ; k <= n ; k++ )
                  s += a(i,k) * v(k,j) ;
               for ( k = l ; k <= n ; k++ )
                  v(k,j) += s * v(k,i) ;
            } // for j
         } // if g
         for ( j = l ; j <= n ; j++ )
            v(i,j) = v(j,i) = 0.0 ;
      } // if i < n
      v(i,i) = 1.0 ;
      g = rv1(i) ;
   } // for i
   for ( i = n ; i >= 1 ; i-- ) {
      l = i + 1 ;
      g = w(i) ;
      if ( i < n ) {
         for ( j = l ; j <= n ; j++ )
            a(i,j)=0.0 ;
      } // if
      if ( g ) {
         g = 1.0 / g ;
         if ( i != n ) {
            for ( j = l ; j <= n ; j++ ) {
               for ( s = 0.0 , k = l ; k <= m ; k++ )
                  s += a(k,i) * a(k,j);
               f = ( s / a(i,i) ) * g ;
               for ( k = i ; k <= m ; k++ )
                  a(k,j) += f * a(k,i) ;
            } // for j
         } // if i != n
         for ( j = i ; j <= m ; j++ )
            a(j,i) *= g ;
      } else {
         for ( j = i ; j <= m ; j++ )
            a(j,i) = 0.0 ;
      } // else
      a(i,i) += 1.0 ;
   } // for i

*/

   for ( k = n ; k >= 1 ; k-- ) {
      for ( its = 1 ; its <= 30 ; its++ ) {
         flag = 1 ;
         for ( l = k ; l >= 1 ; l-- ) {
            nm = l - 1 ;
            if ( fabs( rv1(l) ) + anorm == anorm) {
               flag = 0 ;
               break;
            } // if
            if ( fabs( w(nm) ) + anorm == anorm )
               break;
         } // for l
         if ( flag ) {
            c = 0.0 ;
            s = 1.0;
            for ( i = l ; i <= k ; i++ ) {
               f = s * rv1(i) ;
               if ( fabs(f) + anorm != anorm ) {
                  g = w(i) ;
                  h = PYTHAG(f,g) ;
                  w(i) = h ;
                  h = 1.0 / h ;
                  c = g * h ;
                  s = ( -f * h ) ;
                  for ( j = 1 ; j <= m ; j++ ) {
                     y = a(j,nm) ;
                     z = a(j,i) ;
                     a(j,nm) = y * c + z * s ;
                     a(j,i) = z * c - y * s ;
                  } // for j
               } // if fabs(f)
            } // for i
         } // if flag
         z = w(k) ;
         if ( l == k ) {
            if ( z < 0.0 ) {
               w(k) = -z ;
               for ( j = 1 ; j <= n ; j++ )
                  v(j,k) = (-v(j,k)) ;
            } // if z < 0
            break;
         } // if l == k
         if ( its == 30 ) {
           error = NCONV ;
           return 0 ;
	 } // if  
         x = w(l);
         nm = k - 1 ;
         y = w(nm) ;
         g = rv1(nm) ;
         h = rv1(k) ;
         f = ( (y-z)*(y+z) + (g-h)*(g+h) ) / ( 2.0 * h * y ) ;
         g = PYTHAG(f,1.0) ;
         r = ( f >= 0.0 ? g : - g ) ;
         f= ( (x-z)*(x+z) + h * ( ( y / ( f + r ) ) - h ) ) / x ;
         c = s = 1.0 ;
         for ( j = l ; j <= nm ; j++ ) {
            i = j + 1 ;
            g = rv1(i) ;
            y = w(i) ;
            h = s * g ;
            g = c * g ;
            z = PYTHAG(f,h) ;
            rv1(j) = z ;
            c = f / z ;
            s = h / z ;
            f = x * c + g * s ;
            g = g * c - x * s ;
            h = y * s ;
            y = y * c ;
            for ( jj = 1 ; jj <= n ; jj++ ) {
               x = v(jj,j) ;
               z = v(jj,i);
               v(jj,j) = x * c + z * s ;
               v(jj,i)= z * c - x * s ;
            } // for jj
            z = PYTHAG(f,h) ;
            w(j) = z ;
            if (z) {
               z = 1.0 / z ;
               c = f * z ;
               s = h * z ;
            } // if
            f = ( c * g ) + ( s * y ) ;
            x = ( c * y ) - ( s * g ) ;
            for ( jj = 1 ; jj <= m ; jj++ ) {
               y = a(jj,j) ;
               z = a(jj,i) ;
               a(jj,j) = y * c + z * s ;
               a(jj,i) = z * c - y * s ;
            } // for jj
         } // for j
         rv1(l) = 0.0 ;
         rv1(k) = f ;
         w(k) = x ;
      } // for its
   } // for k
   return OK ;
} // svdcmp

INDEX svdBackSub( const matrix& a, const matrix& w, 
                  const matrix& v, const matrix& b, 
                  matrix& x, matError& error )
/**************************************************************
   Assumes a, w and v are svd decomp of A. Solves Ax=b.
   Ignores components with zero singular values.
   Overwrites x with solution.
**************************************************************/
{
   static char *mName = "svdBackSub" ;
   matFunc func( mName ) ; a.debugInfo( func ) ;
   error = NOERROR ;
   INDEX nc = a.nCols(), nr = a.nRows() ;
   if ( nc != w.nRows() || nc != v.nRows() || 
        nc != v.nCols() || nr != b.nRows() ) {
      error = NEDIM ;
      return 0 ;
   } // if
   if ( b.nCols() != 1 ) {
      error = NOTVC ;
      return 1 ;
   } // if
   // form tmp = w^-1 a'b, passing over zeros in w 
   matrix tmp( nc ) ;
   refMatrix m( a ) ;
   REAL r ;
   INDEX i ;
   for ( i = 1 ; i <= nc ; i++ ) {
      r = 0.0 ;
      if ( w(i) != 0.0 ) {
         m.refCol(i) ;
         r = m.inner(b) ;
	 r /= w(i) ;
      } // if
      tmp(i) = r ;
   } // if
   x.multOf( v, tmp ) ;
   return OK ;
} // svdBackSub

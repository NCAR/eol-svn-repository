/**************************************************/
/*    matkron.c source for Kronecker products     */
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

matrix& matrix::kronOf( const matrix& x, const matrix& y,
                        transposition opt )
{
   static char *mName = "kronOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX xrow = x.nRows(), xcol = x.nCols() ;
   INDEX yrow = y.nRows(), ycol = y.nCols() ;
   INDEX nr, nc, i, j, zi, zj, xi, xj, yi, yj ;
   REAL r ;
   matError error = NOERROR ;
   switch ( opt ) {
      case TRANS0 :
         nr = xrow * yrow ;
         nc = xcol * ycol ;
         break ;
      case TRANS1 :
         nr = xcol * yrow ;
         nc = xrow * ycol ;
         break ;
      case TRANS2 :
         nr = xrow * ycol ;
         nc = xcol * yrow ;
         break ;
      case TRANS12 :
         nr = xcol * ycol ;
         nc = xrow * yrow ;
         break ;
      default :
         error = NGPAR ;
         break ;
   } // switch
   if ( error )
     errorExit( mName, error ) ;
   reset( nr, nc ) ;
   if ( opt == TRANS0 ) {
      for ( xi = 1, i = 1 ; xi <= xrow ; xi++, i += yrow ) {
         for ( xj = 1, j = 1 ; xj <= xcol ; xj++, j += ycol ) {
            r = x( xi, xj ) ;
            for ( yi = 1, zi = i ; yi <= yrow ; yi++, zi++ )
               for ( yj = 1, zj = j ; yj <= ycol ; yj++, zj++ )
                  mat( zi, zj ) = r * y( yi, yj ) ;
         } // for xj
      } // for xi
   } else if ( opt == TRANS1 ) {
      for ( xi = 1, i = 1 ; xi <= xcol ; xi++, i += yrow ) {
         for ( xj = 1, j = 1 ; xj <= xrow ; xj++, j += ycol ) {
            r = x( xj, xi ) ;
            for ( yi = 1, zi = i ; yi <= yrow ; yi++, zi++ )
               for ( yj = 1, zj = j ; yj <= ycol ; yj++, zj++ )
                  mat( zi, zj ) = r * y( yi, yj ) ;
         } // for xj
      } // for xi
   } else if ( opt == TRANS2 ) {
      for ( xi = 1, i = 1 ; xi <= xrow ; xi++, i += ycol ) {
         for ( xj = 1, j = 1 ; xj <= xcol ; xj++, j += yrow ) {
            r = x( xi, xj ) ;
            for ( yi = 1, zi = i ; yi <= ycol ; yi++, zi++ )
               for ( yj = 1, zj = j ; yj <= yrow ; yj++, zj++ )
                  mat( zi, zj ) = r * y( yj, yi ) ;
         } // for xj
      } // for xi
   } else { //  opt == TRANS12
      for ( xi = 1, i = 1 ; xi <= xcol ; xi++, i += ycol ) {
         for ( xj = 1 , j = 1 ; xj <= xrow ; xj++, j += yrow ) {
            r = x( xj, xi ) ;
            for ( yi = 1, zi = i ; yi <= ycol ; yi++, zi++ )
               for ( yj = 1, zj = j ; yj <= yrow ; yj++, zj++ )
                  mat( zi, zj ) = r * y( yj, yi ) ;
         } // for xj
      } // for xi
   } // else
   return *this ;
} // kronOf

matrix matrix::kron( const matrix& y ) M_CONST
{
   static char *mName = "kron" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.kronOf( *this, y ) ;
   return z ;
} // matrix::kron

matrix matrix::TKron( const matrix& y ) M_CONST
{
   static char *mName = "TKron" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.kronOf( *this, y, TRANS1 ) ;
   return z ;
} // matrix::TKron

matrix matrix::kronT( const matrix& y ) M_CONST
{
   static char *mName = "kronT" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.kronOf( *this, y, TRANS2 ) ;
   return z ;
} // matrix::kronT

matrix matrix::TKronT( const matrix& y ) M_CONST
{
   static char *mName = "kronT" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   z.kronOf( *this, y, TRANS12 ) ;
   return z ;
} // matrix::TKronT

matrix matrix::operator % ( const matrix& y ) M_CONST
{
   static char *mName = "op %" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z ;
   if ( isTrans() ) {
      if ( y.isTrans() )
         z.kronOf( *this, y, TRANS12 ) ;
      else
         z.kronOf( *this, y, TRANS1 ) ;
   } else if ( y.isTrans() )
      z.kronOf( *this, y, TRANS2 ) ;
   else
      z.kronOf( *this, y ) ;
   return z ;
} // matrix % (kron)

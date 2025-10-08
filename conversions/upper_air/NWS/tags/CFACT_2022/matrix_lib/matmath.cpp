/**************************************************/
/*  matmath.c source for transcendental functions */
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

#include "matmath.hpp"

#include <math.h>

/***********************************************/
/*            Transcendental Functions         */
/***********************************************/

enum transcendental {
   LOG, LOG10, EXP, SIN, COS, TAN, ASIN, ACOS, ATAN,
   SINH, COSH, TANH
} ; // transcendental

static char* transcNames[] = {
   "Log", "Log10", "Exp",
   "Sin", "Cos", "Tan",
   "ArcSin", "ArcCos", "ArcTan",
   "Sinh", "Cosh", "Tanh"
} ;

void transcend( matrix& z, const matrix& x, int fn )
{
   static char *mName = "transcend" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   z.reset( nr, nc ) ;
   int error ;
   REAL eps = matrixEps(), maxExp = -log( eps ) ;
   // do tests ---- these need more work !!!
   switch ( fn ) {
      case LOG :
      case LOG10 :
         error = x.compare( eps ) != GREATER ;
         break ;
      case EXP :
      case SINH :
      case COSH :
      case TANH :
         error = x.compare( maxExp ) != LESS || 
                 x.compare( -maxExp ) != GREATER ;
         break ;
      case ASIN :
      case ACOS :
         error = x.compare( 1.0 ) & GREATER  ||
                 x.compare( -1.0 ) & LESS ;
         break ;
      case TAN :     // ????
      default :
         error = FALSE ;
         break ;
   } // switch
   if ( error )
      x.errorExit( transcNames[ fn ], TRANS ) ;
   /*****************************/
   /* Loop can be optimised if  */
   /* willing to exploit col    */
   /* major form of matrices    */
   /*****************************/
   for ( j = 1 ; j <= nc ; j++ ) {
      switch ( fn ) {
         case LOG :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) log( x(i,j) ) ;
            break ;
         case LOG10 :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) log10( x(i,j) ) ;
            break ;
         case EXP :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) exp( x(i,j) ) ;
            break ;
         case SIN :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) sin( x(i,j) ) ;
            break ;
         case COS :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) cos( x(i,j) ) ;
            break ;
         case TAN :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) tan( x(i,j) ) ;
            break ;
         case ASIN :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) asin( x(i,j) ) ;
            break ;
         case ACOS :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) acos( x(i,j) ) ;
            break ;
         case ATAN :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) atan( x(i,j) ) ;
            break ;
         case SINH :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) sinh( x(i,j) ) ;
            break ;
         case COSH :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) cosh( x(i,j) ) ;
            break ;
         case TANH :
            for ( i = 1 ; i <= nr ; i++ )
               z(i,j) = (REAL) tanh( x(i,j) ) ;
            break ;
      } // switch
   } // for i
   return ;
} // matrix transcend

#ifdef __cplusplus

matrix abs( matrix& x )
{
   static char *mName = "abs" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   matrix z( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ )
         z(i,j) = (REAL) fabs( x(i,j) ) ;
   return z ;
} // matrix abs


matrix log( const matrix& x )
{ 
   static char *mName = "log" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG ) ;
   return z ;
} // log

matrix exp( const matrix& x )
{ 
   static char *mName = "exp" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, EXP ) ;
   return z ;
} // exp

matrix log10( const matrix& x )
{ 
   static char *mName = "log10" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG10 ) ;
   return z ;
} // log10m

matrix ln( const matrix& x )
{ 
   static char *mName = "ln" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG ) ;
   return z ;
} // ln

matrix sin( const matrix& x )
{ 
   static char *mName = "sin" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, SIN ) ;
   return z ;
} //sin

matrix cos( const matrix& x )
{ 
   static char *mName = "cos" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, COS ) ;
   return z ;
} // cos

matrix tan( const matrix& x )
{ 
   static char *mName = "tan" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, TAN ) ;
   return z ;
} // tan

matrix asin( const matrix& x )
{ 
   static char *mName = "asin" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ASIN ) ;
   return z ;
} // asinm

matrix acos( const matrix& x )
{ 
   static char *mName = "acos" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ACOS ) ;
   return z ;
} // acos

matrix atan( const matrix& x )
{ 
   static char *mName = "atan" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ATAN ) ;
   return z ;
} // atan

matrix sinh( const matrix& x )
{ 
   static char *mName = "sinh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, SINH ) ;
   return z ;
} // sinh

matrix cosh( const matrix& x )
{ 
   static char *mName = "cosh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, COSH ) ;
   return z ;
} // cosh

matrix tanh( const matrix& x )
{ 
   static char *mName = "tanh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, TANH ) ;
   return z ;
} // tanh

#else

matrix absm( matrix& x )
{
   static char *mName = "absm" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   matrix z( nr, nc ) ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ )
         z(i,j) = (REAL) fabs( x(i,j) ) ;
   return z ;
} // matrix absm


matrix logm( const matrix& x )
{ 
   static char *mName = "log" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG ) ;
   return z ;
} // log

matrix expm( const matrix& x )
{ 
   static char *mName = "exp" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, EXP ) ;
   return z ;
} // exp

matrix log10m( const matrix& x )
{ 
   static char *mName = "log10" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG10 ) ;
   return z ;
} // log10m

matrix lnm( const matrix& x )
{ 
   static char *mName = "ln" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, LOG ) ;
   return z ;
} // ln

matrix sinm( const matrix& x )
{ 
   static char *mName = "sin" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, SIN ) ;
   return z ;
} //sin

matrix cosm( const matrix& x )
{ 
   static char *mName = "cos" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, COS ) ;
   return z ;
} // cos

matrix tanm( const matrix& x )
{ 
   static char *mName = "tan" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, TAN ) ;
   return z ;
} // tan

matrix asinm( const matrix& x )
{ 
   static char *mName = "asin" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ASIN ) ;
   return z ;
} // asinm

matrix acosm( const matrix& x )
{ 
   static char *mName = "acos" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ACOS ) ;
   return z ;
} // acos

matrix atanm( const matrix& x )
{ 
   static char *mName = "atan" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, ATAN ) ;
   return z ;
} // atan

matrix sinhm( const matrix& x )
{ 
   static char *mName = "sinh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, SINH ) ;
   return z ;
} // sinh

matrix coshm( const matrix& x )
{ 
   static char *mName = "cosh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, COSH ) ;
   return z ;
} // cosh

matrix tanhm( const matrix& x )
{ 
   static char *mName = "tanh" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   transcend( z, x, TANH ) ;
   return z ;
} // tanh

#endif


matrix& matrix::roundOf( const matrix& x, truncation option )
{
   static char *mName = "roundOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   if ( x.isNull() )
      x.errorExit( mName, NULRF ) ;
   reset( nr, nc ) ;
   DOUBLE d, tmp ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         d = (DOUBLE) x(i,j) ;
         switch ( option ) {
            case ROUND :
               tmp = floor( d ) ;
               d = ( d - tmp > 0.5 ) ? tmp + 1.0 : tmp ;
               break ;
            case FIX :
               d = ( d < 0 ) ? ceil( d ) : floor( d ) ;
               break ;
            case FLOOR :
               d = floor( d ) ;
               break ;
            case CEIL :
               d = ceil( d ) ;
               break ;
         } // switch
         mat(i,j) = (REAL) d ;
      } // for j
   } // for i
   return *this ;
} // matrix roundOf

#ifdef __cplusplus
matrix sign( const matrix& x )
#else
matrix signm( const matrix& x )
#endif
{
   static char *mName = "signm(matrix)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   z.signOf( x ) ;
   return z ;
} // sign

#ifdef __cplusplus
matrix round( const matrix& x )
#else
matrix roundm( const matrix& x )
#endif
{
   static char *mName = "roundm(matrix)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   z.roundOf( x, ROUND ) ;
   return z ;
} // round

#ifdef __cplusplus
matrix fix( const matrix& x )
#else
matrix fixm( const matrix& x )
#endif
{
   static char *mName = "fixm(matrix)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   z.roundOf( x, FIX ) ;
   return z ;
} //  fix

#ifdef __cplusplus
matrix floor( const matrix& x )
#else
matrix floorm( const matrix& x )
#endif
{
   static char *mName = "floorm(matrix)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   z.roundOf( x, FLOOR ) ;
   return z ;
} // floor

#ifdef __cplusplus
matrix ceil( const matrix& x )
#else
matrix ceilm( const matrix& x )
#endif
{
   static char *mName = "ceilm(matrix)" ;
   matFunc func( mName ) ; x.debugInfo( func ) ;
   matrix z( x.nRows(), x.nCols() ) ;
   z.roundOf( x, CEIL ) ;
   return z ;
} // ceil

matrix& matrix::remOf( const matrix& x, const matrix& y )
{
   char *mName = "remOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   x.checkDims( y, mName ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   reset( nr, nc ) ;
   DOUBLE d ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         if ( y(i,j) == 0 )
            errorExit( mName, ZEROD ) ;
         d = x(i,j) / y(i,j) ;
         d = ( d < 0.0 ) ? ceil( d ) : ( ( d > 0.0 ) ? floor( d ) : d ) ;
         mat(i,j) = x(i,j) - y(i,j) * d ;
      } // for j
   } // for j
   return *this ;
} // matrix remOf

matrix matrix::rem( const matrix& y ) M_CONST
{
   char *mName = "rem" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matrix z( nRows(), nCols() ) ;
   z.remOf( *this, y ) ;
   return z ;
} // matrix::rem

matrix& matrix::signOf( const matrix& x )
{
   static char *mName = "signOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = x.nRows(), nc = x.nCols(), i, j ;
   if ( x.isNull() )
      x.errorExit( mName, NULRF ) ;
   reset( nr, nc ) ;
   REAL d ;
   for ( i = 1 ; i <= nr ; i++ ) {
      for ( j = 1 ; j <= nc ; j++ ) {
         d = x(i,j) ;
         mat(i,j) = ( d < 0.0 ) ? -1.0 : ( ( d > 0.0 ) ? 1.0 : 0.0 ) ;
      } // for j
   } // for j
   return *this ;
} // matrix signOf

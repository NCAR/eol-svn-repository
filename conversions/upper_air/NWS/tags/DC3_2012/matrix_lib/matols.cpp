/**************************************************/
/*       matols.c source for matols class         */
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

#include "matols.hpp"
#include <math.h>

/*************************************************************/
/*                     matOls methods                      */
/*************************************************************/

void matOls::setNames( void )
{
   Y.name( "olsY" ) ;
   X.name( "olsX" ) ;
   R.name( "olsR" ) ;
   Rinv.name( "olsR-1" ) ;
   beta.name( "olsBta" ) ;
   resid.name( "olsRes" ) ;
   V.name( "olsV" ) ;
   VSqrt.name( "olsVsq" ) ;
   TSS.name( "olsTSS" );
   RSS.name( "olsRSS" ) ;
   YMean.name( "olsYmn" ) ;
   SE.name( "olsSE" ) ;
} // matOls setNames

#ifdef __cplusplus
matOls::matOls( void ) : matObject()
#else
matOls::matOls( void ) : ()
#endif
{
   status = 0 ;
   setNames() ;
} // matOls

#ifdef __cplusplus
matOls::matOls( const matrix& y, const matrix& x ) : matObject()
#else
matOls::matOls( const matrix& y, const matrix& x ) : ()
#endif
{
   status = 0 ;
   assign( y, x ) ;
   setNames() ;
} // matOls( matrix& )

matOls::~matOls( void ) {} // ~matOls

#ifdef __cplusplus
matOls::matOls( matOls& ols ) : matObject( ols )
#else
matOls::matOls( matOls& ols ) : ( ols )
#endif
{
   errorExit( "matOls copy", NIMPL ) ;
} // matOls copy constr

void matOls::operator = ( matOls& ols )
{
   nm = ols.nm ;
   errorExit( "matOls =", NIMPL ) ;
} // matOls =

void matOls::decompose( void )
{
   errorExit( "matOls::decomp", NIMPL ) ;
} // matOls decompose

outFile& matOls::info( outFile& f ) M_CONST
{
   errorExit( "matOls::info", NIMPL ) ;
   return f ;
} // matOls::info

REAL matOls::varAdd( const matrix& z, INDEX n )
{
   z.errorExit( "matOls::varAdd", NIMPL ) ;
   return (REAL) n ;
} // matOls::varAdd

void matOls::initial( void )
{
   static char *mName = "initial" ;
   matrix XMax, XMin ;
   if ( X.nRows() != Y.nRows() )
      errorExit( mName, NEDIM ) ;
   INDEX j ;
   nObs = X.nRows() ;
   nVars = X.nCols() ;
   nDep = Y.nCols() ;
   dof = nObs - nVars ;
   tol = matrixTol() ;
   // constant true iff X(j) is constant for some j
   XMax = X.colMax() ;
   XMin = X.colMin() ;
   for ( constant = 0, j = 1 ; !constant && ( j <= nVars ) ; j++ )
      constant = ( XMax(j) == XMin(j) ) ;
   YMean.colMeanOf( Y ) ;
   TSS.colSqDevOf( Y, YMean ) ;
   RSS.reset( nDep ) ;
   beta.reset( nVars, nDep ) ;
   resid.reset( nObs, nDep ) ;
   R.reset( nVars, nVars ) ;
   Rinv.reset( nVars, nVars ) ;
   V.reset( nVars, nVars ) ;
   VSqrt.reset( nVars ) ;
   SE.reset( nDep ) ;
   status = ASSIGNED ;
   return ;
} // matOls::initial

void matOls::assign( const matrix& y, const matrix& x )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; // debugInfo( func ) ;
   Y = y ;
   X = x ;
   initial() ;
} // matOls::assign

void matOls::capture( matrix& y, matrix& x )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; // debugInfo( func ) ;
   Y.capture(y) ;
   X.capture(x) ;
   initial() ;
} // matOls::capture

INDEX matOls::ok( void )
{
   decompose() ;
   return !(status & SINGULAR) ;
} // matOls::ok

INDEX matOls::ok( char *mName )
{
   decompose() ;
   if ( status & SINGULAR )
      errorExit( mName, SINGM ) ;
   return OK ;
} // matOls::ok

REAL matOls::setTol( REAL newTol )
{
   REAL oldTol = tol ;
   if ( newTol >= 0.0 )
      tol = newTol ;
   return oldTol ;
} // matOls::setTol

void matOls::clear( void )
{
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( status != 0 ) {
      X.clear() ;
      Y.clear() ;
      R.clear() ;
      Rinv.clear() ;
      beta.clear() ;
      YMean.clear() ;
      RSS.clear() ;
      TSS.clear() ;
      V.clear() ;
      VSqrt.clear() ;
      SE.clear() ;
      status = 0 ;
      constant = 0 ;
   } // if
} // clear

charArray& matOls::name( char *newName )
{
   static char *mName = "name" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( newName != 0 )
      nm = newName ;
   return nm ;
} // matOls::name

char* matOls::nameStr( void ) M_CONST
{
    return nm.array() ;
} // matOls::name

matrix& matOls::coeff( matrix& b )
{
   static char *mName = "coeff" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) )
      b = beta ;
   return b ;
} // coeff

outFile& matOls::olsInfo( outFile& f, char *cName ) M_CONST
{
   if ( matListCtrl() > 4 )
      return f ;
   objectInfo( f ) ;
   putName( nameStr(), f ) ;
   putName( cName, f ) ;
   putField( status, f ) ;
   if ( status & ASSIGNED ) {
     putField( Y.identity(), f ) ;
     putField( X.identity(), f ) ;
   } else
     putField( "NA", f ) ;
   f.newLine() ;
   return f ;
} // matOls::olsInfo

outFile& matOls::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errorExit( mName, NIMPL ) ;
   return f ;
} // matOls::put

inFile& matOls::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errorExit( mName, NIMPL ) ;
   return f ;
} // matOls::get

void matOls::formResid( void )
{
   static char *mName = "formRes" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   ok( mName ) ;
   if ( !( status & RESIDUALS ) ) {
      INDEX i ;
      resid = Y ;
      resid -= X * beta ;
      RSS.colSqrOf( resid ) ;
      SE.reset( nDep ) ;
      REAL r ;
      for ( i = 1 ; i <= nDep ; i++ ) {
         r = RSS(i) ;
         SE(i) = sqrt( r / dof ) ;
      } // for
      status |= RESIDUALS ;
   } // if
} // matOls::formResid

matrix& matOls::residuals( matrix& res )
{
   static char *mName = "resid" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   res = resid ;
   return res ;
} // matOls::residuals

matrix& matOls::fitted( matrix& fit )
{
   static char *mName = "fitted" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   fit =  Y ;
   fit -= resid ;
   return fit ;
} // matOls::fitted

REAL matOls::se( INDEX i )
{
   static char *mName = "se" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   return SE(i) ;
} // matOls::se

REAL matOls::rss( INDEX i )
{
   static char *mName = "rss" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   return RSS(i) ;
} // matOls::rss

REAL matOls::rsq( INDEX i )
{
   static char *mName = "rsq" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   REAL r = RSS(i), t = TSS(i), y = YMean(i) ;
   if ( constant )
      return 1.0 - r / t ;
   else
      return 1.0 - r / ( t + y * y ) ;
} // matOls::rsq

REAL matOls::rBarSq( INDEX i )
{
   static char *mName = "rBarSq" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   REAL r = RSS(i), t = TSS(i), y = YMean(i) ;
   if ( constant )
      return 1.0 - ( r * ( nObs - 1 ) ) / ( t * dof ) ;
   else
      return 1.0 - ( r * ( nObs - 1 ) )
                   / ( ( t + y * y ) * dof ) ;
} // matOls::rBarSq

REAL matOls::tss( INDEX i )
{
   static char *mName = "tss" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   ok( mName ) ;
   return TSS(i) ;
} // matOls::rss

matrix& matOls::stdErr( matrix& std )
{
   std.reset( nVars, nDep ) ;
   INDEX i, j ;
   REAL s ;
   formV() ;
   for ( j = 1 ; j <= nDep ; j++ ) {
      s = SE(j) ;
      for ( i = 1 ; i <= nVars ; i++ )
         std(i,j) = s * VSqrt(i) ;
   } // for
   return std ;
} // matOls::stdErr

matrix& matOls::cov( matrix& cv, INDEX i )
{
   static char *mName = "cov" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formV() ;
   REAL s = SE(i) ;
   cv = V ;
   cv *= s * s ;
   return cv ;
} // matOls::cov

REAL matOls::dw( INDEX j )
/*************************
  Durbin-Watson Test
*************************/
{
   static char *mName = "dw" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   DOUBLE e, num = 0.0, r = resid(1,j), denom ;
   INDEX i ;
   formResid() ;
   denom = r * r ;
   for ( i = 2 ; i <= nObs ; i++ ) {
      e = resid(i,j) ;
      denom += e * e ;
      e -= resid(i-1,j) ;
      num += e * e ;
   } // for i
   return num  / denom ;
} // matOls::dw

REAL matOls::tTest( const matrix& w, REAL r, INDEX n )
/************************************************
  Return classical t-Value for hypothesis
                w'beta = r
  where beta is the set of coeff for nth. eqn.
************************************************/
{
   static char *mName = "tTest" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   DOUBLE num , denom ;
   matrix bCol, v ;
   if ( w.nRows() != nVars )
      errorExit( "ols tTest", NEDIM ) ;
   formV() ;
   // form w'beta - r in num
   bCol = beta.col( n ) ;
   num = w.inner(bCol) - r ;
   bCol.clear() ;
   // form sqrt( w'Vw ) in denom
   v = V * w ;
   denom = sqrt( w.inner(v) ) ;
   return num / ( SE(n) * denom ) ;
} // matOls::tTest

REAL matOls::FTest( const matrix& H, const matrix& a, INDEX n )
/***************************************************
   Return classical F-value for the hypotheses
                 H * beta = a
   where beta is set of coeffs for nth eqn.
***************************************************/
{
   static char *mName = "FTest" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formV() ;
   INDEX nr = H.nRows() ;
   matrix w( nr ), v( nr ), Z(nr,nr) ;
   // w = H * beta - a
   w = H * beta.col(n) ;
   w -= a ;
   Z = H * ( V.multT( H ) ) ;
   v = w / Z ;
   REAL s = SE(n) ;
   return ( w.inner( v ) / ( H.nRows() * s * s ) ) ;
} // matOls::FTest

void matOls::formV( void )
{
   static char *mName = "formV" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   formResid() ;
   if ( !(status & VMATRIX ) ) {
      INDEX i, j, k, n = Rinv.nCols() ;
      DOUBLE sum, r ;
      V.reset(n,n) ;
      VSqrt.reset(n) ;
      for ( i = 1 ; i <= n ; i++ ) {
         for ( j = i ; j <= n ; j++ ) {
            sum = 0.0 ;
            for ( k = j ; k <= n ; k++ ) {
               r = Rinv(i,k) ;
               sum += r * Rinv(j,k) ;
            } // for k
            V(i,j) = (REAL) sum ;
            V(j,i) = sum ;
         } // for j
         r = V(i,i) ;
         VSqrt(i) = sqrt(r) ;
      } // for i
      status |= VMATRIX ;
   } // if
} // matOls::formV

REAL matOls::cond( void )
/***************************************
  Estimated condition of X matrix
  based on Frobenius norm.
***************************************/
{
   static char *mName = "cond" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   ok( mName ) ;
   if ( !(status & CONDITIONED ) ) {
      DOUBLE norm1 = 0.0, norm2 = 0.0 ;
      INDEX i, j ;
      REAL r , ri ;
      for ( i = 1 ; i <= nVars ; i++ ) {
         for ( j = i ; j <= nVars ; j++ ) {
            r = R(i,j) ;
            ri = Rinv(i,j) ;
            norm1 += r * r ;
            norm2 += ri * ri ;
         } // for j
      } // for i
      condition = (REAL) ( sqrt( norm1 ) * sqrt( norm2 ) ) ;
      status |= CONDITIONED ;
   } // if
   return condition ;
} // matOls::cond

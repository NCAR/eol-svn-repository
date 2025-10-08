/**************************************************/
/*        qrhdec.c source for qrhDec class        */
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

#include "qrhdec.hpp"

/*************************************************************/
/*                      qrhDec methods                       */
/*************************************************************/

void qrhDec::setNames( void )
{
   R.name( "decR" ) ;
   w.name( "decW" ) ;
} // qrhDec setNames

#ifdef __cplusplus
qrhDec::qrhDec( void ) : matDec()
#else
qrhDec::qrhDec( void ) : ()
#endif
{
   setNames() ;
} // qrhDec


#ifdef __cplusplus
qrhDec::qrhDec( const qrhDec& qd ) : matDec(qd)
#else
qrhDec::qrhDec( const qrhDec& qd ) : (qd)
#endif
{
   setNames() ;
   R = qd.R ;
   w = qd.w ;
} // qrhDec


#ifdef __cplusplus
qrhDec::qrhDec( const matrix& x ) : matDec(x)
#else
qrhDec::qrhDec( const matrix& x ) : (x)
#endif
{
   static char *mName = "qrhDec(x)" ;
   matFunc func( mName ) ;
   INDEX n = x.nCols() ;
   R.reset( n, n ) ;
   w.reset( n ) ;
   setNames() ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // qrhDec( matrix& )

qrhDec::~qrhDec( void )
{
   static char *mName = "~qrhDec" ;
   matFunc func( mName ) ; debugInfo( func ) ;
} // ~qrhDec

void qrhDec::operator = ( const qrhDec& qd )
{
   static char *mName = "op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::operator = ( qd ) ;
   R = qd.R ;
   w = qd.w ;
} // qrhDec =

void qrhDec::clear( void )
{
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::clear() ;
   R.clear() ;
   w.clear() ;
} // qrhDec clear

void qrhDec::assign( const matrix& x )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::assign(x) ;
   INDEX n = x.nCols() ;
   R.reset( n, n ) ;
   w.reset( n ) ;
} // qrhDec::assign

void qrhDec::capture( matrix& x )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::capture(x) ;
   INDEX n = x.nCols() ;
   R.reset( n, n ) ;
   w.reset( n ) ;
} // qrhDec::capture

void qrhDec::qr( matrix& x, matrix& b, matrix& dg )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      x = m ; 
      b = w ;
      dg.diagOf( R ) ;
   } // if
} // qrhDec qr

void qrhDec::release( matrix& x, matrix& b, matrix& dg )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      x.capture( m ) ; 
      b.capture( w ) ;
      dg.diagOf( R ) ;
      clear() ;
   } // if
} // qrhDec qr

void qrhDec::decompose( void )
{
   static char *mName = "decompose" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & ASSIGNED ) )
      errorExit( mName, UNASS ) ;
   if ( !( status & DECOMPOSED ) ) {
      INDEX n = m.nCols() ;
      matrix diagR(n) ;
      qrh( m, diagR, w, tol, error ) ;
      if ( !error ) {
         R.triuOf( m.sub( 1, n, 1, n ) ) ;
         R.setDiag( diagR ) ;
      } // if
      status |= DECOMPOSED ;
   } // if
} // qrhDec::decompose

void qrhDec::solve( matrix& b )
{
   static char *mName = "solve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      qReflect( b, m, w, error ) ;
      if ( error )
         errorExit( mName, error ) ;
      backSub( b, R, tol, error ) ;
      if ( error )
	 errorExit( mName, error ) ;
   } // else
} // qrhDec::solve

void qrhDec::transSolve( matrix& b )
{
   static char *mName = "transSolve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ok( mName ) ) {
      backSubT( b, R, tol, error ) ;
      if ( error )
	 errorExit( mName, error ) ;
      qTReflect( b, m, w, error ) ;
      if ( error )
         errorExit( mName, error ) ;
   } // else
} // qrhDec::transSolve

outFile& qrhDec::info( outFile& f ) M_CONST
{
   return matDec::decInfo( f, "qrhDec" ) ;
} // qrhDec::info

inFile& qrhDec::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::get(f) ;
   INDEX n = m.nCols() ;
   R.reset( n, n ) ;
   w.reset( n ) ;
   R.get(f) ;
   w.get(f) ;
   return f ;
} // qrhDec::get

outFile& qrhDec::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   matDec::put(f) ;
   R.put(f) ;
   w.put(f) ;
   return f ;
} // qrhDec::put

outFile& qrhDec::print( char* decName, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "QR (Househiolder) Decomposition : " ) ;
   if ( decName != 0 ) 
      f.write( decName ) ;
   f.newLine(2) ;
   matDec::print( "QR", f ) ;
   R.print( "R :" , f ) ;
   w.print( "Householder `betas' : ", f ) ;
   return f ;
} // qrhDec::print

void qrhDec::det( REAL& d1, REAL& d2 )
{
   static char *mName = "det" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & DETERMINED ) ) {
      decompose() ;
      if ( error )
         errorExit( mName, error ) ;
      else {
         dProduct( R.diag(), det1, det2, tol, error ) ;
         if ( error )
	    errorExit( mName, error ) ;
      } // else
   } // if
   d1 = det1 ;
   d2 = det2 ;
} // qrhDec::det

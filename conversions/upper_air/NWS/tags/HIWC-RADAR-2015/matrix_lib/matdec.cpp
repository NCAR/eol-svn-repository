/**************************************************/
/*      matdec.c source for matDec class          */
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

#include "matdec.hpp"

/***************************************************/
/*                  matDec methods                */
/***************************************************/


void matDec::initialValues( void )
{
   status    = 0 ;
   error     = NOERROR ;
   tol       = matrixTol() ;
   det1      = 0 ;
   det2      = 0 ;
   condition = 0 ;
} // matDec initialValues

void matDec::copyValues( const matDec& md )
{
   status    = md.status ;
   error     = md.error ;
   tol       = md.tol ;
   det1      = md.det1 ;
   det2      = md.det2 ;
   condition = md.condition ;
} // matDec copyValues

#ifdef __cplusplus
matDec::matDec( void ) : matObject()
#else
matDec::matDec( void ) : ()
#endif
{
   initialValues() ;
   m.name( "decM" ) ;
} // matDec( void )

#ifdef __cplusplus
matDec::matDec( const matrix& x ) : matObject()
#else
matDec::matDec( const matrix& x ) : ()
#endif
{
   initialValues() ;
   m = x ;
   m.name( "decM" ) ;
   status = ASSIGNED ;
   condition = m.norm1() ;
} // matDec( matrix )


#ifdef __cplusplus
matDec::matDec( const matDec& md ) : matObject(md)
#else
matDec::matDec( const matDec& md ) : (md)
#endif
{
   m.refer( md.m ) ;
   m.name( "decM" ) ;
   copyValues( md ) ;
} // matDec( matDec& )

void matDec::decompose( void )
{
   errorExit( "matDec::decomp", NIMPL ) ;
   return ;
} // matDec::decompose

void matDec::solve( matrix& b )
{
   b.errorExit( "matDec::solve", NIMPL ) ;
   return ;
} // matDec::solve

void matDec::transSolve( matrix& b )
{
   b.errorExit( "matDec::transSolve", NIMPL ) ;
   return ;
} // matDec::transSolve

outFile& matDec::info( outFile& f ) M_CONST
{
   errorExit( "matDec::info", NIMPL ) ;
   return f ;
} // matDec::info

void matDec::operator = ( const matDec& md )
{
   static char *mName = "dec op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   m = md.m ;
   copyValues( md ) ;
} // matDec = matDec

void matDec::assign( const matrix& x )
{
   static char *mName = "dec assign" ;
   matFunc func( mName ) ; // debugInfo( func ) ;
   clear() ;
   m = x ;
   status = ASSIGNED ;
   condition = m.norm1() ;
} // matDec::assign

void matDec::capture( matrix& x )
{
   static char *mName = "dec capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   initialValues() ;
   m.refer(x) ;
   x.clear() ;
   status = ASSIGNED ;
   condition = m.norm1() ;
} // matDec capture

matDec::~matDec( void ) {} // ~matDec

void matDec::clear( void )
{
   static char *mName = "dec clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   m.clear() ;
   initialValues() ;
} // matDec clear

double dfabs( double r )
{
   return ( r < 0.0 ? -r : r ) ;
} // dfabs

matError matDec::errorNo( void )
{
   decompose() ;
   return error ;
} // matDec::errorNo

INDEX matDec::ok( char *mName )
{
   decompose() ;
   if ( error )
      errorExit( mName, error ) ;
   return OK ;
} // matDec::ok

INDEX matDec::ok( void )
{
   decompose() ;
   return !error ;
} // matDec::ok

charArray matDec::name( char *newName )
{
   static char *mName = "name" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( newName != 0 )
      nm = newName ;
   return nm ;
} // matDec::name

char* matDec::nameStr( void ) M_CONST
{
   return nm.array() ;
} // matDec::name

INDEX matDec::Hager( REAL& est, INDEX iter )
/****************************************************************
   Estimates lower bound for norm1 of inverse of A. Returns norm
   estimate in est.  iter sets the maximum number of iterations
   to be used.
   The return value indicates the number of iterations remaining
   on exit from loop, hence if this is non-zero the processed
   "converged".  This routine uses Hager's Convex Optimisation
   Algorithm. See Applied Numerical Linear Algebra, p139 &
   SIAM J Sci Stat Comp 1984 pp 311-16
****************************************************************/
{
   static char *mName = "luHager" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i , n, imax ;
   DOUBLE maxz, absz, product, ynorm1, inv_norm1 = 0.0 ;
   INDEX stop ;

   decompose() ;
   if ( error )
      errorExit( mName, error ) ;
   n = m.nRows() ;
   matrix b(n), y(n), z(n) ;
   b = (REAL) ( 1.0 / n ) ;
   est = -1.0 ;
   do {
      y = b ;
      solve(y) ;
      if ( error )
         return iter ;
      ynorm1 = y.norm1() ;
      if ( ynorm1 <= inv_norm1 ) {
         stop = TRUE ;
      } else {
         inv_norm1 = ynorm1 ;
         for ( i = 1 ; i <= n ; i++ )
            z(i) = ( y(i) >= 0.0 ? 1.0 : -1.0 ) ;
         transSolve(z) ;
         if ( error )
            return iter ;
         imax = 1 ;
         maxz = dfabs( (double) z(1) ) ;
         for ( i = 2 ; i <= n ; i++ ) {
            absz = dfabs( (double) z(i) ) ;
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
} // matDec::Hager

REAL matDec::cond( void )
{
   static char *mName = "cond" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & CONDITION ) ) {
      if ( ok( mName ) ) {
          REAL inorm ;
          if ( Hager( inorm ) && !error )
             condition *= inorm ;
          else if ( error )
             errorExit( mName, error ) ;
          else // no convergence in Hager
             errorExit( mName, NCONV ) ;
      } // if
      status |= CONDITION ;
   } // if
   return condition ;
} // matDec::cond

REAL matDec::setTol( const REAL newTol )
{
   REAL oldTol = tol ;
   if ( newTol >= 0.0 )
      tol = newTol ;
   return oldTol ;
} // matDec::setTol

outFile& matDec::decInfo( outFile& f, const char* decName ) M_CONST
{
   if ( matListCtrl() > 4 )
      return f ;
   objectInfo( f ) ;
   putName( nameStr(), f ) ;
   putName( decName, f ) ;
   if ( status & ASSIGNED )
     putField( m.identity(), f ) ;
   else
     f.write( "NA" ) ;
   f.newLine() ;
   return f ;
} // matDec::decInfo

outFile& matDec::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.putIndex( status ).putIndex( (INDEX)(error) ) ;
   f.putReal( tol ).newLine() ;
   f.putReal( det1 ).putReal( det2 ) ;
   f.putReal( condition ).newLine() ;
   f.putIndex( m.nRows() ).putIndex( m.nCols() ).newLine() ;
   m.put(f) ;
   return f ;
} // matDec::put

outFile& matDec::print( char* decName, outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "Status    : " ).writeIndex( status ).newLine() ;
   f.write( "Error     : " ).writeIndex( (INDEX)(error) ).newLine() ;
   f.write( "Tolerance : " ).writeReal( tol ).newLine() ;
   if ( status & DETERMINED ) {
      f.write( "Det       : " ).writeReal( det1 ) ;
      f.write( "  2^ " ).writeReal( det2 ).newLine() ;
   } // if
   if ( status & CONDITION )
      f.write( "Condition : " ).writeReal( condition ).newLine() ;
   f.newLine() ;
   f.write( decName ).newLine() ;
   INDEX oldFormat = matFormat( DISPLAY ) ;
   m.put(f) ;
   matFormat( oldFormat ) ;
   return f ;
} // matDec::print

inFile& matDec::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX err ;
   f.getIndex( status ).getIndex( err ) ;
   f.getReal( tol ).nextLine() ;
   f.getReal( det1 ).getReal( det2 ) ;
   f.getReal( condition ).nextLine() ;
   error = matError( err ) ;
   INDEX nr, nc ;
   f.getIndex( nr ).getIndex( nc ).nextLine() ;
   m.reset( nr, nc ) ;
   m.get(f) ;
   return f ;
} // matDec::get

void matDec::multiSolve( matrix& B )
//   Solve set of equations with RHS in columns of B
{
   static char *mName = "multiSolve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( m.nRows() != B.nRows() )
      errorExit( mName, NEDIM ) ;
   INDEX n = B.nCols(), i ;
   refMatrix b( B ) ;
   for ( i = 1 ; i <= n && !error ; i++ ) {
      b.refCol(i) ;
      solve( b ) ;
   } // for i
   if ( error )
      errorExit( mName, error ) ;
} // matDec::multiSolve

void matDec::inverse( matrix& inv )
{
   static char *mName = "multiSolve" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = m.nRows(), nc = m.nCols() ;
   if ( inv.nRows() != nr || inv.nCols() != nc )
      inv.reset( nr, nc ) ;
   inv = 0 ;
   inv.setDiag(1.0) ;
   multiSolve( inv ) ;
} // matDec::inverse

void dProduct( const matrix& A, REAL& d1, REAL& d2,
	       REAL tol, matError& error )
/*********************************************************
   Returns product of A's elements in d1 and d2.
   d1 is a mantissa and d2 an exponent for powers of 2.
   If A is in diagonal of triangular matrix form this will
   be determinant. Assumes A is column vector.
   Based on Bowler, Martin, Peters and Wilkinson in HACLA
***********************************************************/
{
   REAL  t1   = 1.0, t2  = 0.0 ;
   REAL  zero = 0.0, one = 1.0, four = 4.0, sixteen = 16.0 ;
   REAL  sixteenth = 0.0625 ;
   INDEX n = A.nRows() ;

   error = NOERROR ;
   for ( INDEX i = 1 ; ( i <= n ) && ( t1 != zero ) ; i++ ) {
      if ( dfabs( A(i) ) > tol ) {
         t1 *= (DOUBLE) A(i) ;
         while ( dfabs( t1 ) > one ) {
            t1 *= sixteenth ;
            t2 += four ;
         } // while
         while ( dfabs( t1 ) < sixteenth ) {
            t1 *= sixteen ;
            t2 -= four ;
         } // while
      } else {
        t1 = zero ;
        t2 = zero ;
      } // else
   } // for
   d1 = (REAL) t1 ;
   d2 = (REAL) t2 ;
   return ;
} // dProduct

void matDec::det( REAL& d1, REAL& d2 )
{
   static char *mName = "det" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( !( status & DETERMINED ) ) {
      decompose() ;
      if ( error == SINGM ) {
         det1 = 0.0 ;
         det2 = 0.0 ;
      } else if ( ok( mName ) ) {
         dProduct( m.diag(), det1, det2, tol, error ) ;
         if ( error )
            errorExit( mName, error ) ;
      } // else
      status |= DETERMINED ;
   } // if
   d1 = det1 ;
   d2 = det2 ;
} // matDec::det

void matDec::reportDet( outFile& fout )
{
   static char *mName = "reportDet" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   REAL d1, d2 ;
   det( d1, d2 ) ;
   fout.write( "det = " ).writeReal( d1 ) ;
   fout.write( " x  2^  " ).writeReal( d2 ).newLine( 2 ) ;
} // matDec::reportDet

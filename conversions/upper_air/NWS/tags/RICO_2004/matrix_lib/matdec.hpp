/**************************************************/
/*    matdec.hpp header for matDec family         */
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

#ifndef MATRIX_H
#include "matrix.hpp"
#endif

#ifndef MATDEC_H

#define MATDEC_H

class matDec : public matObject
{
   protected :

      enum matDecStatus {
         ASSIGNED    = 1,
         DECOMPOSED  = 2,
         DETERMINED  = 4,
         CONDITION   = 8
      } ;

      INDEX      status ;
      matError   error ;
      matrix     m ;
      REAL       tol, det1, det2, condition ;
      charArray  nm ;

      INDEX Hager( REAL& est, INDEX iter = 5 ) ;
      void initialValues( void ) ;
      void copyValues( const matDec& md ) ;

   public :

      matDec( void ) ;
      matDec( const matrix& x ) ;
      matDec( const matDec& md ) ;
      virtual ~matDec( void ) ;
      void operator = ( const matDec& md ) ;

      void operator = ( const matrix& x )
         { assign(x) ; }
      matError errorNo( void ) ;
      INDEX ok( char *mName ) ;
      INDEX ok( void ) ;
      char* nameStr( void )  M_CONST ;
      charArray name( char *newName ) ;
      virtual void assign( const matrix& x ) ;
      virtual void capture( matrix& x ) ;
      virtual void clear( void ) ;
      virtual void decompose( void ) ;
      virtual void solve( matrix& b ) ;
      virtual void transSolve( matrix& b ) ;
      virtual void det( REAL& d1, REAL& d2 ) ;
      virtual REAL cond( void ) ;
      virtual void multiSolve( matrix& B ) ;
      virtual void inverse( matrix& inv ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
      outFile& decInfo( outFile& f, const char *decName ) M_CONST ;
      virtual outFile& put( outFile& f ) M_CONST ;
      virtual outFile& print( char* decName, outFile& f ) M_CONST ;
      virtual inFile& get( inFile& f ) ;
      REAL setTol( const REAL r = -1.0 ) ;

      void reportDet( outFile& fout = out ) ;

      friend outFile& operator << ( outFile& f, matDec& cd )
         { return cd.put(f) ; }
      friend inFile& operator >> ( inFile& f, matDec& cd )
         { return cd.get(f) ; }
} ; // matDec

double dfabs( double r ) ;
void dProduct( const matrix& A, REAL& d1, REAL& d2,
	       REAL tol, matError& error ) ;

#endif

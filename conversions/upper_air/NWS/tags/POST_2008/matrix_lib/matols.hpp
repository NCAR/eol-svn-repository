/**************************************************/
/*    matols.hpp header for matOls family         */
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

#ifndef MATOLS_H

#define MATOLS_H


class matOls : public matObject
{

   protected :

      enum olsStatus {
           ASSIGNED    = 1,
           DECOMPOSED  = 2,
           RESIDUALS   = 4,
           VMATRIX     = 8,
           CONDITIONED = 16,
           SINGULAR    = 128
      } ;

      INDEX       status, constant, nObs, nVars, nDep, dof ;
      matrix      Y, X, R, Rinv, beta, resid, V, VSqrt ;
      matrix      TSS, RSS, YMean, SE ;
      REAL        tol, condition;
      charArray   nm ;

      virtual void setNames( void ) ;
      virtual void initial( void ) ;

   public :

      matOls( void ) ;
      matOls( const matrix& y, const matrix& x ) ;
      matOls( matOls& ols ) ;
      virtual ~matOls( void ) ;
      void operator = ( matOls& ols ) ;

      virtual void assign( const matrix& y,
			   const matrix& x ) ;
      virtual void capture( matrix& y, matrix& x ) ;
      virtual void clear( void ) ;
      virtual REAL setTol( REAL newTol = -1.0 ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      virtual outFile& olsInfo( outFile& f , char* cName ) M_CONST  ;
      virtual outFile& put( outFile& f = out )  M_CONST ;
      virtual inFile& get( inFile& f = in ) ;

      friend outFile& operator << ( outFile& f, matOls& qr )
         { return qr.put(f) ; }
      friend inFile& operator >> ( inFile& f, matOls& qr )
         { return qr.get(f) ; }

      charArray& name( char *newName ) ;
      char* nameStr( void ) M_CONST ;
      INDEX ok( void ) ;
      INDEX ok( char *mName ) ;

      virtual void decompose( void ) ;
      virtual void formResid( void ) ;
      virtual void formV( void ) ;

      virtual matrix& coeff( matrix& b ) ;
      virtual matrix& stdErr( matrix& std ) ;
      virtual matrix& residuals( matrix& res ) ;
      virtual matrix& fitted( matrix& fit ) ;
      virtual matrix& cov( matrix& cv, INDEX i = 1 ) ;
      virtual REAL se( INDEX i = 1 ) ;
      virtual REAL rss( INDEX i = 1 ) ;
      virtual REAL tss( INDEX i = 1 ) ;
      virtual REAL rsq( INDEX i = 1 ) ;
      virtual REAL rBarSq( INDEX i = 1 ) ;
      virtual REAL dw( INDEX i = 1 ) ;
      virtual REAL cond( void ) ;
      virtual REAL tTest( const matrix& w, REAL r, INDEX n = 1 ) ;
      virtual REAL FTest( const matrix& H, const matrix& a,
                          INDEX n = 1 ) ;
      virtual REAL varAdd( const matrix& z, INDEX n = 1 ) ;

} ; // class matOls

#endif

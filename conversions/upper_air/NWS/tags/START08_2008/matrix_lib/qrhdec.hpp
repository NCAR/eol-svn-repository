/**************************************************/
/*    qrhdec.hpp header for qrhDec class          */
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

#ifndef MATDEC_H
#include "matdec.hpp"
#endif

#ifndef MATQRH_H
#include "matqrh.hpp"
#endif

#ifndef MATDECQR_H

#define MATDECQR_H

class qrhDec : public matDec
{

   matrix R, w ;

   void setNames( void ) ;

   public :

      qrhDec( void ) ;
      qrhDec( const matrix& x ) ;
      qrhDec( const qrhDec& cd ) ;
      void operator = ( const qrhDec& cd ) ;
      void operator = ( const matrix& x )
         { assign(x) ; }
      virtual ~qrhDec( void ) ;

      virtual void clear( void ) ;
      void qr( matrix& x, matrix& b, matrix& dg ) ;
      void release( matrix& x, matrix& b, matrix& dg ) ;
      virtual void assign( const matrix& x ) ;
      virtual void capture( matrix& x ) ;
      virtual void decompose( void ) ;
      virtual void solve( matrix& b ) ;
      virtual void transSolve( matrix& b ) ;
      virtual void det( REAL& d1, REAL& d2 ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
      outFile& put( outFile& f = out ) M_CONST ;
      outFile& print( char* decName = 0, outFile& f = out ) M_CONST ;
      inFile& get( inFile& f = in ) ;
      friend outFile& operator << ( outFile& f, qrhDec& cd )
         { return cd.put(f) ; }
      friend inFile& operator >> ( inFile& f, qrhDec& cd )
         { return cd.get(f) ; }

} ; // qrhDec

#endif

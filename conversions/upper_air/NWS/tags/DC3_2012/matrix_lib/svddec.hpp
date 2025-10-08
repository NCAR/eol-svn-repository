/**************************************************/
/*    svddec.hpp header for svdDec class          */
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

#ifndef MATSVD_H
#include "matsvd.hpp"
#endif

#ifndef MATDECSV_H

#define MATDECSV_H

class svdDec : public matDec
{

   matrix S, V ;

   void setNames( void ) ;

   public :

      svdDec( void ) ;
      svdDec( const matrix& x ) ;
      svdDec( const svdDec& cd ) ;
      void operator = ( const svdDec& cd ) ;
      void operator = ( const matrix& x )
         { assign(x) ; }
      virtual ~svdDec( void ) ;

      virtual void clear( void ) ;
      void zero( REAL min = 0.0 ) ;
      void svd( matrix& u, matrix& sv, matrix& v ) ;
      void sv( matrix& s ) ;
      void setSV( matrix& s ) ;
      void release( matrix& u, matrix& sv, matrix& v ) ;
      virtual void assign( const matrix& x ) ;
      virtual void capture( matrix& x ) ;
      virtual void decompose( void ) ;
      virtual void solve( matrix& b ) ;
      virtual void transSolve( matrix& b ) ;
      virtual void det( REAL& d1, REAL& d2 ) ;
      virtual REAL cond( void ) ;
      virtual INDEX rank( void ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
      outFile& put( outFile& f = out ) M_CONST ;
      outFile& print( char* decName = 0, outFile& f = out ) M_CONST ;
      inFile& get( inFile& f = in ) ;
      friend outFile& operator << ( outFile& f, svdDec& cd )
         { return cd.put(f) ; }
      friend inFile& operator >> ( inFile& f, svdDec& cd )
         { return cd.get(f) ; }

} ; // svdDec

#endif




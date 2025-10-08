/**************************************************/
/*         ludec.hpp header for luDec class       */
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

#ifndef MATLU_H
#include "matlu.hpp"
#endif

#ifndef MATDECLU_H

#define MATDECLU_H


class luDec : public matDec
{
      indexArray  indexp ;
      REAL        sign ;

   public :

      friend void initialLudec( void ) ;

      luDec( void ) ;
      luDec( luDec& lu ) ;
      luDec( const matrix& x ) ;
      virtual ~luDec( void ) ;
      void operator = ( const luDec& lu ) ;

      virtual void assign( const matrix& x ) ;
      virtual void capture( matrix& x ) ;
      virtual void clear( void ) ;
      virtual void decompose( void ) ;
      void lu( matrix& x, indexArray& ind ) ;
      void release( matrix& x, indexArray& ind ) ;
      virtual void solve( matrix& b ) ;
      virtual void transSolve( matrix& b ) ;
      virtual void det( REAL& r1, REAL& r2 ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
      outFile& put( outFile& f = out ) M_CONST ;
      outFile& print( char* decName= 0, outFile& f = out ) M_CONST ;
      inFile& get( inFile& f = in ) ;

      void operator = ( const matrix& x )
         { assign( x ) ; }

      friend outFile& operator << ( outFile& f, luDec& lu )
         { return lu.put(f) ; }
      friend inFile& operator >> ( inFile& f, luDec& lu )
         { return lu.get(f) ; }

} ; // class luDec

matrix squareEqn( const matrix& x, const matrix& b ) ;
matrix normalEqn( const matrix& x, const matrix& b ) ;

#endif




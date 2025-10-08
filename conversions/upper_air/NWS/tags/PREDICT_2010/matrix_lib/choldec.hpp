/**************************************************/
/*    choldec.hpp header for cholDec class        */
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

#ifndef MATCHOL_H
#include "matchol.hpp"
#endif

#ifndef MATDECCH_H

#define MATDECCH_H

class cholDec : public matDec
{
   public :

      cholDec( void ) ;
      cholDec( const matrix& x ) ;
      cholDec( const cholDec& cd ) ;
      virtual ~cholDec( void ) ;
      cholDec& operator = ( const cholDec& cd ) ;

      void chol( matrix& x ) ;
      void release( matrix& x ) ;
      virtual void decompose( void ) ;
      virtual void solve( matrix& b ) ;
      virtual void transSolve( matrix& b ) ;
      virtual void det( REAL& d1, REAL& d2 ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
      virtual outFile& print( char* decName, outFile& f ) M_CONST ;

      friend outFile& operator << ( outFile& f, cholDec& cd )
         { return cd.put(f) ; }
      friend inFile& operator >> ( inFile& f, cholDec& cd )
         { return cd.get(f) ; }

} ; // class cholDec

#endif

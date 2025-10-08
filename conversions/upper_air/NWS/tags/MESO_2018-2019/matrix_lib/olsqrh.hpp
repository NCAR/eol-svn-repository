/**************************************************/
/*    olsqrh.hpp header for olsQrh class          */
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

#ifndef MATOLS_H
#include "matols.hpp"
#endif

#ifndef MATOLSQR_H

#define MATOLSQR_H

class olsQrh : public matOls
{

   protected :

      matrix U, Q, w ;
      virtual void setNames( void ) ;
      virtual void initial( void ) ;

   public :

      olsQrh( void ) ;
      olsQrh( const matrix& y, const matrix& x ) ;
      virtual ~olsQrh( void ) ;
      olsQrh( olsQrh& ols ) ;
      void operator = ( olsQrh& ols ) ;

      virtual void clear( void ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      virtual void decompose( void ) ;

      REAL varAdd( const matrix& z, INDEX n = 1 ) ;

} ; // class olsQrh

#endif

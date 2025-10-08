/**************************************************/
/*    olschol.hpp header for olsChol class        */
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

#ifndef MATOLSCH_H

#define MATOLSCH_H

class olsChol : public matOls
{

   protected :

      matrix XTX, XTY ;
      virtual void setNames( void ) ;
      virtual void initial( void ) ;

   public :

      olsChol( void ) ;
      olsChol( const matrix& y, const matrix& x ) ;
      olsChol( olsChol& ols ) ;
      virtual ~olsChol( void ) ;
      void operator = ( olsChol& ols ) ;

      virtual void clear( void ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      virtual void decompose( void ) ;

      REAL varAdd( const matrix& z, INDEX n = 1 ) ;

} ; // class olsChol

INDEX triuInv( const matrix& R, matrix& Rinv, REAL tol,
	       matError& error );

#endif



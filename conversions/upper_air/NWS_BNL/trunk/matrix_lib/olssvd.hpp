/**************************************************/
/*    olssvd.hpp header for olsSvd class          */
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

#ifndef MATOLSSV_H

#define MATOLSSV_H

class olsSvd : public matOls
{

   protected :

      matrix  U, SV, W ;
      virtual void initial( void ) ;
      void zeroSV( void ) ;
      void formBeta( void ) ;

   public :

      olsSvd( void ) ;
      olsSvd( const matrix& y, const matrix& x ) ;
      virtual ~olsSvd( void ) ;
      olsSvd( olsSvd& ols ) ;
      void operator = ( olsSvd& ols ) ;

      virtual void clear( void ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      virtual void decompose( void ) ;

      virtual void formV( void ) ;
      virtual REAL cond( void ) ;
      virtual matrix& sv( matrix& s ) ;
      virtual void setSV( const matrix& newSV ) ;

      REAL varAdd( const matrix& z, INDEX n = 1 ) ;

} ; // class olsSvd

#endif

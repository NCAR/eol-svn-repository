/**************************************************/
/*    matrand.hpp header for matRandom class      */
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

#ifndef MATRAND_H

#define MATRAND_H


// random generators

      enum distribution
      {
          UNIFORM, NORMAL, EXPON, GAMMA, POISSON, BINOMIAL
      } ; // distribution

class matRandom : public matObject
{

      int x, y, z, normalOk ;
      REAL normal0 ;

      matrix& operator()( matrix& x, distribution dist ,
			  REAL p1 , REAL p2 ) ;

   public :

      matRandom( int newX = 26, int newY = 2, int newZ = 1947 ) ;
      matRandom( matRandom& ran ) ;
      void operator = ( matRandom& ran ) ;
      ~matRandom( void ) ;

      outFile& put( outFile& f  ) M_CONST ;
      inFile& get( inFile& f ) ;
      outFile& info( outFile& f ) M_CONST ;


      void seed( int newX, int newY, int newZ ) ;
      REAL uniform( void ) ;
      REAL normal( REAL mean = 0.0, REAL std = 1.0 ) ;
      REAL expon( REAL mean = 1.0 ) ;
      REAL gamma( REAL a = 1.0 ) ;
      REAL poisson( REAL mean = 1.0 ) ;
      REAL binomial( int n, REAL p ) ;

      matrix& uniform( matrix& x )
	      { return (*this)( x, UNIFORM, 0, 0 ) ; }
      matrix& normal( matrix& x, REAL mean = 0.0, REAL std = 1.0 )
	      { return (*this)( x, NORMAL, mean, std ) ; }
      matrix& expon( matrix& x, REAL mean = 1.0 )
	      { return (*this)( x, EXPON, mean, 0 ) ; }
      matrix& gamma( matrix& x, REAL a = 1.0 )
	      { return (*this)( x, GAMMA, a, 0 ) ; }
      matrix& poisson( matrix& x, REAL mean = 1.0 )
	      { return (*this)( x, POISSON, mean, 0 ) ; }
      matrix& binomial( matrix& x, int n, REAL p )
	      { return (*this)( x, BINOMIAL, REAL(n), p )  ; }


} ; // matRandom


#endif




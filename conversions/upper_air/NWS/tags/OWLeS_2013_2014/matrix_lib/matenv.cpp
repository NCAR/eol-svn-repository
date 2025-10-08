/**************************************************/
/*         matenv.c  matEnvironment source        */
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

#include "matrix.hpp"
#include "math.h"
// for sqrt

#ifndef NO_STATIC_DEC
#ifdef __cplusplus
INDEX matInitial::initiated ;
REAL  matEnvironment::epsilon ;
REAL  matEnvironment::tolerance ;
int   matEnvironment::pageWidth ;
int   matEnvironment::precision ;
int   matEnvironment::fieldWidth ;
int   matEnvironment::matForm ;
#endif
#endif

matInitial::matInitial( void )
{
   if ( initiated++ == 0 ) {
      matFuncInitial() ;
      matObjectInitial() ;
      matFileInitial() ;
   } // if
} // matInitial

matInitial::~matInitial( void )
{
   initiated-- ;
} // matInitial

static matEnvironment matEnv ;

matEnvironment::matEnvironment( void )
{
   DOUBLE x = 1.0 ;
   epsilon = 1.0 ;
   while ( ( x + epsilon ) > 1.0 ) epsilon /= 2.0 ;
   epsilon *= 2.0 ;
   tolerance = sqrt( epsilon )  ;
   pageWidth = 80 ;
   precision = 4 ;
   fieldWidth = 14 ;
   matForm = PLAIN ;
} // matEnvironment

DOUBLE matrixTol( const DOUBLE newTol )
{
      DOUBLE oldTol = matEnv.tolerance ;
      if ( newTol >= 0.0 )
         matEnv.tolerance = newTol ;
      return oldTol ;
} // matrixTol

DOUBLE matrixEps( const DOUBLE newEps )
{
      DOUBLE oldEps = matEnv.epsilon ;
      if ( newEps >= 0.0 )
         matEnv.epsilon = newEps ;
      return oldEps ;
} // matrixEps

int matFormat( int value )
{
   int oldvalue = matEnv.matForm ;
   if ( value > 0 )
      matEnv.matForm = value ;
   return oldvalue ;
} // matFormat

int matPrecision( int value )
{
   int oldvalue = matEnv.precision ;
   if ( value > 0 )
      matEnv.precision = value ;
   return oldvalue ;
} // matPrecision

int matField( int value )
{
   int oldvalue = matEnv.fieldWidth ;
   if ( value > 0 )
      matEnv.fieldWidth = value ;
   return oldvalue ;
} // matField

int matPageWidth( int value )
{
   int oldvalue = matEnv.pageWidth ;
   if ( value > 0 )
      matEnv.pageWidth = value ;
   return oldvalue ;
} // matPageWidth


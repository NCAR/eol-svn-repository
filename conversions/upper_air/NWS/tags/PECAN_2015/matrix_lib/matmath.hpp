/***************************************************/
/* matmath.hpp header for transcendental functions */
/***************************************************/


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

#ifndef MATMATH_H

#define MATMATH_H

// transcendental methods
void transcend( matrix& z, const matrix& x, int fn ) ;


#ifdef __cplusplus

matrix round( const matrix& x ) ;
matrix fix( const matrix& x ) ;
matrix floor( const matrix& x ) ;
matrix ceil( const matrix& x ) ;
matrix sign( const matrix& x ) ;
matrix abs( const matrix& x ) ;
matrix log( const matrix& x ) ;
matrix exp( const matrix& x ) ;
matrix log10( const matrix& x ) ;
matrix ln( const matrix& x ) ;
matrix sin( const matrix& x ) ;
matrix cos( const matrix& x ) ;
matrix tan( const matrix& x ) ;
matrix asin( const matrix& x ) ;
matrix acos( const matrix& x ) ;
matrix atan( const matrix& x ) ;
matrix sinh( const matrix& x ) ;
matrix cosh( const matrix& x ) ;
matrix tanh( const matrix& x ) ;


#else 

matrix roundm( const matrix& x ) ;
matrix fixm( const matrix& x ) ;
matrix floorm( const matrix& x ) ;
matrix ceilm( const matrix& x ) ;
matrix signm( const matrix& x ) ;
matrix absm( const matrix& x ) ;
matrix logm( const matrix& x ) ;
matrix expm( const matrix& x ) ;
matrix log10m( const matrix& x ) ;
matrix lnm( const matrix& x ) ;
matrix sinm( const matrix& x ) ;
matrix cosm( const matrix& x ) ;
matrix tanm( const matrix& x ) ;
matrix asinm( const matrix& x ) ;
matrix acosm( const matrix& x ) ;
matrix atanm( const matrix& x ) ;
matrix sinhm( const matrix& x ) ;
matrix coshm( const matrix& x ) ;
matrix tanhm( const matrix& x ) ;


#endif

#endif

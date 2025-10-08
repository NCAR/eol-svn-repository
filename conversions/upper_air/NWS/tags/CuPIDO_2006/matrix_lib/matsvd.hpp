/**************************************************/
/*     matsvd.hpp header for SVD functions        */
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

#ifndef MATSVD_H

#define MATSVD_H

INDEX svdcmp( matrix& a, matrix& w, matrix& v, matError& error ) ;
INDEX svdBackSub( const matrix& a, const matrix& w, 
                 const matrix& v, const matrix& b,
                 matrix& x, matError& error ) ;

#endif


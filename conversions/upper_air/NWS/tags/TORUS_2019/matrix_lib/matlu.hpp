/**************************************************/
/*    matlu.hpp header for LU (crout) functions   */
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

#ifndef MATLU_H

#define MATLU_H

extern INDEX crout( matrix& A, indexArray& index, REAL& sign,
                    REAL tol, matError& error ) ;
extern INDEX luSolve( const matrix& A, const indexArray& index,
		      matrix& b, REAL tol, matError& error ) ;
extern INDEX luInverse( const matrix& A, const indexArray& index,
		        matrix& Ainv, REAL tol, matError &error ) ;
extern INDEX lutSolve( const matrix& A, const indexArray& index,
		       matrix& b, REAL tol, matError& error ) ;
extern INDEX luHager( const matrix& A, const indexArray& index,
		      REAL& est, REAL tol, matError& error,
		      INDEX iter = 5 ) ;

#endif





/**************************************************/
/*    matchol.hpp header for Cholesky functions   */
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

#ifndef MATCHOL_H

#define MATCHOL_H

extern void diagProd( const matrix& A, REAL& d1, REAL& d2,
		      REAL tol, matError& error ) ;

INDEX cholesky( matrix& x, matError& error  ) ;
INDEX cholSolve( const matrix& A, matrix& b, REAL tol,
		 matError& error ) ;
matrix normalEqn( const matrix& x, const matrix& b ) ;

#endif





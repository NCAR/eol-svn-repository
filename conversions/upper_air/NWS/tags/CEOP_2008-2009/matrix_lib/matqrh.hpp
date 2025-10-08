/***************************************************/
/*  matqrh.hpp header for Householder QR functions */
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

#ifndef MATQRH_H

#define MATQRH_H

INDEX hReflector( matrix& v, REAL& alpha, REAL& beta,
                   REAL& vMax, REAL tol, matError& error ) ;
INDEX hReflect( matrix& y, const matrix& u, REAL beta,
                matError& error ) ;
INDEX qReflect( matrix& y, const matrix& q, matrix& w,
                matError& error ) ;
INDEX qTReflect( matrix& y, const matrix& q, matrix& w,
		 matError& error ) ;
INDEX qrh( matrix& x, matrix& diagR, matrix& b, REAL tol,
           matError& error ) ;
INDEX backSub( matrix& y, const matrix& x, REAL tol,
               matError& error ) ;
INDEX backSubT( matrix& y, const matrix& x, REAL tol,
	        matError& error ) ;
INDEX hProject( matrix& y, matrix& x, matrix& R, matrix& w,
                matrix& beta, REAL tol, matError& error ) ;
INDEX triuInv( const matrix& R, matrix& Rinv, REAL tol,
	       matError& error );

#endif

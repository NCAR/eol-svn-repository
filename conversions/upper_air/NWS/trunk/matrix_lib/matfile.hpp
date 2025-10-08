/**************************************************/
/*    matfile.hpp header for matFile family       */
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
#endif // ndef MATRIX_H

#include <stdio.h>

// File Error Values

enum matFileErrors {
     NULLTYPE, FTYPE, FNEW, FOPEN, NFILE, FREAD,
     FWRITE, FNAME, FNCOPY, FBAD
} ; // matFileErrors



/*****************************************************/
/*       matreal.c source for realArray class        */
/*****************************************************/


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

   /*****************************************/
   /* Base is offset back by one to facili- */
   /* tate unit base addressing of elements.*/
   /* Note the elements of base are !not!   */
   /* offset. See matMap below              */
   /*****************************************/

#include <string.h> // for mem functions

#ifdef __cplusplus
realArray::realArray( void ) : matObject()
#else
realArray::realArray( void ) : ()
#endif
{
   errorExit( "realArray(void)", NIMPL ) ;
} // realArray( void )

#ifdef __cplusplus
realArray::realArray( INDEX nr, INDEX nc ) : matObject()
#else
realArray::realArray( INDEX nr, INDEX nc ) : ()
#endif
{
   static char *mName = "realAr(n)" ;
   matFunc func( mName ) ;
   if ( nr == 0 || nc == 0 )
      errorExit( mName, NPDIM );
   if ( ( base = new REAL*[nc] ) == 0 )
      errorExit( mName, NOMEM ) ;
   INDEX size = nr * sizeof( REAL ) ;
   base-- ;
   for ( INDEX i = 1 ; i <= nc ; i++ ) {
      if ( ( base[i] = new REAL[nr] ) == 0 )
         errorExit( mName, NOMEM ) ;
      memset( (void *)base[i], 0, size ) ;
   } // for
   ref = 1 ;
   nrow = nr ;
   ncol = nc ;
   debugInfo( func ) ;
} // realArray::realArray

realArray::~realArray( void )
{
   static char *mName = "~realArray" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( base != 0 ) {
      for ( INDEX i = 1 ; i <= ncol ; i++ )
	  delete base[i] ;
      base++ ;
      delete base ;
   } // if
} // realArray::~realArray

REAL& realArray::elem( INDEX i, INDEX j ) M_CONST
{
   if ( i < 1 || i > nrow || j < 1 || j > ncol )
      errorExit( "realArray()", NRANG ) ;
   return base[j][i-1] ;
} //realArray()

realArray& realArray::operator = ( realArray& y )
{
   static char *mName = "op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i, j, nr = y.nrow, nc = y.ncol ;
   if ( nr != nrow || nc != ncol )
      errorExit( mName, NEDIM ) ;
   for ( i = 1; i <= nr ; i++ )
      for ( j = 1 ; j <= nc ; j++ )
	 elem(i,j) = y(i,j) ;
   return *this ;
} // realArray =

outFile& realArray::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 1 )
     return f ;
   objectInfo( f ) ;
   putName( "", f ) ;
   putName( "realAr", f ) ;
   putField( nrow, f ) ;
   putField( ncol, f ) ;
   putField( "", f ) ;
   putField( ref, f ) ;
   f.newLine() ;
   return f ;
} // realArray::info

outFile& realArray::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errorExit( mName, NIMPL ) ;
   return f ;
} // realArray::put

inFile& realArray::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errorExit( mName, NIMPL ) ;
   return f ;
} // realArray::get

void realArray::clear( void ) {
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errorExit( mName, NIMPL ) ;
} // realArray::clear

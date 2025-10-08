/*****************************************************/
/*         matref.c source for refMatrix class       */
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
#include <string.h>

#ifdef __cplusplus
refMatrix::refMatrix( void ) : matrix()
#else
refMatrix::refMatrix( void ) : ()
#endif
{
    errorExit( "refMatrix(void)", NIMPL ) ; 
} // refMatrix::refMatrix

#ifdef __cplusplus
refMatrix::refMatrix( const matrix& m ) : matrix()
#else
refMatrix::refMatrix( const matrix& m ) : ()
#endif
{
   static char *mName="refMatrix(&m)" ;
   matFunc func( mName ) ;
   ppm = m.pm ;
   ppm->ref++ ;
   if ( pm != 0 )
      delete pm ;
   pm = new matMap( *ppm ) ;
   debugInfo( func ) ;
} // refMatrix::refMatrix

#ifdef __cplusplus
refMatrix::refMatrix( const refMatrix& m ) : matrix()
#else
refMatrix::refMatrix( const refMatrix& m ) : ()
#endif
{
   static char *mName="refMatrix(&rm)" ;
   matFunc func( mName ) ;
   ppm = m.ppm ;
   ppm->ref++ ;
   pm = m.pm ;
   pm->ref++ ;
   debugInfo( func ) ;
} // refMatrix::refMatrix

refMatrix::~refMatrix( void )
{
   if ( ppm != 0 && --ppm->ref == 0 )
      delete ppm ;
} // ~refMatrix

refMatrix& refMatrix::operator = ( const refMatrix& m )
{
   static char *mName="op = ref" ;
   matFunc func( mName ) ;
   clear() ;
   ppm = m.ppm ;
   ppm->ref++ ;
   if ( pm != 0 && --pm->ref == 0 )
      delete pm ;
   pm = m.pm ;
   pm->ref++ ;
   debugInfo( func ) ;
   return *this ;
} // refMatrix =

matrix& refMatrix::operator = ( const matrix& m )
{
   static char *mName="op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = nRows(), nc = nCols(), j ;
   if ( nr != m.nRows() || nc != m.nCols() )
      errorExit( mName, NEDIM ) ;
   INDEX size = nr * sizeof( REAL ) ;
   for ( j = 1 ; j <= nc ; j++ )
      memcpy( (void *)(pm->map[j] + 1),
	      (void *)(m.pm->map[j] + 1), size );
   return *this ;
} // refMatrix::refMatrix

void refMatrix::reset( INDEX nr, INDEX nc )
{
   if ( nRows() != nr || nCols() != nc )
      errorExit( "reset", NEDIM ) ;
} // refMatrix reset

void refMatrix::reset( INDEX nr )
{
   reset( nr, 1 ) ;
} // refMatrix reset

void refMatrix::clear( void )
{
   if ( ppm != 0 && --ppm->ref == 0 )
      delete ppm ;
   ppm = 0 ;
   matrix::clear() ;
} // refMatrix clear

INDEX refMatrix::isNull( void ) M_CONST
{ return ( ppm == 0 || pm == 0 || pm->pa == 0 ) ; }

outFile& refMatrix::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 3 )
      return f ;
   objectInfo( f ) ;
   if ( pm != 0 )
      putName( pm->nm.array(), f ) ;
   else
      putName( "", f ) ;
   putName( "refMat", f ) ;
   putField( nRows(), f ) ;
   putField( nCols(), f ) ;
   if ( pm != 0 )
      putField( pm->identity(), f ) ;
   else
      putField( "NA", f ) ;
   if ( ppm != 0 )
      putField( ppm->identity(), f ) ;
   else
      putField( "NA", f ) ;
   f.newLine() ;
   return f ;
} // refMatrix::info

matrix& refMatrix::refSub( INDEX r1, INDEX r2,
			      INDEX c1, INDEX c2 )
{
   static char *mName = "refSub" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   INDEX nrow = ppm->nrow, ncol = ppm->ncol ;
   if ( r1 < 1 || r1 > r2 || r2 > nrow ||
	c1 < 1 || c1 > c2 || c2 > ncol )
      errorExit( mName, NRANG ) ;
   INDEX nr = r2 - r1 + 1, nc = c2 - c1 + 1, j ;
   if ( nc > pm->mapSize )
      errorExit( mName, NRANG ) ;
   for ( j = 1 ; j <= nc ; j++ )
      pm->map[j] = ppm->map[j+c1-1] + r1 - 1 ;
   pm->ncol = nc ;
   pm->nrow = nr ;
   return *this ;
} // refMatrix refSub

matrix& refMatrix::refRow( INDEX r1, INDEX r2 )
{
   static char *mName = "refRow" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   if ( r2 == 0 )
      r2 = r1 ;
   return refSub( r1, r2, 1, ppm->ncol ) ;
} // refMatrix refRow

matrix& refMatrix::refCol( INDEX c1, INDEX c2 )
{
   static char *mName = "refCol" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( isNull() )
      errorExit( mName, NULRF ) ;
   if ( c2 == 0 )
      c2 = c1 ;
   return refSub( 1, ppm->nrow, c1, c2 ) ;
} // refMatrix refCols

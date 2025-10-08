/**************************************************/
/*    matrix.c source for basic matrix class      */
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
#include <string.h>
#include <stdlib.h>
#include <math.h>
// for sqrt

#ifndef __cplusplus
#define NULL 0L
#endif

void matrix::reset( INDEX nr, INDEX nc )
{
   static char *mName="reset" ;
   matFunc func( mName ) ;
   if ( ( nRows() != nr ) || ( nCols() != nc ) ) {
      if ( nRefs() > 1 )
         errorExit( mName, MREFS ) ;
      pm->reset( nr, nc ) ;
      debugInfo( func ) ;
   } // if
   return ;
} // matrix::reset

void matrix::reset( INDEX nr )
{
    reset( nr, 1 ) ;
} //  matrix::reset

#ifdef __cplusplus
matrix::matrix( INDEX nr, INDEX nc ) : matObject( )
#else
matrix::matrix( INDEX nr, INDEX nc ) : ( )
#endif
{
   static char *mName="matrix(,)" ;
   matFunc func( mName ) ;
   type = STANDARD ;
   pm = new matMap( nr, nc ) ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matrix::matrix( INDEX, INDEX )

#ifdef __cplusplus
matrix::matrix( char* nameStr, INDEX nr, INDEX nc ) : matObject()
#else
matrix::matrix( char* nameStr, INDEX nr, INDEX nc ) : ()
#endif
{
   static char *mName="matrix(ch,,)" ;
   matFunc func( mName ) ;
   type = STANDARD ;
   pm = new matMap( nr, nc ) ;
   pm->nm = nameStr ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matrix::matrix( char*, INDEX, INDEX )

void matrix::clear( void )
{
   static char *mName="clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pm != 0 )
      pm->clear() ;
} // matrix::clear

#ifdef __cplusplus
matrix::matrix( void ) : matObject()
#else
matrix::matrix( void ) : ()
#endif
{
   static char *mName="matrix" ;
   matFunc func( mName ) ;
   type = STANDARD ;
   pm = new matMap ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matrix::matrix( void )

matrix::~matrix( void )
{
   static char *mName="~matrix" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pm != 0 && --pm->ref == 0 )
      delete pm ;
} // matrix::~matrix

#ifdef __cplusplus
matrix::matrix( const matrix &y ) : matObject()
#else
matrix::matrix( const matrix &y ) : ()
#endif
{
   static char *mName = "matrix(&y)" ;
   matFunc func( mName ) ;
   type = y.type ;
   pm = y.pm ;
   if ( pm != 0 )
      pm->ref++ ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matrix::matrix( matrix& )

INDEX matrix::isNull( void ) M_CONST
{ return ( pm == 0 || pm->pa == 0 ) ; }

INDEX matrix::isTrans( void ) M_CONST
{ return type == TRANSPOSED ; }

INDEX matrix::nRefs( void ) M_CONST
{ return pm == 0 ? 0 : pm->ref ; }

INDEX matrix::nRows( void ) M_CONST
{ return pm == 0 ? 0 : pm->nrow ; }

INDEX matrix::nCols( void ) M_CONST
{ return pm == 0 ? 0 : pm->ncol ; }

LONG matrix::size( void ) M_CONST
{ return ( pm == 0 ) ? 0 : (LONG)(pm->ncol) * (LONG)(pm->nrow) ; }

REAL& matrix::xmat( INDEX i, INDEX j ) M_CONST
{
   static char *mName = "xmat(i,j)" ;
   if ( isNull() )
      errorExit( mName, UNASS ) ;
   if ( ( i < 1 ) || ( j < 1 ) || ( i > nRows() ) || ( j > nCols() ) )
      errorExit( mName, NRANG ) ;
   return pm->map[j][i] ;
}// matrix::xmat

REAL& matrix::vec( LONG k ) M_CONST
{
   static char *mName = "vec()" ;
   INDEX i, j, nr = nRows() ;
   if ( isNull() )
      errorExit( mName, UNASS ) ;
   if ( k >= 1 ) {
     k-- ;
     i = (INDEX)( ( k % nr ) + 1) ;
     j = (INDEX)( ( k / nr ) + 1) ;
   } // if
   if ( j > nCols() ) {
      errorij( i, j ) ;
      errorExit( mName, NRANG ) ;
   } // if
   return mat(i,j) ;
} // matrix::vec

void matrix::capture( matrix& y )
{
   static char *mName = "capture" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pm != 0 && --pm->ref == 0 )
      delete pm ;
   pm = y.pm ;
   pm->ref++ ;
   y.clear() ;
} // matrix::capture

matrix& matrix::operator = ( const matrix &y )
{
   static char *mName = "op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( this->identity() == y.identity() )
      return *this ;
   INDEX nr = y.nRows(), nc = y.nCols(), j ;
   if ( nRefs() <= 1 )
      reset( nr, nc ) ;
   else if ( ( nRows() != nr ) || ( nCols() != nc ) )
      errorExit( mName, NEDIM ) ;
   INDEX size = nr * sizeof( REAL ) ;
   for ( j = 1 ; j <= nc ; j++ )
      memcpy( (void *)(pm->map[j] + 1),
	      (void *)(y.pm->map[j] + 1), size ) ;
   return *this ;
} // matrix = matrix


/***************************************************/
/*   initialisation methods for matrix class       */
/***************************************************/

#include <stdarg.h>

matrix& matrix::assign( INDEX nr, INDEX nc, ... )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nr != nRows() || nc != nCols() ) {
      if ( nRefs() > 1 )
	 errorExit( mName, NEDIM ) ;
      else
	 reset( nr, nc ) ;
   } // if
   va_list ap ;
   va_start( ap, nc ) ;
   INDEX i , j ;
   for ( i = 1 ; i <= nr ; i++ )
      for ( j = 1; j <= nc ; j++ )
	 mat(i,j) = va_arg( ap , double ) ;
   va_end( ap ) ;
   return *this ;
} // matrix::assign

/**************************************************************/
/*   matrix (non-iostream) I/O for matrix class               */
/**************************************************************/

outFile& matrix::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 3 )
      return f ;
   objectInfo( f ) ;
   if ( pm != 0 )
      putName( pm->nm.array(), f ) ;
   else
      putName( "", f ) ;
   putName( "matrix", f ) ;
   putField( nRows(), f ) ;
   putField( nCols(), f ) ;
   if ( pm != 0 )
      putField( pm->identity(), f ) ;
   else
      putField( "NA", f ) ;
   f.newLine() ;
   return f ;
} // matrix::info

outFile& matrix::display( outFile& f ) M_CONST
{
   static char *mName = "display" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   int oldform = matFormat() ;
   matFormat( oldform & SCIENTIFIC ? DISPLAY|SCIENTIFIC : DISPLAY ) ;
   put( f ) ;
   matFormat( oldform ) ;
   return f ;
} // matrix display

charArray matrix::name( char *newName )
{
   static char *mName = "name" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pm == 0 )
      pm = new matMap ;
   if ( newName != 0 )
      pm->nm = newName ;
   return pm->nm ;
} // matrix::name

outFile& matrix::print( char *title, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.newLine();
   if ( title != 0 )
      f.write( title ).newLine() ;
   else if ( pm != 0 ) {
      f.writeChar( pm->nm ) ; 
      f.newLine() ;
   } // else
   display( f ) ;
   f.newLine() ;
   return f ;
} // matrix::print

matrix& matrix::read( void )
{
   static char *mName = "read" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errout.newLine().write( "Input matrix with " ) ;
   errout.writeIndex( nRows() ).write( " rows and " ) ;
   errout.writeIndex( nCols() ).write( " columns : " ).newLine() ;
   get( in ) ;
   errout.write( "Ok." ).newLine() ;
   return *this ;
} // matrix::read

void matrix::checkDims( const matrix& y, char *name ) M_CONST
/************************************************************
   Check the two matrices are not null and that they
   have the same dimension.
************************************************************/
{
   if ( isNull() )
      errorExit( name, NULRF ) ;
   if ( y.isNull() )
      y.errorExit( name, NULRF ) ;
   INDEX nr = nRows(), nc = nCols() ;
   if ( ( nr != y.nRows() ) || ( nc != y.nCols() ) ) {
      y.error( name, NEDIM ) ;
      errorExit( name, NEDIM ) ;
   } // if
} // matrix::checkDims

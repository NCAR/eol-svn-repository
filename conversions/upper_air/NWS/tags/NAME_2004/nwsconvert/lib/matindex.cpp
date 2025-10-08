/**************************************************/
/*    matindex.c source for indexArray class      */
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
#include <ctype.h>

#ifndef __cplusplus
#define NULL 0L
#endif

#ifdef __cplusplus
indexArray::indexArray( char* nameStr, INDEX n ) : matObject()
#else
indexArray::indexArray( char* nameStr, INDEX n ) : ()
#endif
{
   static char *mName = "indexAr(name,n)" ;
   matFunc func( mName ) ;
   nm = nameStr ;
   if ( n != 0 ) {
      ip = new INDEX[n] ;
      if ( ip == 0 )
         errorExit( mName, NOMEM ) ;
      len = n;
      memset( (void*) ip, 0, n * sizeof(INDEX) ) ;
      ip-- ; // unit offset
   } else {
     ip = 0 ;
     len = 0;
   } // else
   func.trace(TRUE) ; // debugInfo( func ) ;
} // indexArray

#ifdef __cplusplus
indexArray::indexArray( INDEX n ) : matObject()
#else
indexArray::indexArray( INDEX n ) : ()
#endif
{
   static char *mName = "indexAr(n)" ;
   matFunc func( mName ) ;
   if ( n != 0 ) {
      ip = new INDEX[ n ] ;
      if ( ip == 0 )
         errorExit( mName, NOMEM ) ;
      len = n ;
      memset( (void *) ip, 0, n * sizeof( INDEX ) ) ;
      ip-- ; // unit offset
   } else {
      ip = 0 ;
      len = 0 ;
   } // else
   func.trace(TRUE) ; // debugInfo( func ) ;
} // indexArray

#if 0

#ifdef __cplusplus
indexArray::indexArray( void ) : matObject()
#else
indexArray::indexArray( void ) : ()
#endif
{
   static char *mName = "indexAr(n)" ;
   matFunc func( mName ) ;
   ip = 0 ;
   len = 0 ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // indexArray

#endif

#ifdef __cplusplus
indexArray::indexArray( const indexArray& index ) : matObject()
#else
indexArray::indexArray( const indexArray& index ) : ()
#endif
{
   static char *mName = "indexAr(index)" ;
   matFunc func( mName ) ;
   INDEX n = index.length() ;
   if ( n != 0 ) {
      ip = new INDEX[ n ] ;
      if ( ip == 0 )
         errorExit( mName, NOMEM ) ;
      len = n ;
      ip-- ; // unit offset
      for ( INDEX i = 1 ; i <= n ; i++ )
         ip[i] = index.ip[i] ;
   } else {
      ip = 0 ;
      len = 0 ;
   } // else
   func.trace(TRUE) ; // debugInfo( func ) ;
} // indexArray::indexArray( indexArray& )

indexArray::~indexArray( void )
{
   static char *mName = "~indexArray" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ip != 0 ) {
      ip++ ;
      delete ip ;
   } // if
} // ~indexArray

void indexArray::clear( void )
{
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ip != 0 ) {
      ip++ ;
      delete ip ;
      ip = 0 ;
      len = 0 ;
   } // if
} // indexArray clear

charArray indexArray::name( char *newName )
{
   if ( newName != 0 )
      nm = newName  ;
   return nm ;
} // indexArray::name

indexArray& indexArray::reset( INDEX n )
{
   static char *mName = "reset" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( ip != 0 ) {
      ip++ ;
      delete ip ;
   } // if
   ip = new INDEX[n] ;
   if ( ip == 0 )
      errorExit( mName, NOMEM ) ;
   ip-- ;
   len = n ;
   return *this ;
} // indexArray reset

INDEX& indexArray::xelem( INDEX i ) M_CONST
{
   if ( i > length() )
      errorExit( "xelem ", NRANG ) ;
   return ip[i] ;
} // indexArray xelem

#include <stdarg.h>

indexArray& indexArray::assign( INDEX n, ... )
{
   static char *mName = "assign" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( n != length() )
      reset(n) ;
   va_list ap ;
   va_start( ap, n ) ;
   INDEX i ;
   for ( i = 1 ; i <= n ; i++ )
      elem(i) = va_arg( ap , INDEX ) ;
   va_end( ap ) ;
   return *this ;
} // indexArray::assign

void indexArray::operator = ( indexArray& array )
{
   static char *mName = "op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i, len = array.length() ;
   if ( length() != len )
      reset( len ) ;
   for ( i = 1 ; i <= len ; i++ )
      elem(i) = array(i) ;
   return ;
} // matrix = indexArray

outFile& indexArray::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i, len = length() ;
   int format = matFormat() ;
   if ( format & DISPLAY ) {
      INDEX oldWidth = matField( 7 ) ;
      f.newLine() ;
      f.put( "Element" ).put( "Value" ).newLine() ;
      f.put( "-------" ).put( "-----" ).newLine() ;
      for ( i = 1 ; i <= len ; i++ ) {
         f.putIndex(i) ;
         f.putIndex( elem(i) ).newLine() ;
      } // for
      f.newLine() ;
      matField( oldWidth ) ;
      return f  ;
   } else {
      int cp, n = 0;
      cp = ( matPageWidth() / ( matField() + 2) ) - 1;
      for ( i = 1; i <= len ; i++ ) {
         f.putIndex(elem(i));
         if ( ++n >= cp ) {
            f.newLine();
            n = 0;
         } // if
      } // for
      f.newLine();
      return f ;
   } // else
} // indexArray put

outFile& indexArray::print( char* label, outFile& f ) M_CONST
{
   static char *mName = "print" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.newLine() ;
   if ( label != 0 )
      f.write( label ).newLine() ;
   int oldform = matFormat(DISPLAY) ;
   put( f ) ;
   matFormat( oldform ) ;
   return f ;
} // indexArray print

indexArray& indexArray::read( void )
{
   static char *mName = "read" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errout.newLine().write( "Input index array with " ) ;
   errout.writeIndex( length() ).write( " elements : " ) ;
   errout.newLine() ;
   get( in ) ;
   errout.write( "Ok.").newLine() ;
   return *this ;
} // indexArray read

outFile& indexArray::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 3 )
      return f ;
   objectInfo( f ) ;
   putName( nm.array(), f ) ;
   putName( "indexAr", f ) ;
   putField( length(), f ) ;
   f.newLine() ;
   return f ;
} // indexArray::info

#ifdef __cplusplus
matrix::matrix( indexArray& index ) : matObject()
#else
matrix::matrix( indexArray& index ) : ()
#endif
{
   static char *mName="matrix(ind)" ;
   matFunc func( mName ) ;
   INDEX i, len = index.length() ;
   pm = new matMap( len, 1 ) ;
   for ( i = 1 ; i <= len ; i++ )
       mat(i) = REAL( index(i) ) ;
   debugInfo( func ) ;
} // matrix( indexArray )

indexArray& matrix::toIndex( indexArray& index )
{
   static char *mName = "index=mat" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   INDEX n = nRows() ;
   if ( index.length() != n )
      index.reset(n) ;
   for ( INDEX i = 1 ; i <= n ; i++ )
      index(i) = (INDEX) ( mat(i) ) ;
   return index ;
} // matrix :: toIndex

void matrix::operator = ( indexArray& array )
{
   static char *mName = "matrix=ind" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i, len = array.length() ;
   reset( len, 1 ) ;
   for ( i = 1 ; i <= len ; i++ )
      mat(i) = REAL( array(i) ) ;
   return ;
} // matrix = indexArray

/**********************************************************/
/*  Methods to select elements for rows and columns       */
/**********************************************************/

matrix& matrix::rowSelectOf( const matrix& x,
                             const indexArray& cindex )
{
   static char *mName = "rowSelectOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = cindex.length(), i, j, xnr = x.nRows(),
         xnc = x.nCols() ;
   if ( nr != xnr ) {
      error( mName, NEDIM ) ;
      errorExit( mName, NEDIM ) ;
   } // if
   reset( nr ) ;
   for ( i = 1; i <= nr ; i++ ) {
      j = cindex(i) ;
      if ( ( j < 1 ) || ( j > xnc ) ) {
         errorij( j, 1 ) ;
         errorExit( mName, NRANG ) ;
      } // if
      mat(i) = x(i,j) ;
   } // for i
   return  *this ;
} // matrix::rowSelectOf

matrix& matrix::colSelectOf( const matrix& x,
                            const indexArray& rindex )
{
   static char *mName = "colSelectOf" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nr = rindex.length(), i, j, xnr = x.nRows(),
         xnc = x.nCols() ;
   if ( nr != xnc ) {
      error( mName, NEDIM ) ;
      errorExit( mName, NEDIM ) ;
   } // if
   reset( nr ) ;
   if ( nr != xnc ) {
      error( mName, NEDIM ) ;
      errorExit( mName, NEDIM ) ;
   } // if
   for ( j = 1; j <= nr ; j++ ) {
      i = rindex(j) ;
      if ( ( i < 1 ) || ( i > xnr ) ) {
         errorij( i, 1 ) ;
         errorExit( mName, NRANG ) ;
      } // if
      mat(j) = x(i,j) ;
   } // for j
   return *this ;
} // matrix::colSelect

matrix& matrix::subOf( const matrix& x, const indexArray& rmap,
                      const indexArray& cmap )
{
   static char *mName = "sub(maps)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX nc = cmap.length(), nr = rmap.length(), i, j;
   INDEX xnr = x.nRows(), xnc = x.nCols(), xi, xj ;
   reset( nr, nc ) ;
   for ( i = 1 ; i <= nr; i++ ) {
      xi = rmap(i) ;
      if ( ( xi < 1 ) || ( xi > xnr ) )
         errorExit( mName, NRANG ) ;
      for ( j = 1; j <= nc ; j++ ) {
         xj = cmap(j) ;
         if ( ( xj < 1 ) || ( xj > xnc ) )
            errorExit( mName, NRANG ) ;
         mat(i,j) = x( xi, xj ) ;
      } // for j
   } // for i
   return *this ;
} // matrix::subOf(maps)

matrix& matrix::rowsOf( const matrix& x, const indexArray& map )
{
   static char *mName = "rows" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = map.length(), nr = x.nRows(), nc = x.nCols(), i, j, k ;
   reset( n, nc ) ;
   for ( i = 1 ; i <= n; i++ ) {
      k = map(i) ;
      if ( ( k < 1 ) || ( k > nr ) )
         errorExit( mName, NRANG ) ;
      for ( j = 1; j <= nc ; j++ )
         mat(i,j) = x(k,j) ;
   } // for  i
   return *this ;
} // matrix::rowsOf

matrix& matrix::colsOf( const matrix& x, const indexArray& map )
{
   static char *mName = "cols" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX n = map.length(), nr = x.nRows(), nc = x.nCols(), i, j, k ;
   reset( nr, n ) ;
   for ( j = 1; j <= n ; j++ ) {
      k = map(j) ;
      if ( ( k < 1 ) || ( k > nc ) )
         errorExit( mName, NRANG ) ;
      for ( i = 1 ; i <= nr; i++ )
         mat(i,j) = x(i,k) ;
   } // for j
   return *this ;
} // matrix::colsOf

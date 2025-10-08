/*****************************************************/
/*           matmap.c source for matMap class        */
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

/***********************************************************/
/*  Basic constructor generates the underlying array as    */
/*  well as the map for the matrix.                        */
/***********************************************************/

   /****************************************/
   /* Both pa and map are offset back by   */
   /* one to facilitate unit base address- */
   /* ing of elements. In particular pa[j] */
   /* will refer to the jth realArray and  */
   /* map[j][i] to the ith element of the  */
   /* jth array.                           */
   /****************************************/

#ifdef __cplusplus
matMap::matMap( INDEX nr, INDEX nc ) : matObject()
#else
matMap::matMap( INDEX nr, INDEX nc ) : ()
#endif
{
   static char *mName = "matMap(,)" ;
   matFunc func( mName ) ;
   INDEX i ;
   pa = new realArray(nr,nc) ;
   if ( ( map = new REAL*[nc] ) == 0 )
      errorExit( mName, NOMEM ) ;
   map-- ;
   for ( i = 1 ; i <= nc ; i++ )
      map[i] = pa->base[i] - 1 ;
   nrow = nr ;
   ncol = nc ;
   mapSize = nc ;
   ref = 1 ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matMap::matMap( INDEX, INDEX )

#ifdef __cplusplus
matMap::matMap( void ) : matObject()
#else
matMap::matMap( void ) : ()
#endif
{
   static char *mName = "matMap()" ;
   matFunc func( mName ) ;
   pa = 0 ;
   map = 0 ;
   nrow = 0 ;
   ncol = 0 ;
   mapSize = 0 ;
   ref = 1 ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matMap::matMap( INDEX, INDEX )

#ifdef __cplusplus
matMap::matMap( const matMap& m ) : matObject()
#else
matMap::matMap( const matMap& m ) : ()
#endif
{
   static char *mName = "matMap(&m)" ;
   matFunc func( mName ) ;
   nrow = m.nrow ;
   ncol = m.ncol ;
   if ( ( map = new REAL*[ncol] ) == 0 )
      errorExit( mName, NOMEM ) ;
   map-- ;
   for ( INDEX i = 1 ; i <= ncol ; i++ )
      map[i] = m.map[i] ;
   pa = m.pa ;
   if ( pa != 0 )
      pa->ref++ ;
   mapSize = ncol ;
   ref = 1 ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matMap::matMap( matMap& )

/************************************************/
/* The following constructor generates a subMap */
/* for an existing map to facilitate reference  */
/* matrices.                                    */
/************************************************/

#ifdef __cplusplus
matMap::matMap( const matMap& m, INDEX r1, INDEX r2,
		INDEX c1, INDEX c2 ) : matObject()
#else
matMap::matMap( const matMap& m, INDEX r1, INDEX r2,
		INDEX c1, INDEX c2 ) : ()
#endif
{
   static char *mName = "matMap(&m,,,)" ;
   matFunc func( mName ) ;
   if ( r1 < 1 || r1 > r2 || r2 > m.nRows() ||
	c1 < 1 || c1 > c2 || c2 > m.nCols() )
      m.errorExit( mName, NRANG ) ;
   INDEX nr = r2 - r1 + 1, nc = c2 - c1 + 1 ;
   if ( ( map = new REAL*[nc] ) == 0 )
      errorExit( mName, NOMEM ) ;
   map-- ;
   for ( INDEX i = 1 ; i <= nc ; i++ )
      map[i] = m.map[c1+i-1] + r1 - 1 ;
   pa = m.pa ;
   if ( pa != 0 )
      pa->ref++ ;
   nrow = nr ;
   ncol = nc ;
   mapSize = nc ;
   ref = 1 ;
   func.trace(TRUE) ; // debugInfo( func ) ;
} // matMap::matMap( matMap& ,,, )

matMap::~matMap( void )
{
   static char *mName = "~matMap" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   clear() ;
} // matMap::~matMap

void matMap::reset( INDEX nr, INDEX nc )
{
   static char *mName = "reset" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pa != 0 && --pa->ref == 0 )
      delete pa ;
   pa = new realArray(nr,nc) ;
   if ( map != 0 ) {
      map++ ;
      delete map ;
   } // if
   if ( ( map = new REAL*[nc] ) == 0 )
      errorExit( mName, NOMEM ) ;
   map-- ;
   for ( INDEX i = 1 ; i <= nc ; i++ )
      map[i] = pa->base[i] - 1 ;
   nrow = nr ;
   ncol = nc ;
   mapSize = nc ;
} // matMap reset

REAL& matMap::elem( INDEX i, INDEX j ) M_CONST
{
   if ( i < 1 || i > nrow || j < 1 || j > ncol )
      errorExit( "elem(,)", NRANG ) ;
   return map[j][i] ;
} //matMap()

matMap& matMap::operator = ( const matMap& m )
{
   static char *mName = "op =" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   INDEX i ;
   if ( map != 0 ) {
      map++ ;
      delete map ;
   } // if
   clear() ;
   nrow = m.nrow ;
   ncol = m.ncol ;
   mapSize = ncol ;
   if ( ( map = new REAL*[ncol] ) == 0 )
      errorExit( mName, NOMEM ) ;
   map-- ;
   for ( i = 1 ; i <= ncol ; i++ )
      map[i] = m.map[i] ;
   pa = m.pa ;
   if ( pa != 0 )
      pa->ref++ ;
   return *this ;
} // matMap =

charArray& matMap::name( char * name )
{
   nm = name ;
   return nm ;
} // matMap name

outFile& matMap::info( outFile& f ) M_CONST
{
   if ( matListCtrl() > 2 ) 
      return f ;
   objectInfo( f ) ;
   putName( nm.array(), f ) ;
   putName( "matMap", f ) ;
   putField( nrow, f ) ;
   putField( ncol, f ) ;
   if ( pa != 0 )
      putField( pa->identity(), f ) ;
   putField( ref, f );
   f.newLine() ;
   return f ;
} // matMap::info

outFile& matMap::put( outFile& f ) M_CONST
{
   static char *mName = "put" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   f.write( "\n\n****matMap.put not implemented****\n\n") ;
   return f ;
} // matMap::put

inFile& matMap::get( inFile& f )
{
   static char *mName = "get" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   errout.write( "\n\n****matMap.get not implemented****\n\n") ;
   return f ;
} // matMap::get

void matMap::clear( void )
{
   static char *mName = "clear" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( pa != 0  && --pa->ref == 0 )
      delete pa ;
   pa = 0 ;
   if ( map != 0 ) {
      map++ ;
      delete map ;
   } // if
   map = 0 ;
   nrow = 0 ;
   ncol = 0 ;
} // matMap::clear

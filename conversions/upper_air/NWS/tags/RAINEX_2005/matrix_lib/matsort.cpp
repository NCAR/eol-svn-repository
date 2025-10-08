/**************************************************/
/*  matsort.c source for MatClass sort functions  */
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

void matrix::shellSort( void )
{
   // see Sedgwick p.98
   // this is sorted directly
   static char *mName = "shellSort" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   INDEX nr = nRows(), h, i, j ;
   REAL xi ;
   // initialise step length h to be > nr
   for ( h = 1 ; h <= nr ; h = 3 * h + 1 )
      ;
   do {
      h /= 3 ; // reduce step length
      // h-sort
      for ( i = h + 1 ; i <= nr ; i++ ) {
	 xi = mat(i) ;
	 j = i ;
	 while ( j > h ) {
	    if ( mat(j-h) < xi )
	       break ; // end while
	    mat(j) = mat(j-h) ;
	    j -= h ;
	 } // while
	 mat(j) = xi ;
      } // for i
   } while ( h > 1 ) ;
} // shellSort

void matrix::shellMap( indexArray& m ) M_CONST
{
   // see Sedgwick p.98
   // this is not sorted but returns a map to the sort
   static char *mName = "shellMap(m)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   INDEX nr = nRows(), h, i, j, mi ;
   REAL xi ;
   if ( nr != m.length() )
      m.reset(nr) ;
   // initial index map
   for ( i = 1; i <= nr; i++ )
      m(i) = i ;
   // initialise step length h to be > nr
   for ( h = 1 ; h <= nr ; h = 3 * h + 1 )
      ;
   do {
      h /= 3 ; // reduce step length
      // h-sort
      for ( i = h + 1 ; i <= nr ; i++ ) {
	 mi = m(i) ;
	 xi = mat(mi) ;
	 j = i ;
	 while ( j > h ) {
	    if ( mat(m(j-h)) < xi )
	       break ; // end while
	    m(j) = m(j-h) ;
	    j -= h ;
	 } // while
	 m(j) = mi ;
      } // for i
   } while ( h > 1 ) ;
} // shellMap

void matrix::heapSort( void )
{
   // sort this
   static char *mName = "heapSort" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   INDEX nr = nRows(), bottom, top, parent, child ;
   REAL target;
   bottom = nr ;
   top = ( bottom / 2 ) + 1 ;
   while ( bottom > 1 ) {
      if ( top > 1 ) {  // promote up to form heap
	 --top ;
	 target = mat( top ) ;
      } else {          // drop largest to bottom
	 target = mat( bottom ) ;
	 mat( bottom ) = mat(1) ;
	 --bottom ;
      } // else
      parent = top ;
      child = 2 * top ;
      while ( child <= bottom ) {
	 if ( ( child < bottom ) &&  mat(child) < mat(child+1) )
	    ++child ;
	 if ( target < mat( child ) ) {
	    mat( parent ) = mat( child ) ;
	    parent = child ;
	    child += parent ;
	 } else
	    child = bottom + 1 ;
      } // while child
      mat( parent ) = target ;
   } // while bottom
} // heapSort

void matrix::heapMap( indexArray& m ) M_CONST
{
   // does not sort this but return index map of sort in m
   static char *mName = "heapMap(m)" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   if ( nCols() != 1 )
      errorExit( mName, NOTVC ) ;
   INDEX nr = nRows(), bottom, top, parent, child, i ;
   INDEX target ;
   REAL  targetValue, childValue ;
   // initial index map
   if ( nr != m.length() )
      m.reset(nr) ;
   for ( i = 1; i <= nr; i++ )
      m(i) = i ;
   bottom = nr ;
   top = ( bottom / 2 ) + 1 ;
   while ( bottom > 1 ) {
      if ( top > 1 ) {  // promote up to form heap
	 --top ;
	 target = m(top) ;
      } else {          // drop largest to bottom
	 target = m(bottom) ;
	 m(bottom) = m(1) ;
	 --bottom ;
      } // else
      parent = top ;
      child = 2 * top ;
      while ( child <= bottom ) {
	 childValue = mat( m(child) ) ;
	 if ( ( child < bottom ) &&  childValue < mat( m(child+1) ) )
	    ++child ;
	 targetValue = mat(target) ;
	 if ( targetValue < mat( m(child) ) ) {
	    m(parent) = m(child) ;
	    parent = child ;
	    child += parent ;
	 } else
	    child = bottom + 1 ;
      } // while child
      m(parent) = target ;
   } // while bottom
} // heapMap

indexArray matrix::heapMap( void ) M_CONST
{
   static char *mName = "heapMap()" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() ) ;
   heapMap( map ) ;
   return map ;
} // matrix::heapMap

indexArray matrix::shellMap( void ) M_CONST
{
   static char *mName = "shellMap()" ;
   matFunc func( mName ) ; debugInfo( func ) ;
   indexArray map( nRows() ) ;
   shellMap( map ) ;
   return map ;
} // matrix::shellMap

matrix rankings( indexArray& m )
{
   // assumes m is an sort index
   static char *mName = "rankings" ;
   matFunc func( mName ) ; m.debugInfo( func ) ;
   INDEX nr = m.length(), i, k ;
   matrix z( nr ) ;
   for ( i = 1; i <= nr ; i++ ) {
      k = m(i) ;
      if ( ( k < 1 ) || ( k > nr ) ) {
	 m.errorij( i, 1 ) ;
	 m.errorExit( mName, NRANG ) ;
      } else
	 z(k) = (REAL) i ;
   } // for i
   return z ;
} // rankings

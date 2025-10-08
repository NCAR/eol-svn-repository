/**************************************************/
/*      matpair.c source for matpair class        */
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


/*************************************************/
/*                 matPair class                 */
/*************************************************/


matPair::matPair( void )
{
   i = j = r1 = c1 = 1 ;
   r2 = 0 ;
   c2 = 0 ;
   inBounds = FALSE ;
} // matPair

matPair::matPair( INDEX nr, INDEX nc )
{
   i = j = r1 = c1 = 1 ;
   r2 = nr ;
   c2 = nc ;
   inBounds = ( i <= r2 && j <= c2 ) ;
} // matPair

matPair::matPair( INDEX nr1, INDEX nr2, INDEX nc1, INDEX nc2 )
{
   i = r1 = nr1 ;
   j = c1 = nc1 ;
   r2 = nr2 ;
   c2 = nc2 ;
   inBounds = ( r1 <= i && i <= r2 && c1 <= j && j <= c2 ) ;
} // matPair

matPair::~matPair( void ) {} // matPair

matPair::matPair( matPair& p )
{
   i = p.i ;
   j = p.j ;
   r1 = p.r1 ;
   r2 = p.r2 ;
   c1 = p.c1 ;
   c2 = p.c2 ;
   inBounds = p.inBounds ;
} // matPair( matPair& )

matPair& matPair::operator = ( matPair& p )
{
   i = p.i ;
   j = p.j ;
   r1 = p.r1 ;
   r2 = p.r2 ;
   c1 = p.c1 ;
   c2 = p.c2 ;
   inBounds = p.inBounds ;
   return *this ;
} // matPair op =

void matPair::range( INDEX nr1, INDEX nr2, INDEX nc1, INDEX nc2 )
{
   i = r1 = nr1 ;
   j = c1 = nc1 ;
   r2 = nr2 ;
   c2 = nc2 ;
   inBounds = ( r1 <= i && i <= r2 && c1 <= j && j <= c2 ) ;
} // matPair

void matPair::first( void )
{
   i = r1 ;
   j = c1 ;
   inBounds = ( i <= r2 && j <= c2 ) ;
} // matPair::first

void matPair::last( void )
{
   i = r2 ;
   j = c2 ;
   inBounds = ( i >= r1 && j >= c1 ) ;
} // matPair::first

void matPair::operator ++ ( void )
{
   if ( i < r2 )
      i++ ;
   else if ( j < c2 ) {
      j++ ;
      i = r1 ;
   } else
      inBounds = 0 ;
} // matPair ++

void matPair::operator -- ( void )
{
   if ( i > r1 )
      i-- ;
   else if ( j > c1 ) {
      j-- ;
      i = r2 ;
   } else
      inBounds = 0 ;
} // matPair --

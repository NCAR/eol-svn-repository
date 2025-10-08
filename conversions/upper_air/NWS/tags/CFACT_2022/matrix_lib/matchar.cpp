/**************************************************/
/*      matchar.c source for charArray class      */
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


void charArray::allocate( INDEX n )
{
   static const INDEX minSize = 8 ;
   cpSize = ( ( ( n + 1 ) / minSize ) + 1 ) * minSize ;
   cp = new char[ cpSize + 1 ] ;
   if ( cp == 0 )
      matErrNumExit( "charArray allocate", NOMEM ) ;
} // charArray allcoate

void charArray::reallocate( INDEX n )
{
   if ( n > cpSize ) {
     if ( cp != 0 )
        delete cp ;
     allocate( n ) ;
   } // if
} // charArray reallocate

charArray::charArray( void )
{
   allocate(0) ;
   cp[0] = '\0' ;
} // charArray( void )

charArray::charArray( INDEX n )
{
   allocate( n ) ;
   cp[0] = '\0' ;
} // charArray(n)

charArray::charArray( char *str )
{
   static char *mName = "charArray(str)" ;
   if ( str == 0 )
      matErrNumExit( mName, NULRF ) ;
   INDEX n = strlen( str ) ;
   allocate( n ) ;
   strcpy( cp, str ) ;
} // charArray(str)

charArray::charArray( const charArray& ca )
{
   static char *mName = "charArray(str)" ;
   if ( ca.cp == 0 )
      matErrNumExit( mName, NULRF ) ;
   allocate( ca.length() ) ;
   strcpy( cp, ca.cp ) ;
} // charArray(str)

charArray& charArray::operator = ( const charArray& ca )
{
   static char *mName = "charArray(str)" ;
   if ( ca.cp == 0 )
      matErrNumExit( mName, NULRF ) ;
   INDEX n = strlen( ca.cp ) ;
   reallocate( n ) ;
   strcpy( cp, ca.cp ) ;
   return *this ;
} // charArray =

void charArray::clear( void )
{
   cp[0] = '\0' ;
   return ;
} // charArray::clear

charArray::~charArray( void )
{
   delete cp ;
} // ~charArray

charArray& charArray::operator=( char* str )
{
   static char* mName = "charArray=" ;
   if ( str == 0 )
      matErrNumExit( mName, NULRF ) ;
   INDEX n = strlen( str ) ;
   reallocate( n ) ;
   strcpy( cp, str ) ;
   return *this ;
} // charArray = char*

INDEX charArray::length( void ) M_CONST
{
   return strlen( cp ) ;
} // charArray size

#if 0

outFile& charArray::put( outFile& f ) M_CONST
{
   return f.put( cp ) ;
} // charArray::put

inFile& charArray::get( inFile& f )
{
   return f.get( cp, cpSize ) ;
} // charArray::get

#endif

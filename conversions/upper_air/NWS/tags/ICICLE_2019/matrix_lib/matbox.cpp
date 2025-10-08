/**************************************************/
/*     matbox.c source for matList family         */
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

#include "matbox.hpp"

matList::matList( void )
{
   sentinel = new matListNode ;
   sentinel->item = 0 ;
   sentinel->next = 0 ;
   current = sentinel ;
   top     = sentinel ;
} // matList

matList::matList( matList& list )
{
   list.gotoHead() ;
   matErrNumExit( "matList copy", NIMPL ) ;
} // matList

void matList::operator = ( matList& list )
{
   list.gotoHead() ;
   matErrNumExit( "matList op =", NIMPL ) ;
} // matList

matList::~matList( void )
{
   current = top ;
   while ( current != sentinel ) {
      top = top->next ;
      delete current ;
      current = top ;
   } // while
   delete sentinel ;
} // ~matList

#include <string.h> // for strcpy

charArray matList::name( char *newName )
{
   if ( newName != 0 )
      nm = newName ;
   return nm ;
} // matrix::name

int matList::store( void* item )
{
   matListNode *temp = new matListNode ;
   if ( temp == 0 )
      return 1 ;
   temp->item = item ;
   temp->next = top ;
   top = temp ;
   return 0 ;
} // matList::store

void* matList::examine( void )
{
   if ( current == sentinel )
      return 0 ;
   else
      return current->item ;
} // matList::examine

void* matList::retrieve( void )
{
   void *value = examine() ;
   if ( value != 0 )
      remove( value ) ;
   return value ;
} // matList::retrieve

int matList::remove( void* target )
{
   if ( top == sentinel )
      return 2 ;
   matListNode *temp, *nxt ;
   if ( top->item == target ) {
      nxt = top->next ;
      if ( current == top )
         current = nxt ;
      delete top ;
      top = nxt ;
      return 0 ;
   } // if
   sentinel->item = target ;
   temp = top ;
   while ( temp->next->item != target )
      temp = temp->next ;
   if ( temp->next == sentinel )
      return 1 ;
   nxt = temp->next ;
   if ( current == nxt )
      current = nxt->next ;
   temp->next = nxt->next ;
   delete nxt ;
   return 0 ;
} // matList::remove

int matList::endOfList( void )
{
   return current == sentinel ;
} // matList::endOfList

void matList::gotoHead( void )
{
   current = top ;
} // matList:: gotoHead

void matList::gotoNext( void )
{
   if ( current != sentinel )
      current = current->next ;
} // matList:: gotoNext


/***********************************************/
/*   matObjList : container for  matObjects    */
/***********************************************/

#ifdef __cplusplus
matObjList::matObjList( void ) : matList() {}
#else
matObjList::matObjList( void ) : () {}
#endif

matObjList::~matObjList( void ) {}


#ifdef __cplusplus
matObjList::matObjList( matObjList& list ) : matList(list) {}
#else
matObjList::matObjList( matObjList& list ) : (list) {}
#endif

void matObjList::operator = ( matObjList& list )
{
      matList::operator = ( list ) ;
} // matObjList =

int matObjList::append( matObject* objPtr )
{
   return store( (void *) objPtr ) ;
} // matObjList::append

matObject* matObjList::peek( void )
{
   return (matObject *) examine() ;
} // matObjList::peek

matObject* matObjList::extract( void )
{
   return (matObject *) retrieve() ;
} // matObjList::extract

void matObjList::print( outFile& f )
{
   gotoHead() ;
   if ( endOfList() )
      f.write( "List Empty" ).newLine() ;
   else
      while ( !endOfList() ) {
         peek()->info( f ) ;
         gotoNext() ;
      } // while
   f.newLine() ;
} // matObjList

/************************************************/
/*     matrixList : container for matrices      */
/************************************************/

#ifdef __cplusplus
matrixList::matrixList( void ) : matObjList() {}
#else
matrixList::matrixList( void ) : () {}
#endif

matrixList::~matrixList( void ) {}

#ifdef __cplusplus
matrixList::matrixList( matrixList& list ) : matObjList(list) {}
#else
matrixList::matrixList( matrixList& list ) : (list) {}
#endif

void matrixList::operator = ( matrixList& list )
{
      matObjList::operator = ( list ) ;
} // matrixList =

int matrixList::attach( matrix* m )
{
   return store( (void *) m ) ;
} // matrixList::attach

matrix* matrixList::view( void )
{
   return (matrix *) examine() ;
} // matrixList::view

matrix* matrixList::detach( void )
{
   return (matrix *) retrieve() ;
} // matrixList::detach

void matrixList::dump( outFile& f )
{
   gotoHead() ;
   while ( !endOfList() ) {
      view()->put(f) ;
      gotoNext() ;
   } //while
} // matrixList::dump

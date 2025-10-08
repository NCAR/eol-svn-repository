/**************************************************/
/*    matbox.hpp header for matList family        */
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

#ifndef MATBOX_H

#define MATBOX_H

#ifndef MATRIX_H
#include "matrix.hpp"
#endif

struct matListNode 
{
   void        *item ;
   matListNode *next ;
} ; // matListNode

class matList 
{

    protected :

       charArray     nm ; // name for series
       matListNode   *top, *current, *sentinel ;

    public :

       matList( void ) ;
       ~matList( void ) ;
       matList( matList& list ) ;
       void operator = ( matList& list ) ;

       charArray name( char *newName = 0 ) ;

       virtual int   store( void* item ) ;
       virtual void* examine( void ) ;
       virtual void* retrieve( void ) ;
       virtual int   remove( void* target ) ;
       virtual int   endOfList( void ) ;
       virtual void  gotoHead( void ) ;
       virtual void  gotoNext( void ) ;

} ; // matList

class matObjList : public matList 
{

   public :

      matObjList( void ) ;
      ~matObjList( void ) ;
      matObjList( matObjList& list ) ;
      void operator = ( matObjList& list ) ;

      virtual int        append( matObject* obj ) ;
      virtual matObject* peek( void ) ;
      virtual matObject* extract( void ) ;

      virtual void print( outFile& f = out ) ;

} ; // matObjList

class matrixList : public matObjList 
{

   public :

      matrixList( void ) ;
      matrixList( matrixList& list ) ;
      ~matrixList( void ) ;
      void operator = ( matrixList& list ) ;

      virtual int     attach( matrix* m ) ;
      virtual matrix* view( void ) ;
      virtual matrix* detach( void ) ;

      void dump( outFile& f = out ) ;

} ; // matrixList

#endif

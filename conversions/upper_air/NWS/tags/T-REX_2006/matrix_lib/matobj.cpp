/*****************************************************/
/*        matobj.c source for matObject class        */
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

#include "matbox.hpp"

static matObjList mainObjList ;

/************** object functions ****************/

#ifndef NO_STATIC_DEC
#ifdef __cplusplus
INDEX       matObject::count ;
int         matObject::listCtrl ;
#endif
#endif

void matObjectInitial( void )
{
   matObject::count = 0 ;
   matObject::listCtrl = 0 ;
} // matObjectInitial

matObject::matObject( void )
{
   id = ++count ;
   mainObjList.append( this ) ;
} // matObject::matObject

matObject::~matObject( void )
{
   mainObjList.remove( this ) ;
} // matObject::matObject

matObject::matObject( const matObject& obj )
{
   if ( obj.identity() ) {
      id = ++count ;
      mainObjList.append( this ) ;
   } // if
} // matObject copy constructor

matObject& matObject::operator = ( const matObject& obj )
{
   if ( id == obj.identity() )
      errorExit( "op =", OVPAR ) ;
   return *this ;
} // matObject op =

outFile& matObject::info( outFile& f ) M_CONST
{
   f.write( "Unknown Object" ).newLine() ;
   return f ;
} // matObject::info

outFile& matObject::put( outFile& f ) M_CONST
{
   f.write( "Unknown Object" ).newLine() ;
   return f ;
} // matObject::put

inFile& matObject::get( inFile& f )
{
   f.nextLine() ;
   return f ;
} // matObject::get

static const int INFOWIDTH = 6 ;

outFile& matObject::putName( const char* name, outFile& f ) M_CONST
{
   f.put( name, INFOWIDTH + 2 ).write( " " ) ;
   return f ;
} // matObject::putField

outFile& matObject::putField( INDEX i, outFile& f ) M_CONST
{
   f.putIndex( i, INFOWIDTH ).write( " " ) ;
   return f ;
} // matObject::putField

outFile& matObject::putField( const char* label, outFile& f ) M_CONST
{
   f.put( label, INFOWIDTH ).write( " " ) ;
   return f ;
} // matObject::putField

outFile& matObject::objectInfo( outFile& f ) M_CONST
{
   putField( identity(), f ) ;
   return f ;
} // matObject::objectInfo

int matListCtrl( int newCtrl )
{
   int oldCtrl = matObject::listCtrl ;
   if ( newCtrl >= 0 )
      matObject::listCtrl = newCtrl ;
   return oldCtrl ;
} // matListCtrl

void matObjectList( outFile& f, int ctrl )
{
   int oldCtrl = matListCtrl( ctrl ) ;
   f.newLine() ;
   f.put( "Object", INFOWIDTH ).write( " " ) ;
   f.put( "  Name  ", INFOWIDTH + 2 ).write( " " ) ;
   f.put( "  Type  ", INFOWIDTH + 2 ).write( " " ) ;
   f.put( "Field1", INFOWIDTH ).write( " " ) ;
   f.put( "Field2", INFOWIDTH ).write( " " ) ;
   f.put( "Field3", INFOWIDTH ).write( " " ) ;
   f.put( "Field4", INFOWIDTH ).write( " " ) ;
   f.put( "Field5", INFOWIDTH ).write( " " ) ;
   f.newLine() ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.put( "--------", INFOWIDTH + 2 ).write( " " ) ;
   f.put( "--------", INFOWIDTH + 2 ).write( " " ) ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.put( "------", INFOWIDTH ).write( " " ) ;
   f.newLine() ;
   mainObjList.print(f) ;
   f.newLine() ;
   matListCtrl( oldCtrl ) ;
} // matObjectList

void matObject::clear( void ) {
   matFunc func( "obj clear" ) ;
} // matObject::clear

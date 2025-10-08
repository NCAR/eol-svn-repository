/*****************************************************/
/*        matfunc.c source for matFunc class         */
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

#ifndef NO_STATIC_DEC
#ifdef __cplusplus
INDEX matFunc::count ;
INDEX matFunc::debug ;
#endif
#endif

outFile *matTraceFile = &out ;

static matList matFuncCalls ;

void matFuncInitial( void )
{
   matFunc::count = 0 ;
   matFunc::debug = 0 ;
} // matFuncInitial

void traceFile( outFile& newTrace )
{
   if ( newTrace.fileType() != OUTPUT )
      matErrNumExit( "traceFile", BDARG ) ;
   matTraceFile = &newTrace ;
   return ;
} // traceFile

matFunc::matFunc( char *func )
{
   static char *mName = "matFunc" ;
   count++ ;
   if ( debug >= 1024 ) {
      matTraceFile->write( "#" ).put( mName, 14 ) ;
      matTraceFile->write( " [" ).putIndex( count, 2 ) ;
      matTraceFile->write( "]" ).newLine() ;
   } // if
   name = func ;
   if ( matFuncCalls.store( this ) ) {
      matFuncList() ;
      matErrNumExit( mName, BDLST ) ;
   } // if
} // matFunc

matFunc::matFunc( const matFunc& f )
{
   static char *mName = "matFunc(&F)" ;
   count++ ;
   if ( debug >= 1024 ) {
      matTraceFile->write( "#" ).put( mName, 14 ) ;
      matTraceFile->write( " [" ).putIndex( count, 2 ) ;
      matTraceFile->write( "]" ).newLine() ;
   } // if
   name = f.name ;
   if ( matFuncCalls.store( this ) ) {
      matFuncList() ;
      matErrNumExit( mName, BDLST ) ;
   } // if
} // matFunc

matFunc& matFunc::operator = ( const matFunc& f )
{
   name = f.name ;
   matErrNumExit( "matFunc =", NIMPL ) ;
   return *this ;
} // matFunc

matFunc::~matFunc( void )
{
   static char *mName = "~matFunc" ;
   if ( debug >= 1024 ) {
      matTraceFile->write( "#" ).put( "~matFunc", 14 ) ;
      matTraceFile->write( " [" ).putIndex( count, 2 ) ;
      matTraceFile->write( "]" ).newLine() ;
   } // if
   count-- ;
   int err = matFuncCalls.remove( this ) ;
   if ( err ) {
      matFuncList() ;
      if ( err == 1 )
         matErrNumExit( mName, NULST ) ;
      else
         matErrNumExit( mName, BDLST ) ;
   } // if
} // ~matFunc

INDEX setMatDebug( INDEX dbg )
{
   int old = matFunc::debug ;
   matFunc::debug = dbg ;
   return old ;
} // setMatDebug

void matFunc::info( outFile& f ) M_CONST
{
   f.write( "#" ) ;
   f.put( name.array(), 14 ) ;
   f.write( " [" ).putIndex( count, 2 ).write( "]" ) ;
} // matFunc::info

int matFunc::trace( INDEX lineFeed ) M_CONST
{
   if ( count <= debug ) {
      info( *matTraceFile ) ;
      if ( lineFeed )
         matTraceFile->newLine() ;
      return 1 ;
   } else
      return 0 ;
} // matFunc::trace

void matFuncList( void )
{
   matFunc* funcPtr ;
   INDEX    n = matFunc::count ;

   matTraceFile->newLine().write( "Stack of Function Calls" ) ;
   matTraceFile->newLine(2) ;
   matFuncCalls.gotoHead() ;
   if ( matFuncCalls.endOfList() ) {
      matTraceFile->write( "Stack Empty" ).newLine() ;
   } else while ( !matFuncCalls.endOfList() ) {
      funcPtr = (matFunc *) matFuncCalls.examine() ;
      matFuncCalls.gotoNext() ;
      matTraceFile->write( "# " ) ;
      matTraceFile->put( funcPtr->name.array(), 14 ) ;
      matTraceFile->write( "[" ).putIndex( n--, 2 ).write( "]" ) ;
      matTraceFile->newLine() ;
   } // while
} // matFuncList

void matObject::debugInfo( const matFunc& func ) M_CONST
{
   if ( func.trace() ) {
      info( *matTraceFile ) ;
      matTraceFile->flush() ;
   } // if
} // matFunc::debugInfo

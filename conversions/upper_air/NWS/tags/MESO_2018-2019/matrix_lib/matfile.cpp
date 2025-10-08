/**************************************************/
/*     matfile.c source for matFile family        */
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

#include "matfile.hpp"

#include <stdlib.h>

outFile out( "stdout", STDOUT ), errout( "stderr", STDERR ) ;
inFile  in( "stdin", STDIN ) ;

void matFileInitial( void )
{
} // matFileInitial

#include <string.h>

static char *fileMess[] = {
   "Unknown File Error" ,
   "Wrong File Type",
   "Failed request for New Stream",
   "File Open Failure",
   "Bad File Handle",
   "Read Failure",
   "Write Failure",
   "File Naming Error : Memory Exhausted",
   "File Copy constructor not implemented",
   "File Open/Closed in Close/Open Operation"
} ; // fileMess

void fileError( INDEX errnum )
{
   fprintf( stderr,  "\n\nmatFile Error : %u %s\n\n",
            errnum, fileMess[errnum] ) ;
   exit(2) ;
} // fileError


/********************************************************/
/*                  matFile Modules                     */
/********************************************************/

matFile::matFile( void )
{
   fh = new fileHandle ;
   fh->count = 1 ;
   type  = NULLFILE ;
   ok    = TRUE ;
} // matFile

matFile::matFile( const matFile& mf )
{
   fh = mf.fh ;
   if ( fh )
      fh->count++ ;
   type = mf.type ;
   ok   = mf.ok ;
} // matFile

matFile& matFile::operator = ( const matFile& mf )
{
   if ( fh && --fh->count )
      delete fh ;
   fh = mf.fh ;
   if ( fh )
      fh->count++ ;
   type = mf.type ;
   ok   = mf.ok ;
   return *this ;
} // matFile

matFile::~matFile( void )
{
   if ( !fh )
      fileError( NFILE ) ;
   closeFile() ;
   delete fh ;
} // ~matFile   

void* matFile::handle( void )
{
   if ( !fh )
      fileError( NFILE ) ;
   return fh->handle ;
} // matFile handle

INDEX matFile::openFile( const charArray& newName, INDEX newType )
{
   char* mode;
   if ( !fh )
      fileError( NFILE ) ;
   if ( type != NULLFILE )
      fileError( FBAD ) ;
   if ( fh->count > 1 )  {
      --fh->count ;
      fh = new fileHandle ;
      fh->handle = 0 ;
      fh->count = 1 ;
   } // if
   if ( newType == STDIN || newType == STDOUT || 
        newType == STDERR ) {
      type = newType ;
      return OK ;
   } // if
   if ( newType == INPUT )
      mode = "r" ;
   else if ( newType == OUTPUT )
      mode = "w" ;
   else
      fileError( FTYPE ) ;
   FILE *tmp = fopen( newName.array(), mode ) ;
   if ( tmp == 0 )
      fileError( FOPEN ) ;
   fh->handle = (void *)tmp ;
   fh->name = newName ;
   type = newType ;
   ok = TRUE ;
   return OK ;
} // openFile

INDEX matFile::closeFile( void )
{
   if ( type == NULLFILE )
      return OK ;
   if ( type == STDIN || type == STDOUT || type == STDERR )
      return OK ;
   if ( !fh || !fh->handle )
      fileError( NFILE ) ;
   if ( fh->count > 1 ) {
      --fh->count ;
      fh = new fileHandle ;
   } else 
      fclose( (FILE *) fh->handle ) ;
   fh->handle = 0 ;
   fh->name.clear() ;   
   type = NULLFILE ;
   ok   = TRUE ;
   return OK ;
} // closeFile

INDEX matFile::eof( void )
{
   if ( type == NULLFILE )
      return TRUE ;
   if ( type == STDIN || type == STDOUT || type == STDERR )
      return FALSE ; // ???
   if ( !fh || !fh->handle )
      fileError( NFILE ) ;
   return feof( (FILE *)fh->handle ) ;
} // matFile eof

/********************************************************/
/*                   inFile Modules                     */
/********************************************************/

int inFile::good( void )
{
   return ok && !eof() ;
} // inFile::good

int inFile::fail( void )
{
   return !ok ;
} // inFile::fail

int inFile::close( void )
{
   return closeFile() ;
} // inFile::close

int inFile::open( const charArray& newName, int newType )
{
   if ( newType != INPUT && newType != STDIN ) 
      fileError( FTYPE ) ;
   return openFile( newName, newType ) ;
} // inFile open

inFile::inFile( void ) : matFile()
{} // inFile( void )

inFile::inFile( const charArray& newName, int newType ) : matFile()
{
   if ( newType != INPUT && newType != STDIN ) 
      fileError( FTYPE ) ;
   openFile( newName, newType ) ;
} // inFile

inFile::inFile( const inFile& f ) : matFile( f )
{} // inFile

inFile::~inFile( void )
{} // ~inFile

inFile& inFile::getCh( char& ch )
{
   int chin ;
   if ( type == STDIN )
      chin = getchar() ;
   else if ( type == INPUT )
      chin = getc( (FILE *)handle() ) ;
   else
      fileError( FTYPE ) ;
   if ( chin == EOF )
      fileError( FREAD ) ;
   ch = (char) chin ;
   return *this ;
} // get ch

inFile& inFile::get( char *label, int length )
{
   int i = 0 ;
   int ch ;
   if ( type == STDIN ) {
      do {
         ch = getchar() ;
         if ( ch != '\n' )
            *label++ = ch ;
      } while ( ( ++i < length ) && ( ch != '\n' )
                && ( ch != EOF ) ) ;
      *label++ = '\0' ;
   } else if ( type == INPUT ) {
      FILE *f = (FILE *) handle() ;
      do {
         ch = getc(f) ;
         if ( ch != '\n' )
            *label++ = ch ;
      } while ( ( ++i < length ) && ( ch != '\n' )
                && ( ch != EOF ) ) ;
      *label++ = '\0' ;
   } else
      fileError( FTYPE ) ;
   return *this ;
} // get label

inFile& inFile::getReal( REAL& r )
{
   double d ;
   if ( type == STDIN )
      ok = scanf( "%lf", &d ) ;
   else if ( type == INPUT )
      ok = fscanf( (FILE*)handle(), "%lf", &d ) ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FREAD ) ;
   r = d ;
   return *this ;
} // get REAL

inFile& inFile::getIndex( INDEX& i )
{
   if ( type == STDIN )
      ok = scanf( "%u", &i )  ;
   else if ( type == INPUT )
      ok = fscanf( (FILE*)handle(), "%u", &i ) ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FREAD ) ;
   return *this ;
} // get INDEX

inFile& inFile::getLong( LONG& i )
{
   if ( type == STDIN )
      ok = scanf( "%lu", &i ) ;
   else if ( type == INPUT )
      ok = fscanf( (FILE*)handle(), "%lu", &i ) ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FREAD ) ;
   return *this ;
} // get LONG

void skipline( FILE *strm, int n )
{
   int ch ;
   do {
      do {
         ch = getc( strm ) ;
      } while ( ch != '\n' && ch != EOF ) ;
   } while ( --n && ch != EOF ) ;
} // skipline

void skipchar( FILE *strm, int n )
{
   int ch ;
   do {
      ch = getc( strm ) ;
   } while ( --n && ch != EOF ) ;
} // skipchar

inFile& inFile::nextLine( INDEX nl )
{
   if ( type == STDIN )
      skipline( stdin, nl ) ;
   else if ( type == INPUT )
      skipline( (FILE*)handle(), nl ) ;
   else
      fileError( FTYPE ) ;
   return *this ;
} // inFile::nextLine

inFile& inFile::operator() ( REAL& r )
{ return getReal( r ) ; }

inFile& inFile::operator() ( INDEX& i )
{ return getIndex( i ) ; }

inFile& inFile::operator() ( char& ch )
{ return getCh( ch ) ; }

inFile& inFile::operator() ( charArray& ca )
{ return getChar( ca ) ; }

static FILE* getMatrix( FILE *strm, matrix& x )
{
   INDEX ok, nr = x.nRows(), nc = x.nCols(), i, j ;
   int cw = matField(), pw = matPageWidth() ;
   int format = matFormat() ;
   int left = 1, right ; // left and right col in current pass
   int cp ; // col per page
   double d ;
   if ( format & ROWNUMS )
      cp = ( ( pw - 8 ) / ( cw + 2 ) ) - 1 ;
   else
      cp = ( pw / ( cw + 2 ) ) - 1 ;
   ok = TRUE ;
   if ( format & STACKED ) {
      while ( ok && left <= nc ) {
         skipline( strm, 1 ) ;
         right = ( left + cp <= nc ) ? left + cp : nc ;
         if ( format & (ROWNUMS|COLNUMS) )
            skipline( strm, 2 ) ;
         for ( i = 1; ok && i <= nr ; i++ ) {
            if ( format & ROWNUMS ) {
               skipchar( strm, 8 ) ;
            } // if
            for ( j = left; ok && j <= right ; j++ ) {
               ok = fscanf( strm, "%lf", &d ) ;
               if ( ok )
                  x(i,j) = d ;
            } // for j
            skipline( strm, 1 ) ;
         } // for i
         left = right + 1 ;
      } // while
   } else if ( format & COLMAJOR ) {
      for ( j = 1; ok && j <= nc; j++ ) {
         for ( i = 1; ok && i <= nr; i++ ) {
            ok = fscanf( strm, "%lf", &d ) ;
            if ( ok )
               x(i,j) = d ;
         } // for i
      } // for j
   } else { // ROWMAJOR
      for ( i = 1; ok && i <= nr; i++ ) {
         for ( j = 1; ok && j <= nc; j++ ) {
            ok = fscanf( strm, "%lf", &d ) ;
            if ( ok )
               x(i,j) = d ;
         } // for j
      } // for i
   } // else
   if ( !ok ) {
      x.errorij( i-1, j-1 ) ;
      x.errorExit( "istream >> matrix", NOFIL ) ;
   } // if
   return strm ;
} // getMatrix

inFile& matrix::get( inFile& f  )
{
   INDEX fType = f.fileType() ;
   if ( fType == STDIN )
      getMatrix( stdin, *this ) ;
   else if ( fType == INPUT )
      getMatrix( (FILE*)f.handle(), *this ) ;
   else
      fileError( FTYPE ) ;
   return f ;
} // get matrix

/********************************************************/
/*                  outFile Modules                     */
/********************************************************/

outFile& outFile::put( const char *label, INDEX w )
{
   if ( w == 0 )
      w = matField() ;
   if ( type == STDOUT )
      ok = printf( "%*s", w, label ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%*s", w, label ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%*s", w, label ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // outFile put label

outFile& outFile::putChar( const charArray& ca, INDEX width )
{ return put( ca.array(), width ) ; }

outFile& outFile::writeChar( const charArray& ca )
{ return write( ca.array() ) ; }

outFile& outFile::write( const char *label )
{
   if ( type == STDOUT )
      ok = printf( "%s", label ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%s", label ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%s", label ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // write label

outFile& outFile::putReal( const REAL r, INDEX w, INDEX prec )
{
   if ( w == 0 )
      w = matField() ;
   if ( prec == 0 )
      prec = matPrecision() ;
   if ( type == STDOUT )
      ok = printf( putFormat.array(), w, prec, r ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, putFormat.array(), w, prec, r ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), putFormat.array(), w, prec, r ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // outFile put REAL

outFile& outFile::putReal( const REAL r, const char *format )
{
   if ( type == STDOUT )
      ok = printf( format, r ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, format, r ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), format, r ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // outFile put REAL

outFile& outFile::writeReal( const REAL r, INDEX p )
{
   if ( p == 0 )
      p = matPrecision() ;
   if ( type == STDOUT )
      ok = printf( writeFormat.array(), p, r ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, writeFormat.array(), p, r ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), writeFormat.array(), p, r ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // outFile write REAL

outFile& outFile::putIndex( const INDEX i, INDEX w )
{
   if ( w == 0 )
      w = matField() ;
   if ( type == STDOUT )
      ok = printf( "%*u", w, i ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%*u", w, i ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%*u", w, i ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // putIndex

outFile& outFile::writeIndex( const INDEX i )
{
   if ( type == STDOUT )
      ok = printf( "%u", i ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%u", i ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%u", i ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // write INDEX

outFile& outFile::putLong( const LONG i, INDEX w )
{
   if ( w == 0 )
      w = matField() ;
   if ( type == STDOUT )
      ok = printf( "%*lu", w, i ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%*lu", w, i ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%*lu", w, i ) != EOF ;
   else
      fileError( FTYPE ) ;
   return *this ;
} // put LONG

outFile& outFile::writeLong( const LONG i )
{
   if ( type == STDOUT )
      ok = printf( "%lu", i ) != EOF ;
   else if ( type == STDERR )
      ok = fprintf( stderr, "%lu", i ) != EOF ;
   else if ( type == OUTPUT )
      ok = fprintf( (FILE*)handle(), "%lu", i ) != EOF ;
   else
      fileError( FTYPE ) ;
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // write LONG

outFile& outFile::newLine( INDEX nl )
{
   for ( INDEX i = 1; i <= nl ; i++ ) {
      if ( type == STDOUT )
         ok = putchar( '\n' ) != EOF ;
      else if ( type == STDERR )
         ok = fputc( '\n', stderr ) != EOF ;
      else if ( type == OUTPUT )
         ok = fputc( '\n', (FILE*)handle() ) != EOF ;
      else
         fileError( FTYPE ) ;
   } // for i
   if ( !ok )
      fileError( FWRITE ) ;
   return *this ;
} // outFile::newLine

int outFile::good( void )
{
   return ok ;
} // outFile::good

int outFile::fail( void )
{
   return !ok ;
} // outFile::fail

int outFile::open( const charArray& newName, int newType )
{
   if ( newType != STDOUT && newType != STDERR 
        && newType != OUTPUT )
      fileError( FTYPE ) ;
   return openFile( newName, newType ) ;
} // outFile::open

int outFile::close( void )
{
   return closeFile() ;
} // outFile::close

outFile& outFile::flush( void )
{
   if ( type == STDOUT )
      fflush( stdout ) ;
   else if ( type == STDERR )
      fflush( stderr ) ;
   else if ( type == OUTPUT )
      fflush( (FILE*) handle() ) ;
   else
      fileError( FTYPE ) ;
   return *this ;
} // outFile::flush

outFile::outFile( void ) : matFile()
{
   putFormat   = "%*.*g" ;
   writeFormat = "%.*g" ;
} // outFile( void )

outFile::outFile( const charArray& newName, int newType )
     : matFile()
{
   open( newName, newType ) ;
   putFormat = "%*.*g" ;
   writeFormat = "%.*g" ;
} // outFile

outFile::outFile( const outFile& f ) : matFile(f)
{
   putFormat = "%*.*g" ;
   writeFormat = "%.*g" ;
} // outFile

outFile::~outFile( void )
{} // ~outFile

static FILE* putMatrix( FILE* strm, const matrix& x,
                        const charArray& rFormat )
{
   INDEX ok, nr = x.nRows(), nc = x.nCols(), i, j, n = 0 ;
   int cw = matField(), pw = matPageWidth() ;
   int prec = matPrecision(), format = matFormat() ;
   int left = 1, right ; // left and right col in current pass
   int cp ; // col per page
   if ( format & ROWNUMS )
      cp = ( ( pw - 8 ) / ( cw + 2 ) ) - 1 ;
   else
      cp = ( pw / ( cw + 2 ) ) - 1 ;
   if ( format & STACKED ) {
      while ( left <= nc ) {
         fputc( '\n', strm ) ;
         right = ( left + cp <= nc ) ? left + cp : nc ;
         if ( format & COLNUMS ) {
            if ( format & ROWNUMS )
               fputs( " Row |  ", strm ) ;
            for ( j = left; j <= right; j++ )
               fprintf( strm, " Col: %*u   ", cw - 7, j ) ;
            fputc( '\n', strm ) ;
            if ( format & ROWNUMS )
               fputs( "------  ", strm ) ;
            for ( j = left; j <= right; j++ ) {
               for ( i = 1; i <= cw ; i++ )
                  fputc( '-', strm  ) ;
               fputs( "  ", strm ) ; ;
            } // for j
            fputc( '\n', strm  ) ;
         } else if ( format & ROWNUMS ) {
               fputs( " Row |  \n", strm ) ;
               fputs( "------  \n", strm ) ;
         } // else if
         for ( i = 1; i <= nr ; i++ ) {
            if ( format & ROWNUMS )
               fprintf( strm, "%4u |  ", i ) ;
            for ( j = left; j <= right ; j++ ) {
               ok = fprintf( strm, rFormat.array(), cw, prec, x(i,j) )
                    && fprintf( strm, "  " ) ;
               if ( !ok )
                  fileError( FWRITE ) ;
            } // for j
            fputc( '\n', strm ) ;
         } // for i
         left = right + 1 ;
      } // while
   } else {
      if ( format & COLMAJOR ) {
         for ( j = 1; j <= nc; j++ ) {
            for ( i = 1; i <= nr; i++ ) {
               ok = fprintf( strm, rFormat.array(), cw, prec, x(i,j) )
                    && fprintf( strm, "  " ) ;
               if ( !ok )
                  fileError( FWRITE ) ;
               if ( ++n >= cp ) {
                  fputc( '\n', strm ) ;
                  n = 0 ;
               } // if
            } // for i
         } // for j
      } else { // ROWMAJOR
         for ( i = 1; i <= nr; i++ ) {
            for ( j = 1; j <= nc; j++ ) {
               ok = fprintf( strm, rFormat.array(), cw, prec, x(i,j) )
                    && fprintf( strm, "  " ) ;
               if ( !ok )
                  fileError( FWRITE ) ;
               if ( ++n >= cp ) {
                  fputc( '\n', strm ) ;
                  n = 0 ;
               } // if
            } // for j
         } // for i
      } // else COLMAJOR
   } // else NOT STACKED
   return strm ;
} // putMatrix

charArray outFile::putForm( const charArray& newForm )
{
   charArray oldForm = putFormat ;
   putFormat = newForm ;
   return oldForm ;
} // outFile::putForm

charArray outFile::writeForm( const charArray& newForm )
{
   charArray oldForm = writeFormat ;
   writeFormat = newForm ;
   return oldForm ;
} // outFile::writeForm

outFile& matrix::put( outFile& f ) M_CONST
{
   INDEX fType = f.fileType() ;
   if ( fType == STDOUT )
      putMatrix( stdout, *this, f.putForm() ) ;
   else if ( fType == STDERR )
      putMatrix( stderr, *this, f.putForm() ) ;
   else if ( fType == OUTPUT )
      putMatrix( (FILE*)f.handle(), *this, f.putForm() ) ;
   else
      fileError( FTYPE ) ;
   return f ;
} // matrix::put

static FILE* getIndexArray( FILE *strm, indexArray& x)
{
   INDEX i, len = x.length();
   int format = matFormat() ;
   INDEX elem ;
   INDEX ok = TRUE ;
   if ( format & DISPLAY ) {
      skipline(strm,3);
      for ( i=1; ok && i <=len ; i++) {
         skipchar(strm,6);
         ok = fscanf( strm, "%u",  &elem ) ;
         if ( ok )
            x(i) = elem ;
         skipline(strm,1);
      } // for
      skipline(strm,1);
   } else {
      for ( i= 1;  ok && i <= len ; i++) {
         ok = fscanf( strm, "%u", elem ) ;
         if ( ok )
            x(i) = elem ;
      } //for i
   } // else
   if ( !ok) {
      x.errori(i-1);  // new function in matman.cc
      x.errorExit("istream >> indexArray ", NOFIL);
   } //if
   return strm;
} // getIndexArray

inFile& indexArray::get(inFile& f)
{
   INDEX fType = f.fileType() ;
   if ( fType == STDIN)
      getIndexArray( stdin, *this ) ;
   else if ( fType == INPUT)
      getIndexArray( (FILE*)f.handle(), *this ) ;
   else
      fileError(FTYPE);
   return f;
} // indexArray::get


outFile& outFile::operator << ( const REAL r )
{ return writeReal( r ) ; }

outFile& outFile::operator << ( const INDEX i )
{ return writeIndex( i ) ; }

outFile& outFile::operator << ( const char* s )
{ return write( s ) ; }

outFile& outFile::operator << ( const charArray& ca )
{ return write( ca.array() ) ; }

outFile& outFile::operator () ( const REAL r )
{ return putReal( r ) ; }

outFile& outFile::operator () ( const INDEX i, INDEX width )
{ return putIndex( i, width ) ; }

outFile& outFile::operator () ( const REAL r, INDEX width )
{ return putReal( r, width ) ; }

outFile& outFile::operator () ( const REAL r, INDEX width,
                                INDEX prec )
{ return putReal( r, width, prec ) ; }

outFile& outFile::operator () ( const REAL r, const char * format )
{ return putReal( r, format ) ; }

outFile& outFile::operator () ( const INDEX i )
{ return putIndex( i ) ; }

outFile& outFile::operator () ( const char* s )
{ return put( s ) ; }

outFile& outFile::operator () ( const char* s, INDEX width )
{ return put( s, width ) ; }

outFile& outFile::operator () ( const charArray& ca )
{ return put( ca.array() ) ; }

outFile& outFile::operator () ( const charArray& ca, INDEX width )
{ return put( ca.array(), width ) ; }


/************************************************************/
/*                     Error Methods                        */
/************************************************************/

outFile *matErrFile = &out ;

void errorFile( outFile& newErr )
{
   if ( newErr.fileType() != OUTPUT )
      fileError( FTYPE ) ;
   matErrFile = &newErr ;
   return ;
} // errorFile

static char *errorMessage[] = {
   "Unknown Error : Library fault!",
   "Not implemented",
   "Non-positive dimensions",
   "Non-equal dimensions",
   "Non-square matrix" ,
   "Bad Object List",
   "Memory Exhausted",
   "Index out of range",
   "Unassigned variable",
   "Excessive dimensions",
   "Call with Null Array or Pointer",
   "Null Object List",
   "Zero in Division",
   "Singular Matrix",
   "Not a vector",
   "Transcendatal Range Error",
   "File I/O Error",
   "Parameter out of bounds",
   "No Convergence",
   "Over loaded parameter",
   "Bad argument for function",
   "Non-positive definite matrix",
   "Multiple References to object"
} ; // errorMessage[]

void outFile::errorMess( char *func, char *mess )
{
   write("Error in ").write( func ).newLine() ;
   write( mess ).newLine() ;
   return ;
} // outFile::errorMess

void outFile::errorExit( char *func, char *mess )
{
   errorMess( func, mess ) ;
   exit(1) ;
} // outFile::errorExit

void outFile::errorMess( char *func, matError errorNum )
{
   write( "matObject error no. " ) ;
   writeIndex( (INDEX) errorNum ) ;
   newLine() ;
   errorMess( func, errorMessage[ errorNum ] ) ;
   return ;
} // outFile::errorMess

void outFile::errorExit( char *func, matError errorNum )
{
   errorMess( func, errorNum ) ;
   exit(1) ;
} // outFile::errorExit

void matErrNumExit( char *func, matError errorNum )
{
   matErrFile->errorMess( func, errorNum ) ;
   exit(1) ;
} // matErrNumExit errorNum

void matErrorExit( char *func, char *mess )
{
   matErrFile->errorMess( func, mess ) ;
   exit(1) ;
} // matErrorExit char*

void matErrNumMess( char *func, matError errorNum )
{
   matErrFile->errorMess( func, errorNum ) ;
} // matErrNumExit errorNum

void matErrorMess( char *func, char *mess )
{
   matErrFile->errorMess( func, mess ) ;
} // matErrorMess char*

void matObject::errori( INDEX i) M_CONST
{
   matErrFile->write( "Error in element [" ) ;
   matErrFile->writeIndex(i).write("]") ;
   info( *matErrFile ) ;
   return;
}  // matObject::errori

void matObject::errorij( INDEX i, INDEX j ) M_CONST
{
   matErrFile->write( "Error in element [" ).writeIndex( i ) ;
   matErrFile->write( "," ).writeIndex( j ) ;
   matErrFile->write( "] of following object." ).newLine() ;
   info( *matErrFile ) ;
   return ;
} // matObject::errorij

void matObject::error( char *func, matError errorNum ) M_CONST
{
   matErrNumMess( func, errorNum ) ;
   info( *matErrFile ) ;
   matFuncList() ;
} // matObject::error

void matObject::error( char *func, char *errorMess ) M_CONST
{
   matErrorMess( func, errorMess ) ;
   info( *matErrFile ) ;
   matFuncList() ;
} // matObject::error

#include <stdlib.h>

static void terminate( void )
{
   matObjectList( *matErrFile ) ;
   out.write( "\n\nMatClass Fatal Error.\n\n" ) ;
   exit(1) ;
} // terminate

void matObject::errorExit( char *func, matError num ) M_CONST
{
   error( func, num ) ;
   terminate() ;
} // matObject::errorExit

void matObject::errorExit( char *func, char *errorMess ) M_CONST
{
   error( func, errorMess ) ;
   terminate() ;
} // matObject::errorExit

void outFile::matVersion( void )
{
   write( "MatClass Version 1.0d"  ) ;
} // outFile matVersion

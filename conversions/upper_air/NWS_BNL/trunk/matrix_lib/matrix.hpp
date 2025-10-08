/**************************************************/
/*  matrix.hpp header for MatClass matrix class   */
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


#ifndef MATRIX_H

#define MATRIX_H

enum boolean
{
   FALSE, TRUE, OK
} ; // boolean

enum matStorevalues
{
   NOCLEAR = 0, AUTOCLEAR = 1, NEWSTORE = 2, AUTONEW = 3
} ; // matStorevalues

#if 0

typedef unsigned char BYTE ;
typedef unsigned int  INDEX ;
typedef unsigned long LONG ;
typedef double        REAL ;
typedef double        DOUBLE ;

#endif

#define BYTE   unsigned char
#define INDEX  unsigned int
#define LONG   unsigned long
#define REAL   double
#define DOUBLE double

enum matError
{
   NOERROR, NIMPL, NPDIM, NEDIM, NTSQR, BDLST, NOMEM,
   NRANG, UNASS, GTDIM, NULRF, NULST, ZEROD, SINGM,
   NOTVC, TRANS, NOFIL, NGPAR, NCONV, OVPAR, BDARG,
   NOTPD, MREFS
} ; // matError

enum comparison {
   COMPERR, GREATER, EQUAL, GREATEQUAL, LESS,
   CROSSED, LESSEQUAL, GTEQLT
} ; // comparison

enum truncation
{
   ROUND, FIX, FLOOR, CEIL
} ; // truncation

/* multiplication options */
enum transposition
{
   TRANS0, TRANS1, TRANS2, TRANS12
} ; // transposition

/* Sort, select and optima options */
enum summaries
{
   ABS = 1, DESCENDING = 2, ROWWISE = 4, ROWABS = 5,
   MINIMUM = 8, MINABS = 9, MINROW = 12, MINROWABS = 13,
   SUBTRACT = 16
} ; // summmaries

/**** matFile types *****/
enum matFiletype
{
   NULLFILE, STDIN, STDOUT, STDERR, INPUT, OUTPUT
} ; // matFiletype

/** matrix i/o opts **/
enum matformats
{
   PLAIN = 1, STACKED=2, COLNUMS=4, ROWNUMS=8, DISPLAY=14,
   COLMAJOR=16, SCIENTIFIC=32
} ; // matformats

/****************************************************/
/*        M_CONST to flag a const method for        */
/*               version 2 compilers                */
/****************************************************/

#ifdef __cplusplus
#define M_CONST  const
#else
#define M_CONST
#endif

/****************************************************/
/* Simple charArray Class :  made up of a pointer   */
/* to char array, array length and a set of basic   */
/* methods.                                         */
/****************************************************/

class charArray
{

      char  *cp ;    // char array pointer
      INDEX cpSize ; // size of allocated array

      void allocate( INDEX n ) ;
      void reallocate( INDEX n ) ;

   public :

      charArray( void ) ;
      charArray( INDEX n ) ;
      charArray( char *str ) ;
      charArray( const charArray& ca ) ;
      charArray& operator = ( const charArray& ca ) ;
      virtual ~charArray( void ) ;

      void clear( void ) ;
      char *array( void ) M_CONST { return cp ; }

      INDEX length( void ) M_CONST ; // string length

      INDEX size( void ) M_CONST { return cpSize ; }

      charArray& operator =( char *str ) ;

} ; // charArray

#ifndef IOBASIC_H

#define IOBASIC_H

class matEnvironment
{
      static DOUBLE  tolerance, epsilon ;
      static int     pageWidth, precision, fieldWidth, matForm ;

   public :

      matEnvironment( void ) ;
      ~matEnvironment( void ) {} ;

      friend DOUBLE matrixTol( const DOUBLE newTol = -1.0 ) ;
      friend DOUBLE matrixEps( const DOUBLE newEps = -1.0 ) ;

      friend int matFormat( int value = 0 ) ;
      friend int matPrecision( int value = 0 ) ;
      friend int matField( int value = 0 ) ;
      friend int matPageWidth( int value = 0 ) ;

} ; // class matEnvironment

/*****************************************************/
/*                                                   */
/*                 Basic File Type                   */
/*                                                   */
/*****************************************************/

struct fileHandle {
   void        *handle ;
   INDEX       count ;
   charArray   name ;
} ; // fileHandle

class matFile
{

   protected :

      fileHandle  *fh ;
      INDEX       type, ok ;

   public :

      friend void matFileInitial( void ) ;
      friend void fileError( INDEX errnum ) ;

      matFile( void ) ;
      matFile( const matFile& mf ) ;
      matFile& matFile::operator = ( const matFile& mf ) ;
      ~matFile( void ) ;

      void*      handle( void ) ;
      charArray  name( void ) ;

      INDEX  openFile( const charArray& newName, INDEX newType ) ; 
      INDEX  closeFile( void ) ;
      INDEX  eof( void ) ;

      INDEX  fileType( void ) { return type ; }

} ; // class matFile

/*****************************************************/
/*                 Basic output routines             */
/*****************************************************/

class outFile : public matFile
{
      charArray putFormat, writeFormat ;

   public :

      charArray putForm( const charArray& newForm ) ;
      charArray writeForm( const charArray& newForm ) ;
      charArray putForm( void ) { return putFormat ; }
      charArray writeForm( void ) { return writeFormat ; }

      friend outFile* outFilePtr( INDEX fhdl ) ;
      friend void errorFile( outFile& newErr ) ;
      friend void traceFile( outFile& newTrace ) ;

      int good( void ) ;
      int fail( void ) ;

      outFile( void ) ;
      // outFile( char* newName, int newType = OUTPUT ) ;
      outFile( const charArray& newName, int newType = OUTPUT ) ;
      outFile( const outFile& f ) ;
      ~outFile( void ) ;

      // int open( char* name, int newType = OUTPUT ) ;
      int open( const charArray& name, int newType = OUTPUT ) ;
      int close( void ) ;
      outFile& flush( void ) ;

      // outFile& putChar( char c ) ;
      outFile& putIndex( const INDEX i, INDEX width = 0 ) ;
      outFile& writeIndex( const INDEX i ) ;
      outFile& putLong( const LONG i, INDEX width = 0 ) ;
      outFile& writeLong( const LONG i ) ;
      outFile& putReal( const REAL r, INDEX width = 0,
                        INDEX prec = 0 ) ;
      outFile& putReal( const REAL r, const char *format ) ;
      outFile& writeReal( const REAL r, INDEX prec = 0 ) ;
      outFile& put( const char* label, INDEX width = 0 ) ;
      outFile& write( const char* label ) ;
      outFile& putChar( const charArray& ca, INDEX width = 0 ) ;
      outFile& writeChar( const charArray& ca ) ;

      outFile& operator << ( const REAL r ) ;
      outFile& operator << ( const INDEX i ) ;
      outFile& operator << ( const char* s ) ;
      outFile& operator << ( const charArray& ca ) ;
      outFile& operator () ( const REAL r ) ;
      outFile& operator () ( const INDEX i, INDEX width ) ;
      outFile& operator () ( const REAL r, INDEX width );
      outFile& operator () ( const REAL r, INDEX width, 
                             INDEX prec ) ;
      outFile& operator () ( const REAL r, const char *format ) ;
      outFile& operator () ( const INDEX i ) ;
      outFile& operator () ( const char* s ) ;
      outFile& operator () ( const char* s, INDEX width ) ;
      outFile& operator () ( const charArray& ca ) ;
      outFile& operator () ( const charArray& ca, INDEX width ) ;

      outFile& newLine( INDEX nl = 1 ) ;

      void errorMess( char *func, matError errorNum ) ;
      void errorExit( char *func, matError errorNum ) ;
      void errorMess( char *func, char *mess ) ;
      void errorExit( char *func, char *mess ) ;
      friend void matErrNumExit( char *fun, matError errorNum ) ;
      friend void matErrNumMess( char *fun, matError errorNum ) ;
      friend void matErrorExit( char *fun, char* mess ) ;
      friend void matErrorMess( char *fun, char* mess ) ;
      void matVersion( void ) ;

} ; // class outFile

extern outFile out, errout ;

class inFile : public matFile
{

      void create( char* newName, int newType ) ;

   public :

      int good( void ) ;
      int fail( void ) ;

      inFile( void ) ;
      // inFile( char* newName, int newType = INPUT ) ;
      inFile( const charArray& newName, int newType = INPUT ) ;
      inFile( const inFile& f ) ;
      ~inFile( void ) ;
      // int open( char* name, int newType = INPUT ) ;
      int open( const charArray& name, int newType = INPUT ) ;
      //   { return open( name.array(), newType ) ; }
      int close( void ) ;

      inFile& getReal( REAL& r ) ;
      inFile& getIndex( INDEX& i ) ;
      inFile& getLong( LONG& i ) ;
      inFile& getCh( char& ch ) ;
      inFile& get( char* label, int length ) ;
      inFile& getChar( charArray& ca )
         { return get( ca.array(), ca.size() ) ; }

      inFile& operator() ( REAL& r ) ;
      inFile& operator() ( INDEX& i ) ;
      inFile& operator() ( char& ch ) ;
      inFile& operator() ( charArray& ca ) ;

      inFile& operator >> ( REAL& r )
         { return getReal(r) ; }
      inFile& operator >> ( INDEX& i )
         { return getIndex(i) ; }
      inFile& operator >> ( charArray& ch )
         { return getChar(ch) ; }

      inFile& nextPage( INDEX nl = 1 ) ;
      inFile& nextLine( INDEX nl = 1 ) ;

} ; // class inFile

extern inFile in ;

#endif // ndef IOBASIC_H

/****************************************************/
/* matFunc class is used to maintain a stack        */
/* of function calls for error diagnostics          */
/****************************************************/

class matFunc // Maintain stack of function calls
{
     static INDEX count ;
     static INDEX debug ;

     charArray  name ;

   public :

      friend void matFuncInitial( void ) ;

      matFunc( char* func = 0 ) ;
      matFunc( const matFunc& f ) ;
      ~matFunc( void ) ;
      matFunc& operator = ( const matFunc& fn ) ;

      void info( outFile& f ) M_CONST ;
      int trace( INDEX lineFeed = 0 ) M_CONST ;
      friend void matFuncList( void ) ;
      friend INDEX setMatDebug( INDEX dbg ) ;

} ; // matFunc

/*******************************************************/
/* Simple Object : to interface with lists of objects  */
/* offer trace and error facilities to derived classes */
/*                                                     */
/* Object locals :                                     */
/*                                                     */
/* id     : id number                                  */
/*                                                     */
/* Statics :                                           */
/*                                                     */
/* count     : counts number of objects declared       */
/*             this is the basis of id system          */
/*                                                     */
/*******************************************************/

class matObject
{

      friend class matFunc ;

   protected :

      static int   listCtrl ;    // Object lists control
      static INDEX count ;       // count of objects

      INDEX        id ;       // identity number

   public :

      friend void matObjectInitial( void ) ;

      matObject( void )  ;
      virtual ~matObject( void ) ;
      matObject( const matObject& obj ) ;
      matObject& operator = ( const matObject& obj ) ;

      INDEX  identity( void ) M_CONST { return id ; }

      outFile& objectInfo( outFile& f ) M_CONST ;
      outFile& putName( const char* name, outFile& f ) M_CONST ;
      outFile& putField( INDEX i, outFile& f ) M_CONST ;
      outFile& putField( const char* label, outFile& f ) M_CONST ;
      void debugInfo( const matFunc& func ) M_CONST ;
      void errorij( INDEX i, INDEX j ) M_CONST ;
      void errori( INDEX i ) M_CONST ;
      void error( char *func, matError errorNum ) M_CONST ;
      void errorExit( char *func, matError errorNum ) M_CONST ;
      void error( char *func, char *mess ) M_CONST ;
      void errorExit( char *func, char *mess ) M_CONST ;

      virtual outFile& info( outFile& f ) M_CONST ;
      virtual outFile& put( outFile& f ) M_CONST ;
      virtual inFile& get( inFile& f ) ;
      virtual void clear( void ) ;

      friend outFile& operator << ( outFile& f, const matObject& obj )
         { return obj.put(f) ; }
      friend inFile& operator >> ( inFile& f, matObject& obj )
         { return obj.get(f) ; }

      friend int matListCtrl( int newCtrl = -1 ) ;
      friend void matObjectList( outFile& f = out, int ctrl = -1 ) ;

} ; // matObject

class matInitial
{
       static INDEX initiated ;

    public :

       matInitial( void ) ;
       ~matInitial( void ) ;

} ; // class matInitial

static matInitial matInitiator ;

/****************************************************/
/* Simple indexArray Class :  made up of a pointer  */
/* to INDEX array, array length and a set of basic  */
/* methods.                                         */
/****************************************************/

class indexArray : public matObject
{

      INDEX      *ip ;     // INDEX array pointer with unit offset
      INDEX      len ;    // length of array
      charArray  nm;   // name of indexArray

   public :

      indexArray( char* nameStr, INDEX n= 0 );
      indexArray( INDEX n = 0 ) ;
      indexArray( const indexArray& index ) ;
      virtual ~indexArray( void ) ;
      void operator = ( indexArray& y ) ;

      charArray name(char *newName = 0) ;
      INDEX length( void ) M_CONST { return len ; }
      indexArray& reset( INDEX n ) ;
      void clear( void ) ;
      INDEX& xelem( INDEX i ) M_CONST ;

      // debugging versions
      // INDEX& operator()( INDEX i ) M_CONST { return xelem(i) ; }
      // INDEX& elem( INDEX i ) M_CONST { return xelem(i) ; }

      INDEX& operator()( INDEX i ) M_CONST { return ip[i] ; }
      INDEX& elem( INDEX i ) M_CONST { return ip[i] ; }

      indexArray& assign( INDEX n, ... ) ;
      outFile& print( char* label = 0, outFile& f = out ) M_CONST ;
      indexArray& read( void ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      virtual outFile& put( outFile& f = out ) M_CONST ;
      virtual inFile&  get( inFile& f = in ) ;

#if 0
      friend outFile& operator << ( outFile& f, indexArray& x )
          { return x.put(f) ; }
      friend inFile& operator >> ( inFile& f, indexArray& x )
          { return x.get(f) ; }
#endif

} ; // indexArray

/****************************************************/
/* realArray class : is used as underlying storage  */
/* of reals primarily for matrices. Matrices access */
/* these arrays through a column map, see matMap    */
/* below. An array may be shared by several maps    */
/* An array has the following fields :-             */
/*    base   : pointers to the basic storage        */
/*    nrow   : number of rows in array              */
/*    ncol   : number of columns in array           */
/*    ref    : number of refs to array              */
/****************************************************/


class realArray : public matObject
{

      friend class matMap ;
      friend class refMatrix ;

      REAL   **base ;
      INDEX  nrow, ncol ;
      INDEX  ref ;

   public :

      realArray( void ) ;
      realArray( INDEX nr, INDEX nc ) ;
      realArray( const realArray& ra ) ;
      virtual ~realArray( void ) ;
      realArray& operator = ( realArray& y ) ;

      virtual outFile& info( outFile& f ) M_CONST ;
      virtual outFile& put( outFile& f ) M_CONST ;
      virtual inFile& get( inFile& f ) ;
      virtual void clear( void ) ;

      LONG size( void ) M_CONST
         { return (LONG) nrow * (LONG) ncol ; }
      REAL& elem( INDEX i, INDEX j ) M_CONST ;
      REAL& operator() ( INDEX i, INDEX j ) M_CONST
    { return elem(i,j) ; }

} ; // realArray

/********************************************************/
/*  matMap class : made up of a pointer to a realArray  */
/*  and a `map' to the matrix structure projected onto  */
/*  the array. A map may be shared by two or more       */
/*  matrices.  Fields are :-                            */
/*    pa      : pointer to array                        */
/*    map     : array of pointers to column tops        */
/*    mapSize : number of pointers in array             */
/*    ncol    : number of columns                       */
/*    nrow    : number of rows                          */
/*    ref     : number of references to the map         */
/*    nm      : name of matrix                          */
/********************************************************/

class matMap : public matObject
{

      friend class matrix ;
      friend class refMatrix ;

      realArray  *pa ;
      REAL       **map ;
      charArray  nm ;
      INDEX      mapSize, nrow, ncol, ref ;

   public :

      matMap( void ) ;
      matMap( INDEX nr, INDEX nc = 1 ) ;
      matMap( const matMap& m ) ;
      matMap( const matMap& m, INDEX r1, INDEX r2,
              INDEX c1, INDEX c2 ) ;
      matMap& operator = ( const matMap& m ) ;
      virtual ~matMap( void ) ;

      charArray& name( char* name ) ;
      void reset( INDEX nr, INDEX nc ) ;

      // inline methods
      INDEX nRows( void ) M_CONST
         { return nrow ; }
      INDEX nCols( void ) M_CONST
         { return ncol ; }
      realArray* array( void ) M_CONST
         { return pa ; }
      REAL* base( INDEX j ) M_CONST
         { return pa->base[j] ; }
      REAL& operator ()  ( INDEX i ) M_CONST
         { return map[1][i] ; }
      REAL& operator () ( INDEX i, INDEX j ) M_CONST
         { return map[j][i] ; }

      REAL& elem ( INDEX i, INDEX j ) M_CONST ;

      outFile& info( outFile& f ) M_CONST ;
      outFile& put( outFile& f ) M_CONST ;
      inFile& get( inFile& f ) ;
      void clear( void ) ;

} ; // matMap

/********************************************************/
/*  matPair class : made up a pair of indices and their */
/*  bounds.                                             */
/*  Fields are :-                                       */
/*    i          : row index                            */
/*    j          : column index                         */
/*    r1         : lower row bound                      */
/*    r2         : upper row bound                      */
/*    c1         : lower column bound                   */
/*    c2         : upper column bound                   */
/*    inBounds   : true if i & j in bounds              */
/********************************************************/

class matPair
{
      friend class matrix ;

      INDEX i, j, r1, r2, c1, c2, inBounds ;

   public :

      matPair( void ) ;
      matPair( INDEX nr, INDEX nc ) ;
      matPair( INDEX nr1, INDEX nr2, INDEX nc1, INDEX nc2 ) ;
      matPair( matPair& p ) ;
      ~matPair( void ) ;
      matPair& operator = ( matPair& p ) ;

      INDEX row( void ) { return i ; }
      INDEX col( void ) { return j ; }
      INDEX ok( void ) { return inBounds ; }
      void first( void ) ;
      void last( void ) ;
      void operator ++ ( void ) ;
      void operator -- ( void ) ;
      void range( INDEX nr1, INDEX nr2, INDEX nc1, INDEX nc2 ) ;

} ; // class matPair

/****************************************************/
/* Matrix Class :  made up of a pointer to a matMap */
/* and a set of "methods".   The matrix structure   */
/* uses fields as follows :-                        */
/*                                                  */
/*    pm     : pointer to a matMap                  */
/*                                                  */
/* Two matrices may give access to the same array   */
/* but have very different maps e.g. a sub-matrix   */
/* uses its map to point to a subset of the         */
/* parent's array.                                  */
/****************************************************/

class matrix : public matObject
{
      friend class refMatrix ;

      enum matrixType {
           STANDARD, TRANSPOSED
      } ;

      INDEX     type ;
      matMap    *pm ; // pointer to underlying matMap

   public :

      matrix( void ) ;
      matrix( INDEX nr, INDEX nc = 1 ) ;
      matrix( char* nameStr, INDEX nr, INDEX nc = 1 ) ;
      virtual ~matrix( void ) ;
      matrix( const matrix &y ) ;
      matrix& operator = ( const matrix& y ) ;

      matrix( indexArray& index ) ;

      charArray name( char *newName = 0 ) ;

      virtual void reset( INDEX nr, INDEX nc ) ;
      virtual void reset( INDEX nr ) ;
      virtual void clear( void ) ;

      virtual INDEX isNull( void ) M_CONST ;
      INDEX isTrans( void ) M_CONST ;

      INDEX nRefs( void ) M_CONST ;
      INDEX nCols( void ) M_CONST ;
      INDEX nRows( void ) M_CONST ;
      LONG  size( void ) M_CONST ;


      // use the following for debugging
      // off-line index method with x-checking
      REAL &xmat( INDEX i, INDEX j = 1 ) M_CONST ;

      // debugging versions of basic method of indexing elements
      /*********************************************************
      REAL& mat( INDEX i, INDEX j = 1 ) M_CONST
          { return xmat(i,j) ; }
      REAL& operator() ( INDEX i, INDEX j ) M_CONST
               { return xmat(i,j) ; }
      REAL& operator() ( INDEX i ) M_CONST
               { return xmat(i,1) ; }
      REAL& tran( INDEX i, INDEX j )
          { return xmat(j,i) ; }
      *********************************************************/

      // basic method of indexing elements
      REAL& mat( INDEX i, INDEX j = 1 ) M_CONST
          { return pm->map[j][i] ; }
      REAL& operator() ( INDEX i, INDEX j ) M_CONST
          { return pm->map[j][i] ; }
      REAL& operator() ( INDEX i ) M_CONST
          { return pm->map[1][i] ; }
      REAL& tran( INDEX i, INDEX j )
          { return pm->map[i][j] ; }
      REAL* operator[] ( INDEX i )
          { return pm->map[i] ; }
      REAL& rmat( REAL r, REAL c = 1.0 ) M_CONST
               { return pm->map[ (INDEX) c ] [ (INDEX) r ] ; }
      REAL& vec( LONG k ) M_CONST ;
      REAL& operator() ( matPair p )
               { return pm->map[p.j][p.i] ; }

      void checkDims( const matrix& y, char *mName ) M_CONST ;

      // ************* matrix I/O methods ************

      outFile& print( char* label = 0, outFile& = out ) M_CONST ;
      matrix& read( void ) ;

      virtual outFile& info( outFile& f = out ) M_CONST ;
      outFile& put( outFile& f = out ) M_CONST ;
      outFile& display( outFile& f = out ) M_CONST ;

      inFile& get( inFile& f = in ) ;

#if 0
      friend outFile& operator << ( outFile& f, const matrix& x )
               { return x.put(f) ; }
      friend inFile& operator >> ( inFile& f, matrix& x )
               { return x.get(f) ; }
#endif

      matrix& assign( INDEX nr, INDEX nc, ... ) ;

      // joins and stacks
      friend matrix& joinOf( matrix& z, const matrix& x, const matrix& y ) ;
      friend matrix& stackOf( matrix& z, const matrix& x, const matrix& y ) ;
      friend matrix operator | ( const matrix& x, const matrix &y ) ;
      friend matrix operator || ( const matrix& x, const matrix &y ) ;

      // special constructors
      friend matrix eye( INDEX nr, INDEX nc ) ;
      friend matrix zeros( INDEX nr, INDEX nc ) ;
      friend matrix ones( INDEX nr, INDEX nc ) ;

      matrix& minus( void ) ;
      matrix& step( REAL first = 1.0, REAL increment = 1.0 ) ;

      matrix operator ! ( void ) M_CONST ;
      matrix trans( void ) M_CONST ; // transpose

      // set submatrix to y
      void setSub( INDEX r1, INDEX r2, INDEX c1, INDEX c2,
                   const matrix& y ) ;
      void setRow( INDEX i, const matrix& y ) ;
      void setCol( INDEX j, const matrix& y ) ;


      // reference sub matrix methods
      void capture( matrix& x ) ; // grab x's map
      void refer( const matrix& x ) ; // share x's map

      matrix sub( INDEX r1, INDEX r2, INDEX c1, INDEX c2 ) M_CONST ;
      matrix row( INDEX r1, INDEX r2 = 0 ) M_CONST ;
      matrix smpl( INDEX s1, INDEX s2 ) M_CONST { return row( s1, s2 ) ; }
      matrix col( INDEX c1, INDEX c2 = 0 ) M_CONST ;

      // non-reference sub matrix methods
      void setDiag( matrix& dg, int k = 0 ) ;
      void setDiag( REAL r, int k = 0 ) ;

      matrix& diagOf( const matrix& x, int k = 0 ) ;
      matrix  diag( int k = 0 ) M_CONST ;
      matrix& triuOf( const matrix& x, int k = 0 ) ;
      matrix& trilOf( const matrix& x, int k = 0 ) ;
      matrix& subOf( const matrix& x, INDEX r1, INDEX r2,
                     INDEX c1, INDEX c2 ) ;
      matrix& rowOf( const matrix& x, INDEX i, INDEX j = 0 )  ;
      matrix& colOf( const matrix& x, INDEX i, INDEX j = 0 )  ;


      // sorting methods
      void heapSort( void ) ;
      void shellSort( void ) ;
      void heapMap( indexArray& map ) M_CONST ;
      void shellMap( indexArray& map ) M_CONST ;

      indexArray heapMap( void ) M_CONST ;
      indexArray shellMap( void ) M_CONST ;
      friend matrix rankings( indexArray& map ) ;

      // select and optima methods
      matrix& rowSelectOf( const matrix& x,
                           const indexArray& cindex ) ;
      matrix& colSelectOf( const matrix& x,
                           const indexArray& rindex ) ;
      matrix& subOf( const matrix& x, const indexArray& rmap,
                     const indexArray& cmap ) ;
      matrix& rowsOf( const matrix& x, const indexArray& map ) ;
      matrix& colsOf( const matrix& x, const indexArray& map ) ;

      INDEX colOpti( INDEX j, int opt ) M_CONST ;
      INDEX rowOpti( INDEX i, int opt ) M_CONST ;
      matrix& rowOptOf( const matrix& x, int option = 0 ) ;
      matrix& colOptOf( const matrix& x, int option = 0 ) ;

      indexArray optimaMap( indexArray& map,
                            int option = 0 ) M_CONST ;
      indexArray optimaMap( int option ) M_CONST ;

      matrix rowMax( int opt = 0 ) M_CONST ;
      matrix rowMin( int opt = 0 ) M_CONST ;
      matrix colMax( int opt = 0 ) M_CONST ;
      matrix colMin( int opt = 0 ) M_CONST ;

      indexArray rowMaxMap( int opt = 0 ) M_CONST ;
      indexArray rowMinMap( int opt = 0 ) M_CONST ;
      indexArray colMaxMap( int opt = 0 ) M_CONST ;
      indexArray colMinMap( int opt = 0 ) M_CONST ;

      // summary methods
      matrix& rowSumOf( const matrix& x, int opt = 0 ) ;
      matrix& colSumOf( const matrix& x, int opt = 0 ) ;
      matrix& colMeanOf( const matrix& x, int opt = 0 ) ;
      matrix& rowMeanOf( const matrix& x, int opt = 0 ) ;
      matrix  rowSum( int opt = 0 ) M_CONST ;
      matrix  colSum( int opt = 0 ) M_CONST ;
      matrix  rowMean( int opt = 0 ) M_CONST ;
      matrix  colMean( int opt = 0 ) M_CONST ;
      matrix& colSqrOf( const matrix& x ) ;
      matrix& rowSqrOf( const matrix& x ) ;
      matrix& colSqDevOf( const matrix& x, const matrix& avg ) ;
      matrix& rowSqDevOf( const matrix& x, const matrix& avg ) ;

      // shift methods
      void colPlus( const matrix& m ) ;
      void colMinus( const matrix& m ) ;
      void rowPlus( const matrix& m ) ;
      void rowMinus( const matrix& m ) ;

      REAL trace( void ) M_CONST ;
      REAL sum( void ) M_CONST ;
      REAL aver( void ) M_CONST ;
      REAL sumsq( void ) M_CONST ;
      REAL norm1( void ) M_CONST ;
      REAL normi( void ) M_CONST ;
      REAL normf( void ) M_CONST ;

      // comparisons
      matrix and( const matrix& y ) M_CONST ;
      matrix or( const matrix& y ) M_CONST ;
      friend matrix not( const matrix& y ) ;

      friend matrix operator == ( const matrix& x,
                                  const matrix& y ) ;
      friend matrix operator >  ( const matrix& x,
                                  const matrix& y ) ;
      friend matrix operator >= ( const matrix& x,
                                  const matrix& y ) ;
      friend matrix operator <  ( const matrix& x,
                                  const matrix& y ) ;
      friend matrix operator <= ( const matrix& x,
                                  const matrix& y ) ;
      friend matrix operator != ( const matrix& x,
                                  const matrix& y )  ;
      friend INDEX any( const matrix& x ) ;
      friend INDEX all( const matrix& x ) ;
      comparison compare( const matrix& y ) M_CONST ;

      friend matrix operator == ( const matrix& x, REAL r ) ;
      friend matrix operator >  ( const matrix& x, REAL r ) ;
      friend matrix operator >= ( const matrix& x, REAL r ) ;
      friend matrix operator <  ( const matrix& x, REAL r ) ;
      friend matrix operator <= ( const matrix& x, REAL r ) ;
      friend matrix operator != ( const matrix& x, REAL r ) ;
      comparison compare( const REAL r ) M_CONST ;

      INDEX countTrue( void ) M_CONST ;
      indexArray mapTrue( void ) M_CONST ;

      // check used by binary operators
      friend void binaryChecks( char *name, const matrix& x,
                                const matrix& y ) ;

      // basic linear operations
      matrix& linear( REAL a, const matrix& x, const matrix& y ) ;
      matrix& linear( REAL a, const matrix& x, REAL b ) ;

      // binary operators
      matrix& operator += ( const matrix& y ) ;
      matrix& operator -= ( const matrix& y ) ;

      friend matrix operator - ( const matrix& x ) ;
      friend matrix operator + ( const matrix& x, const matrix& y ) ;
      friend matrix operator - ( const matrix& x, const matrix& y ) ;

      matrix& operator  = ( REAL r ) ;
      matrix& operator += ( REAL r ) ;
      matrix& operator -= ( REAL r ) ;
      matrix& operator *= ( REAL r ) ;
      matrix& operator /= ( REAL r ) ;

      friend matrix operator + ( const matrix& x, REAL r ) ;
      friend matrix operator - ( const matrix& x, REAL r ) ;
      friend matrix operator * ( const matrix& x, REAL r ) ;
      friend matrix operator / ( const matrix& x, REAL r ) ;
      friend matrix operator + ( REAL r, const matrix& y ) ;
      friend matrix operator - ( REAL r, const matrix& y ) ;
      friend matrix operator * ( REAL r, const matrix& y ) ;
      friend matrix operator / ( REAL r, const matrix& y ) ;

      // "projection" operator and inverse based on LU
      friend matrix operator / ( const matrix& b, const matrix& x ) ;
      matrix inv( void ) ;

      // multiplication operations
      friend matrix operator * ( const matrix& x, const matrix &y ) ;
      matrix& multOf( const matrix& x, const matrix& y ) ;
      matrix& multTOf( const matrix& x, const matrix& y ) ;
      matrix& TMultOf( const matrix& x, const matrix& y ) ;
      matrix& TMultTOf( const matrix& x, const matrix& y ) ;
      matrix  TMult( const matrix& y ) M_CONST ;
      matrix  multT( const matrix& y ) M_CONST ;
      matrix  TMultT( const matrix& y ) M_CONST ;

      // element by element operators
      matrix& multijEq( const matrix& y ) ;
      matrix& divijEq( const matrix& y ) ;
      matrix  multij( const matrix& y ) M_CONST ;
      matrix  divij( const matrix& y ) M_CONST ;

      // kronecker product
      matrix& kronOf( const matrix& x, const matrix& y,
                      transposition opt = TRANS0 ) ;
      matrix  kron( const matrix& y ) M_CONST ;
      matrix  TKron( const matrix& y ) M_CONST ;
      matrix  kronT( const matrix& y ) M_CONST ;
      matrix  TKronT( const matrix& y ) M_CONST ;
      matrix  operator % ( const matrix& y ) M_CONST ;

      // indexArray assignment
      void operator = ( indexArray& array ) ;
      indexArray& toIndex( indexArray& index ) ;

      // inner product
      REAL inner( const matrix &y ) M_CONST ;

      // scaling or diagonal products
      matrix& rowMultOf( const matrix& x, const matrix& dg ) ;
      matrix& colMultOf( const matrix& x, const matrix& dg ) ;
      matrix  rowMult( const matrix& dg ) M_CONST ;
      matrix  colMult( const matrix& dg ) M_CONST ;

      // truncation methods
      matrix& roundOf( const matrix& x, truncation option = ROUND ) ;

      matrix& signOf( const matrix& x ) ;

      matrix& remOf( const matrix& x, const matrix& y ) ;
      matrix  rem( const matrix& y ) M_CONST ;

} ; // class matrix


class refMatrix : public matrix
{
   matMap *ppm ; // pointer to parent map

   public :

      refMatrix( void ) ;
      refMatrix( const matrix& m ) ;
      refMatrix( const refMatrix& m ) ;
      refMatrix& operator = ( const refMatrix& m ) ;
      virtual matrix& operator = ( const matrix& m ) ;
      virtual ~refMatrix( void ) ;

      virtual INDEX isNull( void ) M_CONST ;
      virtual void reset( INDEX nr, INDEX nc ) ;
      virtual void reset( INDEX nr ) ;
      virtual void clear( void ) ;
      matrix& refRow( INDEX r1, INDEX r2 = 0 ) ;
      matrix& refCol( INDEX c1, INDEX c2 = 0 ) ;
      matrix& refSub( INDEX r1, INDEX r2, INDEX c1, INDEX c2 ) ;
      virtual outFile& info( outFile& f ) M_CONST ;
} ; // class refMatrix

#endif // ndef MATRIX_H

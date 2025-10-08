/**************************************************/
/*         matpsf source for psFile class         */
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

#include "matpsf.hpp"

#include <math.h>

psFile::psFile( char *name ) : outFile( name )
{
   unit = 72 ; // = 1 inch
   width = 8 ; prec = 5 ;
   pi_4 = atan(1.0) ;
   stdWin.x1 = 0 ;          stdWin.y1 = 0 ;
   stdWin.x2 = 8.5 * unit ; stdWin.y2 = 11 * unit ; // inches
   pWin = stdWin ;
   stdWorld.x1 = 0.0 ; stdWorld.y1 = 0.0 ;
   stdWorld.x2 = 1.0 ; stdWorld.y2 = 1.0 ;
   pWorld = stdWorld ;
   write( "%%Creator MatClass matpsf class\n" ) ;
   moveTo(0,0) ;
} // psFile( name ) ;

psFile::psFile( char *name, psWindow& win ) : outFile( name )
{
   unit = 72 ; // = 1 inch
   width = 8 ; prec = 5 ;
   pi_4 = atan(1.0) ;
   stdWin.x1 = win.x1 ;   stdWin.y1 = win.y1 ;
   stdWin.x2 = win.x2 ;   stdWin.y2 = win.y2 ; // inches
   pWin = win ;
   stdWorld.x1 = 0.0 ; stdWorld.y1 = 0.0 ;
   stdWorld.x2 = 1.0 ; stdWorld.y2 = 1.0 ;
   pWorld = stdWorld ;
   write( "%!PS-Adobe-2.0 EPSF-1.2\n" );
   write( "%%BoundingBox: " ) ;
   units( win.x1 ).units( win.y1 ) ;
   units( win.x2 ).units( win.y2 ) ;
   newLine() ;
   write( "%%Creator MatClass matpsf class\n" ) ;
   moveTo(0,0) ;
} // psFile( name ) ;

psFile::psFile( psFile& f ) : outFile( f )
{
   unit = f.unit ;
   width = f.width ;
   prec = f.prec ;
   pi_4 = f.pi_4 ;
   pWin = f.pWin ;
   pWorld = f.pWorld ;
} // psFile( f ) ;

psFile& psFile::operator = ( psFile& f )
{
   // outFile::operator = ( f ) ; // ?????
   unit = f.unit ;
   width = f.width ;
   prec = f.prec ;
   pi_4 = f.pi_4 ;
   pWin = f.pWin ;
   pWorld = f.pWorld ;
   return f ;
} // psFile = ( f ) ;

psFile::~psFile( void ) {} // ~psFile

psFile& psFile::comment( const char *str )
{
   write( "\n% psFile : " ).write( str ).write( "\n" ) ;
   return *this ;
} // psFile comment

psFile& psFile::setUnit( REAL newUnit )
{
   unit = newUnit ;
   return *this ;
} // psFile setUnit

psFile& psFile::units( REAL r )
{
   putReal( r * unit, width, prec ) ;
   return *this ;
} // psFile writeUnit

psFile& psFile::points( REAL pt )
{
   putReal( pt, width, prec ) ;
   return *this ;
} // psFile points

psFile& psFile::setScales( void )
{
   if ( ( pWorld.x2 == pWorld.x1 ) || ( pWorld.y2 == pWorld.y1 ) )
      errorExit( "setScales", "Zero scale" ) ;
   xScale = ( pWin.x2 - pWin.x1 ) / ( pWorld.x2 - pWorld.x1 ) ;
   yScale = ( pWin.y2 - pWin.y1 ) / ( pWorld.y2 - pWorld.y1 ) ;
   return *this ;
} // psFile setScales

psFile& psFile::setWindow( psWindow& win, INDEX option )
{
   if ( win.x2 <= win.x1 || win.y2 <= win.y1 )
      matErrorExit( "setWindow", "Null window" ) ;
   pWin = win ;
   setScales() ;
   if ( option ) {
      gSave() ;
      newPath() ;
      moveTo( pWin.x1, pWin.y1 ) ;
      lineTo( pWin.x1, pWin.y2 ) ;
      lineTo( pWin.x2, pWin.y2 ) ;
      lineTo( pWin.x2, pWin.y1 ) ;
      closePath() ;
      if ( option & psFILL ) {
         gSave() ;
         fill() ;
         gRestore() ;
      } // if
      if ( option & psBORDER )
         stroke() ;
      gRestore() ;
   } // if
   moveTo(0,0) ;
   // translate( win.x1, win.y1 ) ;
   return *this ;
} // psFile::setWindow

psFile& psFile::setWorld( psWorld& world )
{
   if ( world.x2 <= world.x1 || world.y2 <= world.y1 )
      matErrorExit( "setWorld", "Null world" ) ;
   pWorld = world ;
   setScales() ;
   return *this ;
} // psFile::setWorld

psFile& psFile::gSave( void )
{
   write( "gsave\n" ) ;
   return *this ;
} // psFile gSave

psFile& psFile::gRestore( void )
{
   write( "grestore\n" ) ;
   return *this ;
} // psFile gRestore

psFile& psFile::newPath( void )
{
   write( "newpath\n" ) ;
   return *this ;
} // psFile newPath

psFile& psFile::showPage( void )
{
   write( "showpage\n" ) ;
   return *this ;
} // psFile showPage

psFile& psFile::coords( REAL x, REAL y )
{
   return units( x ).units( y ) ;
} // psFile coords

psFile& psFile::translate( REAL x, REAL y )
{
   coords( x, y ) ;
   write( "  translate\n" ) ;
   return *this ;
} // psFile translate

psFile& psFile::rotate( REAL theta )
{
   points( theta )( " rotate\n" ) ;
   return *this ;
} // psFile rotate

psFile& psFile::moveTo( REAL x, REAL y )
{
   coords( x, y ) ;
   write( "  moveto\n" ) ;
   return *this ;
} // psFile moveTo

psFile& psFile::rMoveTo( REAL a, REAL b )
{
   coords( a, b ) ;
   write( "  rmoveto\n" ) ;
   return *this ;
} // psFile rMoveTo

psFile& psFile::lineTo( REAL x, REAL y )
{
   coords( x, y ) ;
   write( "  lineto\n" ) ;
   return *this ;
} // psFile lineTo

psFile& psFile::rLineTo( REAL a, REAL b )
{
   coords( a, b ).write( "  rlineto\n" ) ;
   return *this ;
} // psFile rLineTo

psFile& psFile::arc( REAL x, REAL y, REAL radius, REAL ang1, REAL ang2 )
{
   coords( x, y ).units( radius ) ;
   points( ang1 ).points( ang2 ) ;
   write( "  arc\n" ) ;
   return *this ;
} // psFile arc

psFile& psFile::findFont( char *font )
{
   write( "/" ).write( font ).write( " findfont\n " ) ;
   return *this ;
} // psFile findFont

psFile& psFile::scaleFont( REAL size )
{
   points( size ).write( "  scalefont\n" ) ;
   return *this ;
} // psFile scaleFont

psFile& psFile::setFont( void )
{
   write( "setfont\n" ) ;
   return *this ;
} // setFont

psFile& psFile::show( const charArray& str, REAL xFact, REAL yFact )
{
   if ( xFact != 0.0 || yFact != 0.0 )
      align( str, xFact, yFact ) ;
   write( "(" ).write( str.array() ).write( ")  show\n" ) ;
   return *this ;
} // psFile show

psFile& psFile::charPath( const charArray& str )
{
   write( "(" ).write( str.array() ) ;
   write( ") true charpath\n" ) ;
   return *this ;
} // psFile charPath

psFile& psFile::charBlank( const charArray& str )
{
   comment( "Blank background" ) ;
   write( "gsave newpath\n" ) ;
   write( "(" ).write( str.array() ).write( ") true charpath flattenpath\n" ) ;
   setGray( 1.0 ) ;
   write( "fill\n" ) ;
   write( "grestore\n" ) ;
   return *this ;
} // psFile charPath

psFile& psFile::align( const charArray& str, REAL xFact, REAL yFact )
{
   comment( "Setting alignment" ) ;
   write( "gsave newpath 0 0 moveto\n" ) ;
   write( "(" ).write( str.array() ).write( ") true charpath flattenpath\n" ) ;
   write( "pathbbox /psAligny1 exch def /psAlignx1 exch def pop pop\n" ) ;
   write( "grestore\n" ) ;
   write( "/psAlignx1 psAlignx1 " ) ; points( xFact ) ; write( " mul def\n" ) ;
   write( "/psAligny1 psAligny1 " ) ; points( yFact ) ; write( " mul def\n" ) ;
   write( "psAlignx1 psAligny1 rmoveto\n" ) ;
   return *this ;
} // psFile align

psFile& psFile::align( void )
{
   write( "psAlignx1 psAligny1 rmoveto\n" ) ;
   return *this ;
} // psFile align

psFile& psFile::curveTo( REAL x1, REAL y1, REAL x2, REAL y2,
                      REAL x3, REAL y3 )
{
   coords( x1, y1 ).coords( x2, y2 ) ;
   coords( x3, y3 ).write( "  curveto\n" ) ;
   return *this ;
} // psFile curveTo

psFile& psFile::closePath( void )
{
   write( "closepath\n" ) ;
   return *this ;
} // psFile closePath

psFile& psFile::stroke( void )
{
   write( "stroke\n" ) ;
   return *this ;
} // psFile stroke

psFile& psFile::fill( void )
{
   write( "fill\n" ) ;
   return *this ;
} // psFile fill

psFile& psFile::scale( REAL xScale, REAL yScale )
{
   points( xScale ).points( yScale ) ;
   write( "  scale\n" ) ;
   return *this ;
} // psFile scale

psFile& psFile::setLineWidth( REAL lineWidth )
{
   points( lineWidth ) ;
   write( "  setlinewidth\n" ) ;
   return *this ;
} // psFile setLineWidth

psFile& psFile::setGray( REAL gray )
{
   points( gray )( "  setgray\n" ) ;
   return *this ;
} // psFile setGray

psFile& psFile::setDash( const charArray& str )
{
   writeChar( str ).write( " setdash\n" ) ;
   return *this ;
} // psFile setGray

psFile& psFile::setWorld( psWorld& world, matrix& x, matrix& y )
{
   world.x1 = x.colMin()(1) ;
   world.x2 = x.colMax()(1) ;
   world.y1 = y.colMin()(1) ;
   world.y2 = y.colMax()(1) ;
   if ( world.x2 <= world.x1 || world.y2 <= world.y1 )
      matErrorExit( "setWorld", "Null world" ) ;
   pWorld = world ;
   return setScales() ;
} // psFile::setWorld

psFile& psFile::wUnits( REAL r, INDEX option )
{
   if ( option )
      return units( r * yScale  ) ;
   else
      return units( r * xScale  ) ;
} // psFile wUnits

psFile& psFile::wCoords( REAL x, REAL y )
{
   REAL xc, yc ;
   xc = ( x - pWorld.x1 ) * xScale + pWin.x1 ;
   yc = ( y - pWorld.y1 ) * yScale + pWin.y1 ;
   return coords( xc, yc ) ;
} // psFile wCoords

psFile& psFile::wMoveTo( REAL x, REAL y )
{
   wCoords( x, y ).write( "  moveto\n" ) ;
   return *this ;
} // psFile wMoveTo

psFile& psFile::wTranslate( REAL x, REAL y )
{
   wCoords( x, y ).write( "  translate\n" ) ;
   return *this ;
} // psFile wTranslate

psFile& psFile::wRotate( REAL theta )
{
   REAL alpha = pi_4 * theta / 45 ;
   alpha = 45 * atan( ( yScale / xScale ) * tan( alpha ) ) / pi_4 ;
   return rotate( alpha ) ;
} // psFile wRotate

psFile& psFile::wLineTo( REAL x, REAL y )
{
   wCoords( x, y ).write( "  lineto\n" ) ;
   return *this ;
} // psFile wLineTo

psFile& psFile::axes( INDEX option )
{
   comment( "drawing axes" ) ;
   if ( !option && pWorld.x1 <= 0 && pWorld.x2 >= 0 &&
        pWorld.y1 <= 0 && pWorld.y2 >= 0
   ) {
        gSave() ;
        newPath() ;
           wMoveTo( pWorld.x1, 0 ) ;
           wLineTo( pWorld.x2, 0 ) ;
        stroke() ;
        newPath() ;
           wMoveTo( 0, pWorld.y1 ) ;
           wLineTo( 0, pWorld.y2 ) ;
        stroke() ;
        gRestore() ;
   } else {
        gSave() ;
        newPath() ;
           wMoveTo( pWorld.x2, pWorld.y1 ) ;
           wLineTo( pWorld.x1, pWorld.y1 ) ;
           wLineTo( pWorld.x1, pWorld.y2 ) ;
        stroke() ;
        gRestore() ;
   } // else
   return *this ;
} // psFile axes

psFile&  psFile::labelAxes( const charArray& xLabel, const charArray& yLabel,
                            INDEX option )
{
   if ( !option && pWorld.x1 <= 0 && pWorld.x2 >= 0 &&
        pWorld.y1 <= 0 && pWorld.y2 >= 0
   ) {
        wMoveTo( 0.95 * pWorld.x2, 0 ) ;
        show( xLabel, -1.0, -2.0 ) ;
        wMoveTo( 0.05 * pWorld.x1, pWorld.y2 ) ;
        show( yLabel, -1.0, -2.0 ) ;
   } else {
        wMoveTo( 0.95 * pWorld.x2, pWorld.y1 ) ;
        show( xLabel, -1.0, 0.0 ) ;
        wMoveTo( pWorld.x1, pWorld.y2 ) ;
        show( yLabel, 0.0, -2.0 ) ;
   } // else
   return *this ;
} // psFile labelAxes

psFile&  psFile::axes( const charArray& xLabel, const charArray& yLabel,
                       INDEX option )
{
   axes( option ) ;
   labelAxes( xLabel, yLabel, option ) ;
   return *this ;
} // psFile axes


psFile& psFile::bezier( matrix& x, matrix& y, matrix& xBez, matrix& yBez )
{
   //   Calculate Intermediate Points for
   //   Bezier Curves
   static char *mName = "psFile::bezier" ;
   if ( x.nRows() != y.nRows() )
      x.errorExit( mName, NEDIM ) ;
   if ( x.nCols() != 1 || y.nCols() != 1 )
      x.errorExit( mName, NOTVC ) ;
   INDEX i, n = x.nRows() ;
   // ?? have option to have horiznontal override neighbour ???
   matrix slope( n+1 ), vertical( n+1 ), horizontal( n+1 ) ;
   REAL a, b ;
   xBez.reset( n, 3 ) ;
   yBez.reset( n, 3 ) ;
   REAL   r, s ;
   for ( i = 1 ; i < n ; i++ ) {
      a = x(i+1) - x(i) ;
      vertical(i+1) = ( a == 0.0 ) ;
      b = y(i+1) - y(i) ;
      horizontal(i+1) = ( b == 0.0 ) ;
      if ( vertical(i+1) || horizontal(i+1) )
         slope(i+1) = 0.0 ;
      else
         slope(i+1) = ( y(i+1) - y(i) ) / ( x(i+1) - x(i) ) ;
   } // for
   slope(1) = slope(2) ;
   slope(n+1) = slope(n) ;
   vertical(1) = vertical(2) ;
   horizontal(1) = horizontal(2) ;
   vertical(n+1) = vertical(n) ;
   horizontal(n+1) = horizontal(n) ;
   xBez(1,1) = x(1) ;
   yBez(1,1) = y(1) ;
   for ( i = 1 ; i < n ; i++ ) {
      xBez(i,1) = x(i) ;
      yBez(i,1) = y(i) ;
      r = ( x(i+1) - x(i) ) / 3 ;
      if ( !vertical(i) && !vertical(i+1) ) {
         xBez(i,2) = x(i) + r ;
         s = ( slope(i) + slope(i+1) ) / 2 ;
         yBez(i,2) = y(i) + s * r ;
      } else {
         xBez(i,2) = x(i) ;
         yBez(i,2) = ( 2 * y(i) + y(i+1) ) / 3 ;
      } // else
      if ( !vertical(i+1) && !vertical(i+2) ) {
         xBez(i,3) = x(i) + 2 * r ;
         s = ( slope(i+2) + slope(i+1) ) / 2 ;
         yBez(i,3) = y(i+1) - s * r ;
      } else {
         xBez(i,3) = x(i+1) ;
         yBez(i,3) = ( y(i) + 2 * y(i+1) ) / 3 ;
      } // else
   } // for
   xBez(n,1) = x(n) ;
   yBez(n,1) = y(n) ;
   return *this ;
} // psFile bezier

psFile& psFile::wCurve( matrix& x, matrix& y, INDEX option )
{
   matrix xBez, yBez ;

   bezier( x, y, xBez, yBez ) ;
   INDEX i, p, q, n = x.nRows() ;
   comment( "Bezier curves thro' points" ) ;
   if ( option ) {
      p = 1 ;
      q = n ;
   } else {
      p = 2;
      q = n-1 ;
   } // else
   for ( i = p ; i < q ; i++ ) {
      wCoords( xBez(i,2), yBez(i,2) ) ;
      wCoords( xBez(i,3), yBez(i,3) ) ;
      wCoords( xBez(i+1,1), yBez(i+1,1) ) ;
      write( "  curveto\n" ) ;
   } // for
   return *this ;
} // psFile wCurve

psFile& psFile::wPoints( matrix& x, matrix& y, const charArray& pt )
{
   static char *mName = "psFile::wPoints" ;
   INDEX i, n = x.nRows() ;
   if ( n != y.nRows() )
      x.errorExit( mName, NEDIM ) ;
   if ( x.nCols() != 1 || y.nCols() != 1 )
      x.errorExit( mName, NOTVC ) ;
   gSave() ;
   wMoveTo( x(1), y(1) ) ;
   comment( "plotting points" ) ;
   align( pt, -0.5, -0.5 ) ;
   for ( i = 1 ; i <= n ; i++ ) {
      wMoveTo( x(i), y(i) ) ;
      align() ;
      show( pt ) ;
   } // for
   gRestore() ;
   return *this ;
} // psFile wPoints

psFile& psFile::wJoin( matrix& x, matrix& y, INDEX option )
{
   static char *mName = "psFile::wJoin" ;
   INDEX i, n = x.nRows() ;
   if ( n != y.nRows() )
      x.errorExit( mName, NEDIM ) ;
   if ( x.nCols() != 1 || y.nCols() != 1 )
      x.errorExit( mName, NOTVC ) ;
   comment( "joining points" ) ;
   if ( !option ) {
      for ( i = 2; i <= n ; i++ )
         wLineTo( x(i), y(i) ) ;
   } // if
   return *this ;
} // psFile wJoin

psFile& psFile::wEllipse( REAL x, REAL y, REAL theta,
                         REAL a, REAL b, REAL r ) 
{
   static char *mName = "psFile::wEllipse" ;
   REAL rScale = sqrt( xScale * xScale + yScale * yScale ) ;
   gSave() ;
   wTranslate( x, y ) ;
   moveTo(0,0) ;
   wRotate( theta ) ;
   scale( a, b ) ;
   newPath() ;
   arc( 0.0, 0.0, rScale * r, 0, 360 ) ;
   stroke() ;
   gRestore() ;
   return *this ;   
} // psFile wEllipse

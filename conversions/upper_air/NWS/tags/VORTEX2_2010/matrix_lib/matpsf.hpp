/**************************************************/
/*       matpsf.hpp header for psFile class       */
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
#include "matrix.hpp"
#endif

#ifndef MATPSF_H

#define MATPSF_H

struct psWindow { REAL x1, y1, x2, y2 ; } ;

struct psWorld { REAL x1, y1, x2, y2 ; } ;

enum psOptions {
         psNULL = 0, psBORDER = 1, psFILL = 2,
         psLEFT = 4, psDOWN = 8
} ;

class psFile : public outFile
{
      psWindow  stdWin ;
      psWorld   stdWorld ;

      INDEX     width, prec ;     // field widths for writes
      REAL      unit ;            // unit of length in points
      REAL      xScale, yScale ;  // scaling world to window
      psWindow  pWin ;            // current window
      psWorld   pWorld ;          // current world
      REAL      pi_4 ;            // == pi/4

   public:

      psFile( char *name ) ;
      psFile( psFile& f ) ;
      psFile( char *name, psWindow& win ) ;
      psFile& operator = ( psFile& f ) ;
      ~psFile( void ) ;

      psFile&  setScales( void ) ;
      psFile&  setWindow( psWindow& win, INDEX option = 0 ) ;
      psFile&  setWorld( psWorld& world ) ;
      psFile&  comment( const char *str ) ;

      // writeCoord writes coord * unit
      psFile&  setUnit( REAL newUnit ) ;
      psFile&  points( REAL pt ) ;
      psFile&  units( REAL r ) ;
      psFile&  coords( REAL x, REAL Y ) ;


      psFile&  translate( REAL x, REAL y ) ;
      psFile&  rotate( REAL theta ) ;
      psFile&  moveTo( REAL x, REAL y ) ;
      psFile&  rMoveTo( REAL a, REAL b ) ;
      psFile&  gSave( void ) ;
      psFile&  gRestore( void ) ;
      psFile&  newPath( void ) ;
      psFile&  lineTo( REAL x, REAL y ) ;
      psFile&  rLineTo( REAL a, REAL b ) ;
      psFile&  curveTo( REAL x1, REAL y1, REAL x2, REAL y2,
                     REAL x3, REAL y3 ) ;
      psFile&  arc( REAL x, REAL y, REAL radius, REAL ang1, 
                    REAL ang2 ) ;
      psFile&  closePath( void ) ;
      psFile&  stroke( void ) ;
      psFile&  fill( void ) ;
      psFile&  setLineWidth( REAL lineWidth ) ;
      psFile&  scale( REAL xScale, REAL yScale ) ;
      psFile&  setGray( REAL gray ) ;
      psFile&  setDash( const charArray& str ) ;

      psFile&  findFont( char *font ) ;
      psFile&  scaleFont( REAL size ) ;
      psFile&  setFont( void ) ;
      psFile&  show( const charArray& str, REAL xFact = 0.0,
                     REAL yFact = 0.0 ) ;
      psFile&  charPath( const charArray& str ) ;
      psFile&  charBlank( const charArray& str ) ;
      psFile&  align( const charArray& str, REAL xFact, REAL yFact ) ;
      psFile&  align( void ) ;

      // world based procedures
      psFile&  wUnits( REAL r, INDEX option = 0 ) ;
      psFile&  wCoords( REAL x, REAL y ) ;

      psFile&  wTranslate( REAL x, REAL y ) ;
      psFile&  wRotate( REAL theta ) ;
      psFile&  wMoveTo( REAL x, REAL y ) ;
      psFile&  wLineTo( REAL x, REAL y ) ;

      psFile&  setWorld( psWorld& world, matrix& x, matrix& y ) ;

      psFile&  axes( INDEX option = 0 ) ;
      psFile&  labelAxes( const charArray& xLabel,
                          const charArray& yLabel,
                          INDEX option = 0 ) ;
      psFile&  axes( const charArray& xLabel,
                     const charArray& yLabel,
                     INDEX option = 0 ) ;

      psFile&  bezier( matrix& x, matrix& y, matrix& xBez, matrix& yBez ) ;
      psFile&  wJoin( matrix& x, matrix& y, INDEX option = 0 ) ;
      psFile&  wCurve( matrix& x, matrix& y, INDEX option = 0 ) ;
      psFile&  wPoints( matrix& x, matrix& y, const charArray& pt ) ;

      psFile&  wEllipse( REAL x, REAL y, REAL theta,
                        REAL a, REAL b, REAL r ) ;

      psFile& showPage( void ) ;

} ; // class psFile

#endif





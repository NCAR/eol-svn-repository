/**************************************************/
/*    matspec.hpp header for matSpecFunc family   */
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

#ifndef MATSPEC_H

#define MATSPEC_H

#include <math.h>

class matSpecFunc : public matObject
{
   protected :

      matError  error ;
      INDEX     maxIter ;
      REAL      eps ;

   public :
  
#ifdef __cplusplus
      static const REAL   zero,  half,  one,   two,    three,  
                   four,  five,  six,   seven, eight,  nine,
                   ten,   root2, pi;
#else
      static REAL  zero,  half,  one,   two,    three,  
                   four,  five,  six,   seven, eight,  nine,
                   ten,   root2, pi;
#endif

      matSpecFunc( void ) ;
      matSpecFunc( const matSpecFunc& fn ) ;

      ~matSpecFunc( void )  ;
      void operator = ( matSpecFunc& fn ) 
         { error = fn.error ; }

      INDEX ok ( void ) 
         { return !error ; } 

      virtual outFile& info( outFile& f ) M_CONST ;
      outFile& specInfo( outFile& f, const char* fName ) M_CONST ;

      // virtual class based on
      virtual REAL value( REAL r ) ;
      virtual REAL inv( REAL v ) ;
      REAL operator () ( REAL r ) ;

      // iterators
      matrix operator () ( const matrix& x ) ;
      virtual matrix inv( const matrix& x ) ;
      matrix& transform( matrix& z, const matrix& x ) ;
      matrix& invTrans( matrix& z, const matrix& x ) ;

      INDEX setIter( INDEX newMax = 0 ) ;
      REAL  setEps( REAL newEps = -1.0 ) ;

} ; // class matSpecFunc

class logGammaFunc : public matSpecFunc
{
      // logGamma function

   public:

      logGammaFunc( void ) ;
      ~logGammaFunc( void ) ;
      
      REAL value( REAL r ) ;
      outFile& info( outFile& f ) M_CONST ;

} ; // class logGammaFunc

extern logGammaFunc logGamma ;

extern REAL logBeta( REAL z, REAL w ) ;

class incBetaFunc : public matSpecFunc
{
      // parameters a,b with c = logBeta(a,b)

      REAL a, b, c ;

      REAL fraction( REAL x, REAL a, REAL b, REAL c ) ;

   public:

      incBetaFunc( void ) ;
      ~incBetaFunc( void ) ;
      incBetaFunc( incBetaFunc& fn ) ;
      void operator = ( incBetaFunc& fn ) ;
      
      REAL value( REAL r ) ;
      outFile& info( outFile& f ) M_CONST ;

      incBetaFunc& par( REAL newA, REAL newB ) ;

      matrix operator() ( const matrix& x, REAL newA, REAL newB ) ;
      REAL operator() ( REAL r, REAL newA, REAL newB ) ;

} ; // class incBetaFunc

extern incBetaFunc incBeta ;

class incGammaFunc : public matSpecFunc
{
      // incomplete Gamma Function
      // Parameter a with c = logGamma(a) ;

      REAL a, c ;

      REAL fraction( REAL x ) ;
      // returns estimate of complement of incGamma

      REAL series( REAL x ) ;
      // returns estimate of incGamma

   public:

      incGammaFunc( void ) ;
      ~incGammaFunc( void ) ;
      incGammaFunc( incGammaFunc& fn ) ;
      void operator = ( incGammaFunc& fn ) ;
      
      REAL value( REAL r ) ;
      outFile& info( outFile& f ) M_CONST ;
 
      incGammaFunc& par( REAL newA ) ;

      matrix operator() ( const matrix& x, REAL newA ) ;
      REAL operator() ( REAL r, REAL newA ) ;

} ; // class incGammaFunc

extern incGammaFunc incGamma ;

class normalFunc : public matSpecFunc
{
      // Normal Distribution

      REAL  lower, upper ;
      int   calcUpper ;

   public:

      normalFunc( void ) ;
      ~normalFunc( void ) ;
      normalFunc( normalFunc& fn ) ;
      void operator = ( normalFunc& fn ) ;
      
      REAL value( REAL r ) ;
      REAL inv( REAL p ) ;
      outFile& info( outFile& f ) M_CONST ;
 
      normalFunc& par( REAL newLower, REAL newUpper ) ;
      void upperTail( int l ) { calcUpper = l ; } 
      matrix operator() ( const matrix& x ) ;
      matrix inv( const matrix& x ) ;
      REAL operator() ( REAL r ) ;

} ; // class normalFunc

extern normalFunc normal ;

extern REAL possionDist( REAL k, REAL mean ) ;
extern REAL chi2Dist( REAL r, INDEX dof ) ;
extern REAL studentDist( REAL r, INDEX dof ) ;
extern REAL FDist( REAL r, INDEX dof1, INDEX dof2 ) ;

#ifndef __cplusplus
extern matrix possionDistM( const matrix& x, REAL mean ) ;
extern matrix chi2DistM( const matrix& x, INDEX dof ) ;
extern matrix studentDistM( const matrix& x, INDEX dof ) ;
extern matrix FDistM( const matrix& x, INDEX dof1, INDEX dof2 ) ;
#else
extern matrix possionDist( const matrix& x, REAL mean ) ;
extern matrix chi2Dist( const matrix& x, INDEX dof ) ;
extern matrix studentDist( const matrix& x, INDEX dof ) ;
extern matrix FDist( const matrix& x, INDEX dof1, INDEX dof2 ) ;
#endif 

#endif

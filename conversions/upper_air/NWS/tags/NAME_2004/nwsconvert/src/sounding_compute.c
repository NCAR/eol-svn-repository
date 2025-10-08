/******************************************/
/*                                        */
/* SOUNDING_COMPUTE:                      */
/*   A set of functions and procedures to */
/* smooth angles and calculate winds.     */
/*                                        */
/* Do not forget to add the -lm to the    */
/* compile/link command.                  */
/*                                        */
/******************************************/
/*	May 1999		          */
/* Made changes to smooth_elev routine    */
/* Problem cause by excessive number of   */
/* missing elevation and/or azimuth angles*/
/* current algorithm just insures end     */
/* points angles not missing there is no  */
/* check on number of missing angles      */
/* contained in interval. Files containing*/
/* a large number of missing elev or azim */
/* angles created infinite loops          */
/* Solution			          */
/* Counted number of missing angles       */ 
/* contained within[start_index,end_index]*/
/* called miss_freq if miss_freq/num_obs  */
/* greater than .25 i.e 75% non-missing   */
/* than no smoothing performed            */
/* currently threshold % experimental     */
/* further testing required               */
/*                                        */
/*     Feb 2005                           */
/* Removed Notch Limit checking.          */
/******************************************/     

#include <iostream.h>
#include <math.h>
#include <stdio.h>
#include <matrix.hpp>
#include "sounding_def.h"

#define ORDER 9
#define THRESHOLD 1.5
#define UPPER_NOTCH_LIMIT 190.0
#define LOWER_NOTCH_LIMIT 90.0
#define END_SPACE 40
#define START_SPACE 40

matrix solve_it(matrix a, matrix b)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  matrix coef, tmp;
  double sum, val;
  unsigned int i, j, k;

  tmp = a;
  coef = b;

//
// Gussian elemination.
//

  for(i=1;i<=tmp.nRows();i++)
  {
    if(tmp(i,i) != 0.0)
    {
      for(j=i+1;j<=tmp.nRows();j++)
      {
        if(tmp(j,i) != 0.0)
	{
          val = -1.0*(tmp(i,i)/tmp(j,i));
          for(k=i;k<=tmp.nCols();k++)
	  {
            tmp(j,k) = tmp(j,k)*val+tmp(i,k);
	  }
          coef(j,1) = coef(j,1)*val + coef(i,1);
	} // if tmp(j,i) != 0
      } // for j.
    } // end if tmp(i,i) != 0.
  } // end if elemination.

//  tmp.print();

//
// Back sub.
//

  for(i=tmp.nRows();i>=1;i--)
  {
    if(tmp(i,i) != 0)
    {
      sum = 0.0;
      for(j=i+1;j<=tmp.nCols();j++)
        sum += tmp(i,j)*coef(j,1);
      coef(i,1) = (coef(i,1) - sum)/tmp(i,i);
    } // if tmp(i,i) != 0
    else
      coef(i,1) = 0.0; 
  } // for i.

  return(coef);
} // End of solve_it.

void smooth_elev(double w_ang[MAX_OB], double elev_smooth[MAX_OB],
                 double time[MAX_OB], int npts, int angle_status[MAX_OB],
                 double lim_ang[360], SOUND_DATA_ARR sd, int azim_flag)
/******************************************/
/*                                        */
/*                                        */
/* For matices mat[row][col].             */
/*                                        */
/* angle_status flags:                    */
/* 0 good.                                */
/* 1 outlier                              */
/* 9 missing                              */
/*                                        */
/******************************************/
{
  matrix a_mat, y_mat, b_mat, coef_mat;
  double sumxx[2*ORDER+1], sumxy[ORDER+1], residual[MAX_OB];
  double x_pow, num, stand_dev, stand_dev_sq;
  double fund_freq, delta_t, theta;
  double a_o_coef[MAX_OB], b_o_coef[MAX_OB];
  double a_s_coef[MAX_OB], b_s_coef[MAX_OB];
  double x0, x1, x2, dt, sumyy;
  double val_o, val_s;
  float ratio=0;;

  int orgin_f, num_sample_pts, num_freq;
  int i, j, k, start_index, end_index, num_pts;
  int out_min_ind, out_max_ind, azim_ind;
  int work_flag, low_elev_count, miss_freq;

//
// Inializion of varibles.
//

  start_index = 10;
  end_index = npts;
  work_flag = 1;
  low_elev_count = 0;
  miss_freq = 0;
//
// Adjust the start and end points so that they do
// not fall on missing values.  If the difference
// between the start point and end point is <= 150
// do not create the fourier series. 
//

  while((start_index < end_index)&&(angle_status[start_index] == 9))
    start_index++;
  start_index++;

  while((end_index > start_index)&&(angle_status[end_index] == 9))
    end_index--;

// Check for number of missing elev angles 

  for(i=start_index;i<=end_index;i++){
	if(angle_status[i] == 9)
		miss_freq++;
  }
  
  num_pts = end_index-start_index;
/*
  cout << "miss_freq " << miss_freq << endl;
  cout << "num_pts " << num_pts << endl;
  cout << "start_index " << start_index << " end_index " << end_index << endl;
  cout << "start time " << time[start_index] << endl;
  cout << "start elev angle " << w_ang[start_index] << endl;
  cout << "end time " << time[end_index] << endl;
  cout << "end  elev angle " << w_ang[end_index] << endl;
*/
  if(num_pts)
  	ratio = (float)miss_freq/num_pts;
//  cout << "percentage of missing points " << ratio << "%" << endl;
  if(num_pts <= 150 || ratio > .25)
    work_flag = 0;

//
// Remove outliers.
// Use 2 min interval to compute 2nd degree fit to curve
// centered at point i.
// Use only "good" point in fit.
// Use 1 min interval to check points.
//

  for(i=start_index;i<end_index;i++)
  {
    sumyy = 0.0;
    for(j=0;j<2*ORDER+1;j++)
      sumxx[j] = 0.0;
    for(j=0;j<ORDER+1;j++)
      sumxy[j] = 0.0;

    out_min_ind = i-10;
    if(out_min_ind <0)
      out_min_ind = 0;
    out_max_ind = i+10;
    if(out_max_ind>=end_index)
      out_max_ind = end_index-1;

//
// Calulate values for curve by solving Normal Equation
//

    for(j=out_min_ind;j<=out_max_ind;j++)
    { // for j.
      if(angle_status[j] == 0)
      { // Good point.
        dt = time[j] - time[i];
        sumxx[0] += 1.0;
        sumxx[1] += dt;
        sumxx[2] += dt*dt;
        sumxx[3] += dt*dt*dt;
        sumxx[4] += dt*dt*dt*dt;
        sumxy[0] += w_ang[j];
        sumxy[1] += w_ang[j]*dt;
        sumxy[2] += w_ang[j]*dt*dt;
        sumyy += w_ang[j]*w_ang[j];
      } // Good point.
    } // for j.

// Calculate coef.

    a_mat.assign(3,3);
    for(k=1;k<=3;k++)
      for(j=1;j<=3;j++)
        a_mat(k,j) = sumxx[(k-1)+(j-1)];

    y_mat.assign(3,1);
    for(k=1;k<=3;k++)
      y_mat(k,1) = sumxy[k-1];

    coef_mat = solve_it(a_mat, y_mat);
    x0 = coef_mat(1,1);
    x1 = coef_mat(2,1);
    x2 = coef_mat(3,1);

    stand_dev_sq = (sumxx[0]*sumyy-sumxy[0]*sumxy[0])/(sumxx[0]*(sumxx[0]-1));
    if(stand_dev_sq <0.0)
      stand_dev_sq *= -1.0;
    stand_dev = sqrt(stand_dev_sq);

    out_min_ind = i-5;
    if(out_min_ind <0)
      out_min_ind = 0;
    out_max_ind = i+5;
    if(out_max_ind>=npts)
      out_max_ind = npts-1;

    for(j=out_min_ind;j<=out_max_ind;j++)
    { //
      dt = time[j] - time[i];
      if(angle_status[j] != 9)
      {
        if(fabs(w_ang[j]-x0-dt*x1-dt*x2*x2) >THRESHOLD*stand_dev)
          angle_status[j] = 4;
      }
    } //
//cout<<i<<" "<<angle_status[i]<<endl;
  } // for i.

//
// Smooth elev angles due to low elev angle.
// Use the residual array as temp storage.
//

  for(i=0;i<npts;i++)
    residual[i] = w_ang[i];

  if(azim_flag == 0)
  {
    for(i=start_index;i<end_index;i++)
    {

      azim_ind = (int)(sd[i].azim+180.0);
      if(azim_ind > 360)
        azim_ind -= 360;

      if((angle_status[i] != 9) && (w_ang[i] <= lim_ang[azim_ind]+10.0))
      {

        low_elev_count++;

        for(j=0;j<2*ORDER+1;j++)
	  sumxx[j] = 0.0;
        for(j=0;j<ORDER+1;j++)
          sumxy[j] = 0.0;

        if(w_ang[i] <= lim_ang[azim_ind]+5.0)
	{
          out_min_ind = i-20;
          out_max_ind = i+20;
	}
        else if(w_ang[i] <= lim_ang[azim_ind]+7.5)
	{
          out_min_ind = i-15;
          out_max_ind = i+15;
	}
        else
	{
          out_min_ind = i-10;
          out_max_ind = i+10;
	}
        if(out_min_ind < 0)
          out_min_ind = 0;
        if(out_max_ind >= npts)
          out_max_ind = npts-1;

        for(j=out_min_ind;j<=out_max_ind;j++)
        { // for j.
         if(angle_status[j] == 0)
         { // Good point.
           dt = time[j] - time[i];
           sumxx[0] += 1.0;
           sumxx[1] += dt;
           sumxx[2] += dt*dt;
           sumxy[0] += w_ang[j];
           sumxy[1] += w_ang[j]*dt;
           sumyy += w_ang[j]*w_ang[j];
        } // Good point.
      } // for j.

// Calculate coef.

      coef_mat(2,1) = (sumxx[0]*sumxy[1]-sumxx[1]*sumxy[0])/
                      (sumxx[0]*sumxx[2]-sumxx[1]*sumxx[1]);

      coef_mat(1,1) = sumxy[0]/sumxx[0]-coef_mat(2,1)*sumxx[1]/sumxx[0];

      if((sumxx[0] < 1.0) || ((sumxx[0]*sumxx[2]-sumxx[1]*sumxx[1]) == 0))
        residual[i] = w_ang[i];
      else
        residual[i] = coef_mat(1,1); 

      } // If non-missing data.
    } // for i;


    for(i=0;i<npts;i++)
    {
      w_ang[i] = residual[i];
    }

  } // If azim_flag.

  if(low_elev_count >= (0.2*(end_index-start_index)))
    work_flag = 0;

//
// Collect values for least square fit, then fill the
// matrices with the values.
//

  for(i=0;i<2*ORDER+1;i++)
    sumxx[i] = 0.0;
  for(i=0;i<ORDER+1;i++)
    sumxy[i] = 0.0;

  for(i=start_index;((i<end_index)&&(work_flag));i++)
  {
    if(w_ang[i] != 999.0)
    { // if point is not missing.
      x_pow = 1.0;
      for (j=0;j<2*ORDER+1;j++)
      {
        sumxx[j] += x_pow;
        x_pow = x_pow * time[i];
      }
    } // if point is not missing.
  } // Loop over data set.

  for(i=start_index;i<end_index;i++)
  {
    if(w_ang[i] != 999.0)
    {
      x_pow = w_ang[i];
      for(j=0;j<ORDER+1;j++)
      {
        sumxy[j] += x_pow;
        x_pow = x_pow*time[i];
      }
    } // End of if.
  } // End of loop over data set..

  a_mat.assign(ORDER+1,ORDER+1);
  for(i=1;i<=ORDER+1;i++)
    for(j=1;j<=ORDER+1;j++)
      a_mat(i,j) = sumxx[(i-1)+(j-1)];

  y_mat.assign(ORDER+1,1);
  for(i=1;i<=ORDER+1;i++)
    y_mat(i,1) = sumxy[i-1];

//
// Solve.
//

  if(work_flag)
  {
//    coef_mat = solve_it(a_mat, y_mat);
    b_mat = a_mat.inv();
    coef_mat = b_mat * y_mat;
  }

//
// Construct smooth elev from fit curve.
//

  for(i=0;((i<end_index)&&(work_flag));i++)
  {
    if(i<start_index)
      elev_smooth[i] = w_ang[i];
    else
    {
      num = coef_mat(1,1);
      elev_smooth[i] = num;
      x_pow = 1.0;

      for(j=2;j<=ORDER+1;j++)
      {
        num = coef_mat(j,1);
        x_pow = x_pow * time[i];
        elev_smooth[i] += x_pow*num;
      }
    } // Else
//cout << elev_smooth[i] << endl;
  } // For i.


//
// Calculate residuals.
//

  for(i=0;((i<end_index)&&(work_flag));i++)
  {
    if(!angle_status[i])
    {
      residual[i] = w_ang[i] - elev_smooth[i];
    }
    else
      residual[i] = 0.0;
  } //for i;

//
// The Fourier series required that every point must have a value.
// For missing data n=and outliers and interoplated residual will
// be found.
//

  for(i=start_index;((i<end_index)&&(work_flag));i++)
  {
    if(angle_status[i] != 0)
    { // Have an outlier or missing pt.

      j = i-1;
      while((angle_status[j] != 0) && (j>=0))
        j--;

      k=i+1;
      while((angle_status[k] != 0) && (k<npts))
        k++;

      if((j<0) || (k==npts))
        residual[i] = 0.0;
      else
        residual[i] = residual[k] + ((double)(i-k)*(residual[j]-residual[k]))/
                                    (double)(j-k);

    } // Have an outlier or missing pt.
  }


//  for(i=0;i<npts;i++)
//   cout << angle_status[i] << endl;

//
// Finite Fourier series on residuals.
//

// correct starting point, need an even number of points.

  if(work_flag)
  {
    if(((end_index-start_index) % 2) == 1)
      start_index -= 1;

    num_sample_pts = end_index-start_index;
    num_freq = num_sample_pts/2;
    delta_t = time[npts-1]/(npts-1);
    fund_freq = 1.0/((double)(num_sample_pts)*delta_t);
    orgin_f = start_index+num_freq;

    for(i=0;i<=num_freq;i++)
    {
      a_o_coef[i] = 0.0;
      b_o_coef[i] = 0.0;
      for(j=-num_freq;j<num_freq;j++)
      {
        theta = (2.0*M_PI*(double)(i*j))/(double)(num_sample_pts);
        a_o_coef[i] += residual[orgin_f+j]*cos(theta);
        b_o_coef[i] += residual[orgin_f+j]*sin(theta);
      } // for j
      a_o_coef[i] = a_o_coef[i]/num_sample_pts;
      b_o_coef[i] = b_o_coef[i]/num_sample_pts;
    } // for i

    for(i=0;i<=num_freq;i++)
    {
      a_s_coef[i] = a_o_coef[i];
      b_s_coef[i] = b_o_coef[i];
    }

//
// Notch filters.
//

    for(i=1;i<=num_freq;i++)
      if(((double)(i)*fund_freq) >= (1.0/30.0))
      {
        a_s_coef[i] = a_o_coef[i]*1.0e-1;
        b_s_coef[i] = b_o_coef[i]*1.0e-1;
      }

/*
    if(azim_flag == 0)
    {
      for(i=1;i<=num_freq;i++)
        if((((double)(i)*fund_freq) >= (1.0/UPPER_NOTCH_LIMIT)) &&
          (((double)(i)*fund_freq) <= (1.0/LOWER_NOTCH_LIMIT)))

        {
          a_s_coef[i] = a_o_coef[i]*1.0e-10;
          b_s_coef[i] = b_o_coef[i]*1.0e-10;
        }
    }
*/

//
// Reconstruct smooth elev from fit curve and freq coef.
// elev_smooth is the fitted curve.
//

//cout << fund_freq << endl;
//for(i=0;i<=num_freq;i++)
//  cout << a_f_coef[i] << " " << b_f_coef[i] << endl;


     for(i=-num_freq;i<num_freq;i++)
     { // For i for final reconstruct.
       val_o = a_o_coef[0];
       val_s = a_s_coef[0];
       theta = 2.0*M_PI*fund_freq*delta_t*(double)(i);
       for(j=1;j<num_freq;j++)
       {
         val_o += 2.0*(a_o_coef[j]*cos((double)(j)*theta) +
            b_o_coef[j]*sin((double)(j)*theta));
         val_s += 2.0*(a_s_coef[j]*cos((double)(j)*theta) +
            b_s_coef[j]*sin((double)(j)*theta));
       }
       val_o += a_o_coef[num_freq]*cos(2.0*M_PI*(double)(num_freq*i)*fund_freq*delta_t);
       val_s += a_s_coef[num_freq]*cos(2.0*M_PI*(double)(num_freq*i)*fund_freq*delta_t);

       if((orgin_f+i) <= (start_index + START_SPACE))
       {
         elev_smooth[orgin_f+i] += ((val_o-val_s)*(start_index-(orgin_f+i))+
                START_SPACE*val_o)/START_SPACE;
       }
       else  if ((orgin_f+i) >= (end_index - END_SPACE))
       {
         elev_smooth[orgin_f+i] += ((val_s*(END_SPACE+1))+(end_index-END_SPACE-
              (orgin_f+i))*(val_s-val_o))/(END_SPACE+1);
       }
       else
         elev_smooth[orgin_f+i] += val_s;

    } // For i for final reconstruct.
  } // If work_flag.
  else
  {
//cout<<start_index<<" "<<end_index<<endl;
    for(i=0;i<npts;i++)
      elev_smooth[i] = w_ang[i];    
  }

} // End of smooth_elev..


void smooth_azim(double w_ang[MAX_OB], double azim_smooth[MAX_OB],
                 double time[MAX_OB], int npts, int angle_status[MAX_OB],
                 double lim_ang[360],SOUND_DATA_ARR sd)
/******************************************/
/*                                        */
/* j is a non-missing point with the      */
/* closest index less then i.             */
/*                                        */
/******************************************/
{
  double diff, offset, tmp_azim[MAX_OB];
  int i, j, start_index;

  for(i=0;i<npts;i++)
    tmp_azim[i] = w_ang[i];

  start_index = 1;
  offset = 0.0;

  for(i=start_index;i<npts;i++)
  {
    j = i-1;
    while((angle_status[j] == 9) && (j>=0))
      j--;

    if((j != -1) && (angle_status[i] != 9))
    { // Both i and j are non-missing points.
//      diff = tmp_azim[j]-tmp_azim[i];
      diff = w_ang[j]-w_ang[i];
      if(fabs(diff) > 340.0)
//         tmp_azim[i] = tmp_azim[i] + 360.0*(diff/fabs(diff));
         offset +=  360.0*(diff/fabs(diff));

      tmp_azim[i] = w_ang[i] + offset;

    } // Both i and j are non-missing points.

//cout<<w_ang[i]<<" "<<tmp_azim[i]<<" "<<i<<" "<<w_ang[j]<<" "<<tmp_azim[j]<<" "<<j<<endl;

  } // For i.

  smooth_elev(tmp_azim,azim_smooth,time,npts,angle_status,lim_ang,sd,1);

//for(i=0;i<npts;i++)
//  cout << tmp_azim[i] << " " << angle_status[i]<<endl;


  for(i=0;i<npts;i++)
  {
    while(azim_smooth[i] <0.0)
      azim_smooth[i] += 360.0;
    while(azim_smooth[i] >=360.0)
      azim_smooth[i] -= 360.0;
  }

} // End of smooth_azim.

void cal_position(double elev[MAX_OB], double azim[MAX_OB], double alt[MAX_OB],
                  double x[MAX_OB], double y[MAX_OB], int qxy[MAX_OB], int num)
/******************************************/
/*                                        */
/* For TOGA COARE azim is from site to    */
/* sonde.                                 */
/* For NWS azim is from sonde to site.    */
/*                                        */
/******************************************/
{
  double rad_earth=6378388.0, conv=0.0174533;
  double elevr, azimr, theta, arc_length;
  double work_val;
  int i;

  x[0] = 0.0;
  y[0] = 0.0;
  qxy[0] = 99;
  for(i=1;i<num;i++)
  { // loop over data.
    if((alt[i] != 99999.0) && (azim[i] <= 360.0) && (azim[i] >=0.0) &&
       (elev[i] <= 90.0) && (elev[i] >= 0.0))
    {
      elevr = elev[i] * conv;
      azimr = (azim[i]-180.0) * conv;
      work_val = asin((rad_earth*cos(elevr))/((rad_earth+alt[i])));
      if((work_val <= 0.0) || (work_val >= 3.2))
      {
        work_val = (rad_earth*cos(elevr))/((rad_earth+alt[i]));
      }
      theta = (M_PI)/(double)(2.0) - elevr - work_val;;
      arc_length = theta*rad_earth;
      x[i] = arc_length * sin(azimr);
      y[i] = arc_length * cos(azimr);
      qxy[i] = 99;
    } // Valid data to cal position.
    else
    {
      x[i] = x[i-1];
      y[i] = y[i-1];
      qxy[i] = 9;
    }
  } // loop of data.
} // End of cal_position.


void cal_uv(SOUND_DATA_ARR sd, double x[MAX_OB], double y[MAX_OB],
           double xs[MAX_OB], double ys[MAX_OB], int qxy[MAX_OB], 
           int angle_status[MAX_OB], int num)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i, j, k, min_ind=0, max_ind=0, spacing, left_tot, right_tot;
  int flag=0, linear_flag=0, end_flag=0, index_flag=0;
  double st[5], sx[3], sy[3], xa0=0, xa1=0, ya0=0, ya1=0, dt;
  matrix a_mat, y_mat, coef_mat;

  for(i=1;i<MAX_OB;i++)
    if(angle_status[i] == 9)
      qxy[i] = 9;

  for(i=0;i<num;i++)
  { // for i, loop over entire data set

    end_flag = 0;
    index_flag = 0;

    if(sd[i].press <50.0)
      spacing = 15; // Half the smoothing interval centered around i.
    else
      spacing = 10; // Half the smoothing interval centered around i.

//
// Get start and stop indeces.
//

    while(!index_flag)
    {

      if(end_flag)
        index_flag = 1;

      if((i-spacing) < 0)
      {
        min_ind = 0;
        end_flag = 1;
      }
      else
        min_ind = i-spacing;
      if((i+spacing) >= num)
      {
        max_ind = num - 1;
        end_flag = 1;
      }
      else
        max_ind = i+spacing;

//
// Check to see if there are the correct number of point
// to proform the calulation.
//

      left_tot = 0;
      for(j=min_ind;j<i;j++)
        if((qxy[j] == 99) || (j== 0))
          left_tot++;
      right_tot = 0;
      for(j=i+1;j<=max_ind;j++)
        if(qxy[j] == 99)
          right_tot++;

      flag = 0;
      if((left_tot >0) && (right_tot >0) && ((left_tot+right_tot)>3) &&
         (qxy[i] != 9))
        flag = 1;
      else if ((left_tot >= 3) && (qxy[i] == 99))
        flag = 1;
      else if ((right_tot >= 3) && (qxy[i] == 99))
        flag = 1;

      if((left_tot < (spacing/3)) || (right_tot < (spacing/3)))
        linear_flag = 1;
      else 
        linear_flag = 0;

      if(end_flag)
      {
        linear_flag = 0;
        spacing += 5;
      }
      else
        index_flag = 1;



    } // While !index_flag

//
// Compute curve fit for x and y over min_ind to max_ind.
//

    for(j=0;j<5;j++)
      st[j] = 0.0;
    for(j=0;j<3;j++)
    {
      sx[j] = 0.0;
      sy[j] = 0.0;
    }

    if((qxy[i] != 9) || (j == 0))
    { // Point not missing.
      for(j=min_ind;j<max_ind;j++)
      { // compute stats.
        if(((qxy[j] == 99) && (flag == 1)) || (j == 0))
	{
          dt = sd[j].time - sd[i].time;
          st[0] += 1.0;
          st[1] += dt;
          st[2] += dt*dt;
          st[3] += dt*dt*dt;
          st[4] += dt*dt*dt*dt;
          sx[0] += x[j];
          sx[1] += x[j]*dt;
          sx[2] += x[j]*dt*dt;
          sy[0] += y[j];
          sy[1] += y[j]*dt;
          sy[2] += y[j]*dt*dt;
	}
      } // compute stats.
      if(flag == 1)
      {
        if(linear_flag)
	{
          xa1 = (st[0]*sx[1]-st[1]*sx[0])/(st[0]*st[2]-st[1]*st[1]);
          ya1 = (st[0]*sy[1]-st[1]*sy[0])/(st[0]*st[2]-st[1]*st[1]);
          xa0 = sx[0]/st[0]-xa1*st[1]/st[0];
          ya0 = sy[0]/st[0]-ya1*st[1]/st[0];
        }
        else
	{
          a_mat.assign(3,3);
          for(k=1;k<=3;k++)
            for(j=1;j<=3;j++)
              a_mat(k,j) = st[(k-1)+(j-1)];

          y_mat.assign(3,1);
          for(k=1;k<=3;k++)
            y_mat(k,1) = sx[k-1];

          coef_mat = solve_it(a_mat, y_mat);
          xa0 = coef_mat(1,1);
          xa1 = coef_mat(2,1);
//          xa2 = coef_mat(3,1);

          y_mat.assign(3,1);
          for(k=1;k<=3;k++)
            y_mat(k,1) = sy[k-1];

          coef_mat = solve_it(a_mat, y_mat);
          ya0 = coef_mat(1,1);
          ya1 = coef_mat(2,1);
//          ya2 = coef_mat(3,1);

	} // Else linear_flag.
      } // If flag true.

    } // Point not missing.

    if((qxy[i] != 9) && (flag == 1))
    {
      xs[i] = xa0;
      ys[i] = ya0;
      sd[i].u_cmp = xa1;
      sd[i].v_cmp = ya1;
    }
    else
    {
      xs[i] = x[i];
      ys[i] = y[i];
      sd[i].u_cmp = 9999.0;
      sd[i].v_cmp = 9999.0;
    }

    if((angle_status[i] == 4) || (angle_status[i] == 9))
      sd[i].qu = sd[i].qv = angle_status[i];
    else
      sd[i].qu = sd[i].qv = qxy[i];

    if(sd[i].u_cmp == 9999.0)
    {
      sd[i].qu = sd[i].qv = 9.0;
    }
    else if((fabs(sd[i].u_cmp) >= 250.0) || (fabs(sd[i].v_cmp) >= 250.0))
    {
      sd[i].u_cmp = sd[i].v_cmp = 9999.0;
      sd[i].qu = sd[i].qv = 9.0;
    }

  } // for i.

//for(i=0;i<num;i++)
//cout <<x[i]<<" "<<y[i]<<" "<<xs[i]<< " "<<ys[i]<<endl;

} // End of cal-uv.

void cal_latlon(SOUND_DATA_ARR sd, double xs[MAX_OB], double ys[MAX_OB],
               float lon, float lat, int pos_status[MAX_OB])
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i;
  double x_pos, y_pos;


  sd[0].lon = lon;
  sd[0].lat = lat;
  
  for(i=1;i<MAX_OB;i++)
  {
    if((pos_status[i] == 99) && (sd[i].alt != 99999.0) && (sd[i].u_cmp != 9999.0 && sd[i].v_cmp != 9999.0))
    {
      x_pos = xs[i] - xs[0];
      y_pos = ys[i] - ys[0];
      sd[i].lat = lat + y_pos/111033.0;
      sd[i].lon = lon + x_pos/(111033.0*cos(sd[i].lat*M_PI/180.0));
    }
    else
    {
      sd[i].lat = 999.000;
      sd[i].lon = 9999.000;
    }
  }

} // End of cal_latlon.


void cal_spddir(SOUND_DATA_ARR sd)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i;

  for(i=1;i<MAX_OB;i++)
  {
    if((sd[i].u_cmp == 0.0)&&(sd[i].v_cmp == 0.0))
    {
      sd[i].wind_spd = 0.0;
      sd[i].wind_dir = 0.0;
      sd[i].qu = sd[i].qv = 3.0;
    }
    else if((sd[i].u_cmp != 9999.0)&&(sd[i].v_cmp != 9999.0))
    {
      sd[i].wind_spd = sqrt(sd[i].u_cmp*sd[i].u_cmp+sd[i].v_cmp*sd[i].v_cmp);
      sd[i].wind_dir = (atan2(sd[i].u_cmp,sd[i].v_cmp)*180.0/M_PI)+180;
    }
    else
    {
      sd[i].wind_spd = 999.0;
      sd[i].wind_dir = 999.0;
    }

    if(sd[i].wind_spd > 999.0)
    {
      sd[i].u_cmp = 9999.0;
      sd[i].v_cmp = 9999.0;
      sd[i].wind_spd = 999.0;
      sd[i].wind_dir = 999.0;
      sd[i].qu = 9.0;
      sd[i].qv = 9.0;
    }

  }
} // End of cal_spddir.

void cal_ascen(SOUND_DATA_ARR sd)
/******************************************/
/*                                        */
/*                                        */
/******************************************/
{
  int i = 0;
  float altlast=0, timelast=0;

  sd[0].z_cmp = 999.0;
  sd[0].qz = 9.0;
  while((i<MAX_OB)&&((sd[i].alt == 99999.0)||(sd[i].time == 9999.0))){
    i++;
  }
  if(i<MAX_OB){
    timelast = sd[i].time;
    altlast = sd[i].alt;
  }
  i++;
  while(i<MAX_OB)
  {
    if((sd[i].alt != 99999.0)&&(sd[i].time != 9999.0)){ 
      sd[i].z_cmp = (sd[i].alt-altlast)/(sd[i].time-timelast);
      sd[i].qz = 99.0;
      //printf ("%5.2f %5.2f\n",sd[i].time,sd[i].z_cmp);
      timelast = sd[i].time;
      altlast = sd[i].alt;

      if(sd[i].z_cmp > 99.9)
      {
        sd[i].z_cmp = 99.9;
        sd[i].qz = 4.0;
      }
      if(sd[i].z_cmp < -99.9)
      {
        sd[i].z_cmp = -99.9;
        sd[i].qz = 4.0;
      }
    }
    else
    {
      sd[i].z_cmp = 999.0;
      sd[i].qz = 9.0;
    }
    i++;
  } // End of for loop.
} // End of cal_ascen.


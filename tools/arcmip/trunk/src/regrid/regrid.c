/* findnn.c - find nearest model grid point neighbors to centers of EASE-Grid cells */


#include <stdio.h>
#include <errno.h>
#include <sys/param.h>
#include "arcmip_ease.h"
#include "netcdf.h"



int start_col,start_row,ease_cols,ease_rows,ease_len;
int num_nbors,nbor_len;
int (*nbor_idx)[NUM_NBORS];
double (*nbor_rats)[NUM_NBORS];


int ease_len;
int model_len;
double *var,*easevar;
int varlen,easevarlen;
int mvarlen=0,measevarlen=0;
int easedimidsmap[NC_MAX_VAR_DIMS]; /* easedimidsmap[i] corresponds to model dimid i */
int easevaridsmap[NC_MAX_VAR_DIMS]; /* easedimidsmap[i] corresponds to model dimid i */


double ease_interp();



main(argc,argv)
int argc;
char *argv[];
{
int i;
char *s;
char infnam[MAXPATHLEN],outfnam[MAXPATHLEN];
int ncid,easencid;

if (argc<2) {
  fprintf(stderr,"Usage: regrid file.nc ...\n");
  exit(1);
  }

read_nbors();

for (i=1; i<argc; i++) {
  for (s=argv[i];*s;s++) ;
  s-=3;
  if (strcmp(s,".nc")) {
    fprintf(stderr,"file %d : %s does not end in .nc; skipping.\n",i,argv[i]);
    continue;
    }
  /*fprintf(stderr,"\ndoing file %i %s\n",i,argv[i]);*/
  strcpy(infnam,argv[i]);
  *s='\0';
  strcpy(outfnam,argv[i]);
  strcat(outfnam,"_ease.nc");

  if (nc_open(infnam,0,&ncid) != NC_NOERR) {
   fprintf(stderr,"cannot nc_open model file %s; skipping.\n",infnam);
   continue;
   }
  if (nc_create(outfnam,0,&easencid) != NC_NOERR) {
   fprintf(stderr,"cannot nc_create ease file %s; skipping its model file.\n",outfnam);
   continue;
   }

  setupnc(ncid,easencid);
  regrid_vars(ncid,easencid);

  nc_close(ncid);
  nc_close(easencid);

  }

free(var);
free(easevar);

free(nbor_idx);
free(nbor_rats);
} /* main() */



read_nbors()
{
FILE *fp;
char buf[1024];
int i,j;
int ei,ex,ey;

fp=fopen(NBOR_FRAT,"rb");

/* look for ^L after text string, echo string, & save string to check model name */
for (i=0; (i<1024) && ((buf[i]=fgetc(fp)) != 12) && !feof(fp); fputc(buf[i],stderr), i++) ;
buf[--i]='\0'; /* replace newline with null */
for (;(i>0) && (buf[i]!=' '); i--) ;
i++;
if (strcmp(buf+i,MODEL_NAME))
  fprintf(stderr,"Warning: compiled model name %s != neighbor file name %s.\n",MODEL_NAME,buf+i);

fread(&start_col,sizeof(int),1,fp);
fread(&start_row,sizeof(int),1,fp);
fread(&ease_cols,sizeof(int),1,fp);
fread(&ease_rows,sizeof(int),1,fp);
fread(&num_nbors,sizeof(int),1,fp);
ease_len=ease_cols*ease_rows;
nbor_len=ease_len*num_nbors;
fprintf(stderr," start col=%d row=%d\n num cols=%d rows=%d len=%d\n num neighbors=%d len=%d\n",
 start_col,start_row,ease_cols,ease_rows,ease_len,num_nbors,nbor_len);

if (num_nbors != NUM_NBORS)
 fprintf(stderr,"Warning: neighbor file %s has %d neighbors but code #define is %d.\n",
  NBOR_FRAT,num_nbors,NUM_NBORS);

nbor_idx=(int(*)[NUM_NBORS])malloc(nbor_len*sizeof(int));
nbor_rats=(double(*)[NUM_NBORS])malloc(nbor_len*sizeof(double));
if (!nbor_idx || !nbor_rats) {
 fprintf(stderr,"malloc error %d\n",errno);
 exit(1);
 }
if (fread(nbor_idx,sizeof(int),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor indices\n",errno);
 exit(1);
 }
if (fread(nbor_rats,sizeof(double),nbor_len,fp) < nbor_len) {
 fprintf(stderr,"error %d reading neighbor ratios\n",errno);
 exit(1);
 }

fclose(fp);
} /* read_nbors() */



setupnc(ncid,easencid)
int ncid,easencid;
{
char name[NC_MAX_NAME];
int ndims,nvars,ngatts,unlim;
int varndims,varnatts;
int dimids[NC_MAX_VAR_DIMS],easedimids[NC_MAX_VAR_DIMS]; /* a var's model/ease dimids */
size_t length;
nc_type xtype;
int i,j;
char *s;
double missing_value=MISSING_VAL;

if (nc_inq(ncid,&ndims,&nvars,&ngatts,&unlim) != NC_NOERR) {
  fprintf(stderr,"cannot nc_inq for ncid %d\n",ncid);
  return(1);
  }

/* copy dimensions */
for (i=0,model_len=varlen=easevarlen=1; i<ndims; i++) {
  if (nc_inq_dim(ncid,i,name,&length) != NC_NOERR) {
    fprintf(stderr,"cannot nc_inq_dim for ncid %d dimid %d\n",ncid,i);
    return(1);
    }
  /*fprintf(stderr,"doing dim %d %s\n",i,name);*/
  varlen *= length;
  if ( (strcmp(name,"iew")==0)
    || (strcmp(name,"col")==0)
    || (strcmp(name,"lon")==0) ) { /* replace with new grid info */
    model_len *= length;
    easevarlen *= ease_cols;
    if (nc_def_dim(easencid,"col",ease_cols,&easedimidsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_def_dim for ncid %d iew/col\n",ncid,i);
      return(1);
      }
    }
  else if ( (strcmp(name,"jns")==0)
    || (strcmp(name,"row")==0)
    || (strcmp(name,"lat")==0) ) { /* replace with new grid info */
    model_len *= length;
    easevarlen *= ease_rows;
    if (nc_def_dim(easencid,"row",ease_rows,&easedimidsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_def_dim for ncid %d jns/row\n",ncid,i);
      return(1);
      }
    }
#if 0
  else if (strcmp(name,"npres")==0) { /* replace with new grid info */
    easevarlen *= length;
    if (nc_def_dim(easencid,"pres",length,&easedimidsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_def_dim for ncid %d pres\n",ncid,i);
      return(1);
      }
    }
  else if (strcmp(name,"thp")==0) { /* replace with new grid info */
    easevarlen *= length;
    if (nc_def_dim(easencid,"hour",length,&easedimidsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_def_dim for ncid %d thp/hour\n",ncid,i);
      return(1);
      }
    }
#endif
  else { /* simple copy */
    easevarlen *= length;
    if (nc_def_dim(easencid,name,length,&easedimidsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_def_dim for ncid %d dimid %d %s\n",ncid,i,name);
      return(1);
      }
    }
  }

if (model_len != (MODEL_ROWS * MODEL_COLS))
  fprintf(stderr,"Warning: netcdf calculated model length %d != .h defines %d.\n",
  model_len,(MODEL_ROWS * MODEL_COLS));

if (varlen > mvarlen) {
  if (var) free(var);
  var = (double*)malloc(sizeof(double)*varlen);
  mvarlen = varlen;
  }
if (easevarlen > measevarlen) {
  if (easevar) free(easevar);
  easevar = (double*)malloc(sizeof(double)*easevarlen);
  measevarlen = easevarlen;
  }
if (!var || !easevar) {
  fprintf(stderr,"Cannot malloc var/easevar.\n");
  exit(1);
  }

/* copy var defs */
for (i=0; i<nvars; i++) {
  if (nc_inq_var(ncid,i,name,&xtype,&varndims,dimids,&varnatts) != NC_NOERR) {
    fprintf(stderr,"cannot nc_inq_var for ncid %d varid %d\n",ncid,i);
    return(1);
    }
  /*fprintf(stderr,"doing var %d %s\n",i,name);*/
  for (j=0; j<varndims; j++) {
    easedimids[j]=easedimidsmap[dimids[j]];
    }
  if (nc_def_var(easencid,name,xtype,varndims,easedimids,&easevaridsmap[i]) != NC_NOERR) {
    fprintf(stderr,"cannot nc_def_var for ncid %d varid %d %s\n",ncid,i,name);
    return(1);
    }
  /* var atts */
  for (j=0; j<varnatts; j++) {
    if (nc_inq_attname(ncid,i,j,name) != NC_NOERR) {
      fprintf(stderr,"cannot nc_inq_attname for ncid %d varid %d attid %d\n",ncid,i,j);
      return(1);
      }
    /*fprintf(stderr," doing var %d att %d %s\n",i,j,name);*/
    if (nc_copy_att(ncid,i,name,easencid,easevaridsmap[i]) != NC_NOERR) {
      fprintf(stderr,"cannot nc_copy_att for ncid %d varid %d attid %d %s\n",ncid,i,j,name);
      return(1);
      }
    }
  if (varndims>1)
    if (nc_put_att_double(easencid,easevaridsmap[i],"missing_value",NC_DOUBLE,1,&missing_value)
        != NC_NOERR) {
      fprintf(stderr,"cannot add missing_value att for ncid %d varid %d\n",ncid,i);
      return(1);
      }
  }

/* copy global atts */
for (i=0; i<ngatts; i++) {
  if (nc_inq_attname(ncid,NC_GLOBAL,i,name) != NC_NOERR) {
    fprintf(stderr,"cannot nc_inq_attname for ncid %d global attid %d\n",ncid,i);
    return(1);
    }
  /*fprintf(stderr,"doing gatt %d %s\n",i,name);*/
  /* replace some, else just copy */
  if (strcmp(name,"ew_index")==0) {
    s="col = index of grid points in x direction";
    j=nc_put_att_text(easencid,NC_GLOBAL,"x_index",strlen(s),s);
    if (j!=NC_NOERR) fprintf(stderr,"nc_put_att_text x_index error: %s\n",nc_strerror(i));
    }
  else if (strcmp(name,"ns_index")==0) {
    s="row = index of grid points in y direction";
    j=nc_put_att_text(easencid,NC_GLOBAL,"y_index",strlen(s),s);
    if (j!=NC_NOERR) fprintf(stderr,"nc_put_att_text y_index error: %s\n",nc_strerror(i));
    }
  else if (strcmp(name,"vertical_index")==0) {
    /* s="pres = index of constant pressure levels (z direction)"; */
    s="npres = index of constant pressure levels (z direction)";
    j=nc_put_att_text(easencid,NC_GLOBAL,"vertical_index",strlen(s),s);
    if (j!=NC_NOERR) fprintf(stderr,"nc_put_att_text vertical_index error: %s\n",nc_strerror(i));
    }
  else if (strcmp(name,"time_index")==0) {
    /* s="hour = index number of the start of each 3-hour period"; */
    s="thp = index number of the start of each 3-hour period";
    j=nc_put_att_text(easencid,NC_GLOBAL,"time_index",strlen(s),s);
    if (j!=NC_NOERR) fprintf(stderr,"nc_put_att_text time_index error: %s\n",nc_strerror(i));
    }
  else if (nc_copy_att(ncid,NC_GLOBAL,name,easencid,NC_GLOBAL) != NC_NOERR) {
    fprintf(stderr,"cannot nc_copy_att for ncid %d global attid %d %s\n",ncid,i,name);
    return(1);
    }
  }

/* new global atts */
s="Prepared by UCAR/JOSS, 2003. http://www.joss.ucar.edu/arcmip/regrid/  codiac@joss.ucar.edu";
i=nc_put_att_text(easencid,NC_GLOBAL,"joss_comment0",strlen(s),s);
if (i!=NC_NOERR) fprintf(stderr,"nc_put_att_text joss_comment0 error: %s\n",nc_strerror(i));
s="Dataset re-gridded to ARCMIP 50km EASE-Grid. See URL or send email for details.";
i=nc_put_att_text(easencid,NC_GLOBAL,"joss_comment1",strlen(s),s);
if (i!=NC_NOERR) fprintf(stderr,"nc_put_att_text joss_comment1 error: %s\n",nc_strerror(i));

if (nc_enddef(easencid) != NC_NOERR) {
  fprintf(stderr,"cannot nc_enddef\n");
  return(1);
  }
} /* setupnc() */



regrid_vars(ncid,easencid)
int ncid; /* model nc file (read) */
int easencid; /* ease nc file (write) */
{
char name[NC_MAX_NAME];
int ndims;
int dimids[NC_MAX_VAR_DIMS];
size_t dimlens[NC_MAX_VAR_DIMS];
int i,j,k,varid,numval,thisvarlen,numvars,exact_match;
double num,den;

if (nc_inq_nvars(ncid,&numvars) != NC_NOERR) {
 fprintf(stderr,"cannot read info for numvars of model file\n");
 exit(1);
 }

/* regrid each var in the model */
for (varid=0; varid<numvars; varid++) {

/* get var's info, esp #dimensions */
if (nc_inq_var(ncid, varid, name, 0, &ndims, dimids, 0) != NC_NOERR) {
 fprintf(stderr,"cannot read info for var %d\n",varid);
 exit(1);
 }
/*fprintf(stderr,"regridding var id %d %s\n",varid,name);*/
for (i=0, j=1; i<ndims; i++) {
 if (nc_inq_dimlen(ncid,dimids[i],&dimlens[i]) != NC_NOERR) {
  fprintf(stderr,"cannot get dim %d length for dim %d of var %d (%s)\n",dimids[i],i,varid,name);
  exit(1);
  }
 j*=dimlens[i];
 }
if (varlen<j) {
  fprintf(stderr,"var %d (%s) has len %d > prealloc len %d\n",
    varid,name,j,varlen);
  exit(1);
  }
thisvarlen=j;

/* "simple" var, just echo it */
if (ndims < 2) {
  if (copy_varnc(ncid,varid,easencid,easevaridsmap[varid]) != 0) {
    fprintf(stderr,"cannot copy var %d %s\n",varid,name);
    exit(1);
    }
  continue;
  }

/* how many values per grid cell in the model var ? */
numval = thisvarlen / model_len;
if (thisvarlen % model_len) {
  fprintf(stderr,"var %d (%s) has len %d : not multiple of model len %d\n",
    varid,name,thisvarlen,model_len);
  exit(1);
  }

j = ease_len * numval;
if (easevarlen < j) {
  fprintf(stderr,"ease var array needs len %d > prealloc len %d\n",
    j,easevarlen);
  exit(1);
  }

/* get the model var */
if (nc_get_var_double(ncid,varid,var) != NC_NOERR) {
  fprintf(stderr,"cannot read data for var %d (%s)\n",varid,name);
  exit(1);
  }

for (i=0,exact_match=-1; i<ease_len; i++,exact_match=-1) { /* for each ease grid cell */
  for (j=0; j<numval; j++) { /* for each value (level,time,etc) */
    for (num=den=0.0, k=0; k<num_nbors; k++) {
     if (nbor_rats[i][k] < 0.0)
       exact_match = k;
     else {
       num += var[nbor_idx[i][k] + j*model_len] * nbor_rats[i][k];
       den += nbor_rats[i][k];
       }
     }
    if (den == 0.0) /* all ratios are 0.0, i.e. missing */
     easevar[i+j*ease_len] = MISSING_VAL;
    else if (exact_match >= 0) /* same pont */
     easevar[i+j*ease_len] = var[nbor_idx[i][exact_match] + j*model_len];
    else easevar[i+j*ease_len] = num / den; /* use avg */
    }
  }

if (nc_put_var_double(easencid,varid,easevar) != NC_NOERR) {
 fprintf(stderr,"cannot write data for var %d (%s)\n",varid,name);
 exit(1);
 }

} /* for varid */
} /* regrid_vars() */



int copy_varnc(ncid,varid,easencid,easevarid)
int ncid,varid,easencid,easevarid;
/* there is a secret nc_copy_var/ncvarcpy but it doesn't allow specifying the output varid */
{
nc_type xtype;

nc_inq_vartype(ncid,varid,&xtype);

switch (xtype) {
case NC_BYTE:
  if (nc_get_var_uchar(ncid,varid,(unsigned char *)var) != NC_NOERR ||
      nc_put_var_uchar(easencid,easevarid,(unsigned char *)var) != NC_NOERR)
    return(1);
  break;
case NC_CHAR:
  if (nc_get_var_uchar(ncid,varid,(unsigned char *)var) != NC_NOERR ||
      nc_put_var_uchar(easencid,easevarid,(unsigned char *)var) != NC_NOERR)
    return(1);
  break;
case NC_SHORT:
  if (nc_get_var_short(ncid,varid,(short*)var) != NC_NOERR ||
      nc_put_var_short(easencid,easevarid,(short*)var) != NC_NOERR)
    return(1);
  break;
case NC_INT:
  if (nc_get_var_int(ncid,varid,(int*)var) != NC_NOERR ||
      nc_put_var_int(easencid,easevarid,(int*)var) != NC_NOERR)
    return(1);
  break;
case NC_FLOAT:
  if (nc_get_var_float(ncid,varid,(float*)var) != NC_NOERR ||
      nc_put_var_float(easencid,easevarid,(float*)var) != NC_NOERR)
    return(1);
  break;
case NC_DOUBLE:
  if (nc_get_var_double(ncid,varid,var) != NC_NOERR ||
      nc_put_var_double(easencid,easevarid,var) != NC_NOERR)
    return(1);
  break;
default:
  return(1);
  break;
}

return(0);
} /* copy_varnc() */

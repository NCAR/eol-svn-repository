# line 1 "templ.f"
#include <f90.h>
# line 4 "templ.f"
void ll2xydrv_(plat_,plon_,x_,y_,orlat_,orlon_,angxax_)
     Real *plat_;
     Real *plon_;
     Real *x_;
     Real *y_;
     Real *orlat_;
     Real *orlon_;
     Real *angxax_;
{
Real tmp1;
Real tmp2;
Real tmp3;
auto Real swlat_;
auto Real swlon_;
extern void ll2xy_();
auto Integer icross_;
auto Real x1_;
auto Real y1_;
auto Real x2_;
auto Integer inhem_;
auto Integer iwhem_;
auto Real deglat_;
auto Real theta_;
auto Real deglon_;
auto Real xt_;
auto Real y2_;
auto Real yt_;
static char FMT_10[] = "(/,5X,'+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER ','+++')";
# line 4 "templ.f"
# line 38 "templ.f"
# line 40 "templ.f"
icross_ = 0;
# line 44 "templ.f"
if ((Real)sign_r((tmp2 = (float) 1.00000000000000000e+00 , &tmp2),& *orlat_)>(Real)sign_r((tmp3 = (float) 1.00000000000000000e+00 , &tmp3),& *plat_)) {
# line 44 "templ.f"
icross_ = 1;
# line 44 "templ.f"
}
# line 45 "templ.f"
if ((Real)sign_r((tmp2 = (float) 1.00000000000000000e+00 , &tmp2),& *orlat_)<(Real)sign_r((tmp3 = (float) 1.00000000000000000e+00 , &tmp3),& *plat_)) {
# line 45 "templ.f"
icross_ = 2;
# line 45 "templ.f"
}
# line 46 "templ.f"
if ((Real)sign_r((tmp2 = (float) 1.00000000000000000e+00 , &tmp2),& *orlon_)>(Real)sign_r((tmp3 = (float) 1.00000000000000000e+00 , &tmp3),& *plon_)) {
# line 47 "templ.f"
if (icross_!=0) {
# line 48 "templ.f"
f90_write(6,FMT_10,-1,(char*)0,0,0);
# line 48 "templ.f"
f90_endwrite();
# line 49 "templ.f"
# line 51 "templ.f"
stop((char*)0);
# line 52 "templ.f"
} else {
# line 53 "templ.f"
icross_ = 3;
# line 54 "templ.f"
}
# line 55 "templ.f"
} else if ((Real)sign_r((tmp2 = (float) 1.00000000000000000e+00 , &tmp2),& *orlon_)<(Real)sign_r((tmp3 = (float) 1.00000000000000000e+00 , &tmp3),& *plon_)) {
# line 56 "templ.f"
if (icross_!=0) {
# line 57 "templ.f"
f90_write(6,FMT_10,-1,(char*)0,0,0);
# line 57 "templ.f"
f90_endwrite();
# line 58 "templ.f"
stop((char*)0);
# line 59 "templ.f"
} else {
# line 60 "templ.f"
icross_ = 4;
# line 61 "templ.f"
}
# line 62 "templ.f"
}
# line 64 "templ.f"
if ( *orlat_>(float) 0.00000000000000000e+00) {
# line 65 "templ.f"
inhem_ = 1;
# line 66 "templ.f"
} else {
# line 67 "templ.f"
inhem_ = 0;
# line 68 "templ.f"
}
# line 69 "templ.f"
if ( *orlon_>(float) 0.00000000000000000e+00) {
# line 70 "templ.f"
iwhem_ = 1;
# line 71 "templ.f"
} else {
# line 72 "templ.f"
iwhem_ = 0;
# line 73 "templ.f"
}
# line 74 "templ.f"
if (icross_==0) {
# line 82 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 83 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 84 "templ.f"
swlat_ = ( *orlat_>=(float)0 ? ( *orlat_) : -(( *orlat_)));
# line 85 "templ.f"
swlon_ = ( *orlon_>=(float)0 ? ( *orlon_) : -(( *orlon_)));
# line 87 "templ.f"
ll2xy_(&deglat_,&deglon_,x_,y_,&swlat_,&swlon_);
# line 92 "templ.f"
if (inhem_==0) {
# line 92 "templ.f"
 *y_ = -( *y_);
# line 92 "templ.f"
}
# line 93 "templ.f"
if (iwhem_==0) {
# line 93 "templ.f"
 *x_ = -( *x_);
# line 93 "templ.f"
}
# line 95 "templ.f"
} else if (icross_==1) {
# line 100 "templ.f"
deglat_ = (float) 9.99999974737875164e-05;
# line 101 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 102 "templ.f"
swlat_ = ( *orlat_>=(float)0 ? ( *orlat_) : -(( *orlat_)));
# line 103 "templ.f"
swlon_ = ( *orlon_>=(float)0 ? ( *orlon_) : -(( *orlon_)));
# line 105 "templ.f"
ll2xy_(&deglat_,&deglon_,&x1_,&y1_,&swlat_,&swlon_);
# line 107 "templ.f"
if (iwhem_==0) {
# line 107 "templ.f"
x1_ = -(x1_);
# line 107 "templ.f"
}
# line 109 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 110 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 111 "templ.f"
swlat_ = (float) 9.99999974737875164e-05;
# line 112 "templ.f"
swlon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 113 "templ.f"
ll2xy_(&deglat_,&deglon_,&x2_,&y2_,&swlat_,&swlon_);
# line 115 "templ.f"
 *y_ = -(((y1_>=(float)0 ? y1_ : -(y1_)) + (y2_>=(float)0 ? y2_ : -(y2_))));
# line 116 "templ.f"
 *x_ = x1_;
# line 118 "templ.f"
} else if (icross_==2) {
# line 122 "templ.f"
deglat_ = (float) 9.99999974737875164e-05;
# line 123 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 124 "templ.f"
swlat_ = ( *orlat_>=(float)0 ? ( *orlat_) : -(( *orlat_)));
# line 125 "templ.f"
swlon_ = ( *orlon_>=(float)0 ? ( *orlon_) : -(( *orlon_)));
# line 127 "templ.f"
ll2xy_(&deglat_,&deglon_,&x1_,&y1_,&swlat_,&swlon_);
# line 129 "templ.f"
if (iwhem_==0) {
# line 129 "templ.f"
x1_ = -(x1_);
# line 129 "templ.f"
}
# line 131 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 132 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 133 "templ.f"
swlat_ = (float) 9.99999974737875164e-05;
# line 134 "templ.f"
swlon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 135 "templ.f"
ll2xy_(&deglat_,&deglon_,&x2_,&y2_,&swlat_,&swlon_);
# line 137 "templ.f"
 *y_ = ((y1_>=(float)0 ? y1_ : -(y1_)) + (y2_>=(float)0 ? y2_ : -(y2_)));
# line 138 "templ.f"
 *x_ = x1_;
# line 140 "templ.f"
} else if (icross_==3) {
# line 144 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 145 "templ.f"
if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))<=(float) 4.50000000000000000e+01) {
# line 146 "templ.f"
deglon_ = (float) 9.99999974737875164e-05;
# line 147 "templ.f"
} else if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))>=(float) 1.35000000000000000e+02) {
# line 148 "templ.f"
deglon_ = (float) 1.79999893188476562e+02;
# line 149 "templ.f"
}
# line 150 "templ.f"
swlat_ = ( *orlat_>=(float)0 ? ( *orlat_) : -(( *orlat_)));
# line 151 "templ.f"
swlon_ = ( *orlon_>=(float)0 ? ( *orlon_) : -(( *orlon_)));
# line 153 "templ.f"
ll2xy_(&deglat_,&deglon_,&x1_,&y1_,&swlat_,&swlon_);
# line 155 "templ.f"
if (inhem_==0) {
# line 155 "templ.f"
y1_ = -(y1_);
# line 155 "templ.f"
}
# line 157 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 158 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 159 "templ.f"
swlat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 160 "templ.f"
if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))<=(float) 4.50000000000000000e+01) {
# line 161 "templ.f"
swlon_ = (float) 9.99999974737875164e-05;
# line 162 "templ.f"
} else if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))>=(float) 1.35000000000000000e+02) {
# line 163 "templ.f"
swlon_ = (float) 1.79999893188476562e+02;
# line 164 "templ.f"
}
# line 166 "templ.f"
ll2xy_(&deglat_,&deglon_,&x2_,&y2_,&swlat_,&swlon_);
# line 168 "templ.f"
 *y_ = y1_;
# line 169 "templ.f"
 *x_ = ((x1_>=(float)0 ? x1_ : -(x1_)) + (x2_>=(float)0 ? x2_ : -(x2_)));
# line 171 "templ.f"
} else if (icross_==4) {
# line 175 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 176 "templ.f"
if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))<=(float) 4.50000000000000000e+01) {
# line 177 "templ.f"
deglon_ = (float) 9.99999974737875164e-05;
# line 178 "templ.f"
} else if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))>=(float) 1.35000000000000000e+02) {
# line 179 "templ.f"
deglon_ = (float) 1.79999893188476562e+02;
# line 180 "templ.f"
}
# line 181 "templ.f"
swlat_ = ( *orlat_>=(float)0 ? ( *orlat_) : -(( *orlat_)));
# line 182 "templ.f"
swlon_ = ( *orlon_>=(float)0 ? ( *orlon_) : -(( *orlon_)));
# line 184 "templ.f"
ll2xy_(&deglat_,&deglon_,&x1_,&y1_,&swlat_,&swlon_);
# line 186 "templ.f"
if (inhem_==0) {
# line 186 "templ.f"
y1_ = -(y1_);
# line 186 "templ.f"
}
# line 188 "templ.f"
deglat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 189 "templ.f"
deglon_ = ( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)));
# line 190 "templ.f"
swlat_ = ( *plat_>=(float)0 ? ( *plat_) : -(( *plat_)));
# line 191 "templ.f"
if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))<=(float) 4.50000000000000000e+01) {
# line 192 "templ.f"
swlon_ = (float) 9.99999974737875164e-05;
# line 193 "templ.f"
} else if (( *plon_>=(float)0 ? ( *plon_) : -(( *plon_)))>=(float) 1.35000000000000000e+02) {
# line 194 "templ.f"
swlon_ = (float) 1.79999893188476562e+02;
# line 195 "templ.f"
}
# line 197 "templ.f"
ll2xy_(&deglat_,&deglon_,&x2_,&y2_,&swlat_,&swlon_);
# line 199 "templ.f"
 *y_ = y1_;
# line 200 "templ.f"
 *x_ = -(((x1_>=(float)0 ? x1_ : -(x1_)) + (x2_>=(float)0 ? x2_ : -(x2_))));
# line 202 "templ.f"
}
# line 207 "templ.f"
if ( *angxax_!=(float) 9.00000000000000000e+01) {
# line 208 "templ.f"
theta_ = ( *angxax_ - (float) 9.00000000000000000e+01)*(float) 1.74532905220985413e-02;
# line 209 "templ.f"
xt_ =  *x_;
# line 210 "templ.f"
yt_ =  *y_;
# line 211 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&theta_));
# line 211 "templ.f"
ASSIGNFLOAT(tmp3,r_sin_(&theta_));
# line 211 "templ.f"
 *x_ = xt_*tmp2 - yt_*tmp3;
# line 212 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&theta_));
# line 212 "templ.f"
ASSIGNFLOAT(tmp3,r_cos_(&theta_));
# line 212 "templ.f"
 *y_ = xt_*tmp2 + yt_*tmp3;
# line 213 "templ.f"
}
# line 216 "templ.f"
return ;
# line 218 "templ.f"
}
# line 222 "templ.f"
void ll2xy_(deglat_,deglon_,x_,y_,swlat_,swlon_)
     Real *deglat_;
     Real *deglon_;
     Real *x_;
     Real *y_;
     Real *swlat_;
     Real *swlon_;
{
Real tmp1;
Real tmp2;
Real tmp3;
Real tmp4;
Real tmp5;
auto Real cosn_;
auto Real cblat_;
auto Real sblat_;
auto Real dlon_;
auto Real s_;
auto Real dlat_;
auto Real cdlon_;
auto Real alat_;
auto Real aza_;
auto Real sinn_;
auto Real calat_;
auto Real azb_;
auto Real salat_;
auto Real along_;
auto Real blat_;
auto Real blong_;
# line 222 "templ.f"
# line 229 "templ.f"
# line 230 "templ.f"
# line 231 "templ.f"
# line 234 "templ.f"
# line 236 "templ.f"
 *x_ = (float) 0.00000000000000000e+00;
# line 237 "templ.f"
 *y_ = (float) 0.00000000000000000e+00;
# line 238 "templ.f"
alat_ =  *swlat_*(float) 1.74532905220985413e-02;
# line 239 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&alat_));
# line 239 "templ.f"
calat_ = tmp2;
# line 240 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&alat_));
# line 240 "templ.f"
salat_ = tmp2;
# line 241 "templ.f"
tmp2 =  *swlon_*(float) 1.74532905220985413e-02;
# line 241 "templ.f"
along_ = (tmp2>=(float)0 ? tmp2 : -(tmp2));
# line 242 "templ.f"
blat_ =  *deglat_*(float) 1.74532905220985413e-02;
# line 243 "templ.f"
blong_ = ( *deglon_>=(float)0 ? ( *deglon_) : -(( *deglon_)))*(float) 1.74532905220985413e-02;
# line 244 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&blat_));
# line 244 "templ.f"
cblat_ = tmp2;
# line 245 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&blat_));
# line 245 "templ.f"
sblat_ = tmp2;
# line 246 "templ.f"
dlon_ = along_ - blong_;
# line 247 "templ.f"
tmp2 =  *deglat_ -  *swlat_;
# line 247 "templ.f"
dlat_ = (tmp2>=(float)0 ? tmp2 : -(tmp2))*(float) 1.74532905220985413e-02;
# line 248 "templ.f"
if (dlat_<(float) 9.99999974737875164e-05 && (dlon_>=(float)0 ? dlon_ : -(dlon_))<(float) 9.99999974737875164e-05) {
# line 248 "templ.f"
goto L_90;
# line 248 "templ.f"
}
# line 249 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&dlon_));
# line 249 "templ.f"
cdlon_ = tmp2;
# line 250 "templ.f"
ASSIGNFLOAT(tmp3,r_sin_(&dlon_));
# line 250 "templ.f"
ASSIGNFLOAT(tmp4,r_atan_((tmp2 = tmp3/(calat_*sblat_/cblat_ - salat_*cdlon_) , &tmp2)));
# line 250 "templ.f"
aza_ = tmp4;
# line 251 "templ.f"
ASSIGNFLOAT(tmp4,r_sin_((tmp3 = -(dlon_) , &tmp3)));
# line 251 "templ.f"
ASSIGNFLOAT(tmp5,r_atan_((tmp2 = tmp4/(cblat_*salat_/calat_ - sblat_*cdlon_) , &tmp2)));
# line 251 "templ.f"
azb_ = tmp5;
# line 255 "templ.f"
if (blat_<alat_) {
# line 255 "templ.f"
aza_ = aza_ + (float) 3.14159274101257324e+00;
# line 255 "templ.f"
}
# line 256 "templ.f"
if (alat_<blat_) {
# line 256 "templ.f"
azb_ = azb_ + (float) 3.14159274101257324e+00;
# line 256 "templ.f"
}
# line 257 "templ.f"
if (aza_<0) {
# line 257 "templ.f"
aza_ = aza_ + (float) 6.28318548202514648e+00;
# line 257 "templ.f"
}
# line 258 "templ.f"
if (azb_<0) {
# line 258 "templ.f"
azb_ = azb_ + (float) 6.28318548202514648e+00;
# line 258 "templ.f"
}
# line 260 "templ.f"
sinn_ = dlat_;
# line 261 "templ.f"
if (dlon_!=(float) 0.00000000000000000e+00) {
# line 262 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&dlon_));
# line 262 "templ.f"
ASSIGNFLOAT(tmp4,r_sin_((tmp3 = (float) 1.57079637050628662e+00 - blat_ , &tmp3)));
# line 262 "templ.f"
ASSIGNFLOAT(tmp5,r_sin_(&aza_));
# line 262 "templ.f"
sinn_ = tmp2*tmp4/tmp5;
# line 262 "templ.f"
}
# line 264 "templ.f"
cosn_ = salat_*sblat_ + calat_*cblat_*cdlon_;
# line 265 "templ.f"
ASSIGNFLOAT(tmp3,r_atan_((tmp2 = sinn_/cosn_ , &tmp2)));
# line 265 "templ.f"
s_ = (float) 6.38012011718750000e+03*tmp3;
# line 266 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&aza_));
# line 266 "templ.f"
 *x_ = s_*tmp2;
# line 267 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&aza_));
# line 267 "templ.f"
 *y_ = s_*tmp2;
L_90:
# line 269 "templ.f"
;
# line 270 "templ.f"
return ;
# line 272 "templ.f"
}
# line 276 "templ.f"
void xy2lldrv_(plat_,plon_,xp_,yp_,orlat_,orlon_,angxax_)
     Real *plat_;
     Real *plon_;
     Real *xp_;
     Real *yp_;
     Real *orlat_;
     Real *orlon_;
     Real *angxax_;
{
Real tmp1;
Real tmp2;
Real tmp3;
auto Real y3_;
extern void ll2xydrv_();
auto Integer icross_;
auto Real x3_;
auto Real x2_;
auto Real x_;
auto Real y_;
auto Real orlon2_;
extern void xy2ll_();
auto Real xnew_;
auto Real ynew_;
auto Real theta_;
auto Real orlat2_;
auto Real xt_;
auto Real y2_;
auto Real yt_;
static char FMT_10[] = "(/,5X,'+++XY2LL ROUTINE CANNOT HANDLE CASE',' WHERE POINTS CROSS A POLE+++')";
static char FMT_20[] = "(/,5X,'+++ XY2LL CANNOT HANDLE DUAL HEMISPHERE',' CROSSOVER ','+++')";
# line 276 "templ.f"
# line 308 "templ.f"
# line 310 "templ.f"
x_ =  *xp_;
# line 311 "templ.f"
y_ =  *yp_;
# line 315 "templ.f"
if ( *angxax_!=(float) 9.00000000000000000e+01) {
# line 316 "templ.f"
theta_ = ((float) 9.00000000000000000e+01 -  *angxax_)*(float) 1.74532905220985413e-02;
# line 317 "templ.f"
xt_ = x_;
# line 318 "templ.f"
yt_ = y_;
# line 319 "templ.f"
ASSIGNFLOAT(tmp2,r_cos_(&theta_));
# line 319 "templ.f"
ASSIGNFLOAT(tmp3,r_sin_(&theta_));
# line 319 "templ.f"
x_ = xt_*tmp2 - yt_*tmp3;
# line 320 "templ.f"
ASSIGNFLOAT(tmp2,r_sin_(&theta_));
# line 320 "templ.f"
ASSIGNFLOAT(tmp3,r_cos_(&theta_));
# line 320 "templ.f"
y_ = xt_*tmp2 + yt_*tmp3;
# line 321 "templ.f"
}
# line 327 "templ.f"
icross_ = 0;
# line 328 "templ.f"
if ( *orlon_>(float) 0.00000000000000000e+00) {
# line 329 "templ.f"
if (x_>(float) 0.00000000000000000e+00 &&  *orlon_<(float) 9.00000000000000000e+01) {
# line 330 "templ.f"
ll2xydrv_(orlat_,(tmp1 = (float) 9.99999974737875164e-05 , &tmp1),&x2_,&y2_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 331 "templ.f"
if (x2_<x_) {
# line 331 "templ.f"
icross_ = 1;
# line 331 "templ.f"
}
# line 332 "templ.f"
} else if (x_<(float) 0.00000000000000000e+00 &&  *orlon_>(float) 9.00000000000000000e+01) {
# line 333 "templ.f"
ll2xydrv_(orlat_,(tmp1 = (float) 1.79999893188476562e+02 , &tmp1),&x2_,&y2_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 334 "templ.f"
if (x2_>x_) {
# line 334 "templ.f"
icross_ = 2;
# line 334 "templ.f"
}
# line 335 "templ.f"
}
# line 336 "templ.f"
} else if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 337 "templ.f"
if (x_>(float) 0.00000000000000000e+00 &&  *orlon_<(float)-9.00000000000000000e+01) {
# line 338 "templ.f"
ll2xydrv_(orlat_,(tmp1 = (float)-1.79999893188476562e+02 , &tmp1),&x2_,&y2_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 339 "templ.f"
if (x2_<x_) {
# line 339 "templ.f"
icross_ = 3;
# line 339 "templ.f"
}
# line 340 "templ.f"
} else if (x_<(float) 0.00000000000000000e+00 &&  *orlon_>(float)-9.00000000000000000e+01) {
# line 341 "templ.f"
ll2xydrv_(orlat_,(tmp1 = (float)-9.99999974737875164e-05 , &tmp1),&x2_,&y2_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 342 "templ.f"
if (x2_>x_) {
# line 342 "templ.f"
icross_ = 4;
# line 342 "templ.f"
}
# line 343 "templ.f"
}
# line 344 "templ.f"
}
# line 346 "templ.f"
if ( *orlat_>(float) 0.00000000000000000e+00) {
# line 347 "templ.f"
if (y_>(float) 0.00000000000000000e+00) {
# line 348 "templ.f"
ll2xydrv_((tmp1 = (float) 8.99999008178710938e+01 , &tmp1),orlon_,&x3_,&y3_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 349 "templ.f"
if (y3_<y_) {
# line 350 "templ.f"
f90_write(6,FMT_10,-1,(char*)0,0,0);
# line 350 "templ.f"
f90_endwrite();
# line 351 "templ.f"
# line 353 "templ.f"
stop((char*)0);
# line 354 "templ.f"
}
# line 355 "templ.f"
} else {
# line 356 "templ.f"
ll2xydrv_((tmp1 = (float) 9.99999974737875164e-05 , &tmp1),orlon_,&x3_,&y3_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 357 "templ.f"
if (y3_>y_) {
# line 358 "templ.f"
if (icross_==0) {
# line 359 "templ.f"
icross_ = 5;
# line 360 "templ.f"
} else {
# line 361 "templ.f"
f90_write(6,FMT_20,-1,(char*)0,0,0);
# line 361 "templ.f"
f90_endwrite();
# line 362 "templ.f"
# line 364 "templ.f"
stop((char*)0);
# line 365 "templ.f"
}
# line 366 "templ.f"
}
# line 367 "templ.f"
}
# line 368 "templ.f"
} else if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 369 "templ.f"
if (y_>(float) 0.00000000000000000e+00) {
# line 370 "templ.f"
ll2xydrv_((tmp1 = (float)-9.99999974737875164e-05 , &tmp1),orlon_,&x3_,&y3_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 371 "templ.f"
if (y3_<y_) {
# line 372 "templ.f"
if (icross_==0) {
# line 373 "templ.f"
icross_ = 6;
# line 374 "templ.f"
} else {
# line 375 "templ.f"
f90_write(6,FMT_20,-1,(char*)0,0,0);
# line 375 "templ.f"
f90_endwrite();
# line 376 "templ.f"
stop((char*)0);
# line 377 "templ.f"
}
# line 378 "templ.f"
}
# line 379 "templ.f"
} else if (y_<(float) 0.00000000000000000e+00) {
# line 380 "templ.f"
ll2xydrv_((tmp1 = (float)-8.99999008178710938e+01 , &tmp1),orlon_,&x3_,&y3_,orlat_,orlon_,(tmp2 = (float) 9.00000000000000000e+01 , &tmp2));
# line 381 "templ.f"
if (y3_>y_) {
# line 382 "templ.f"
f90_write(6,FMT_10,-1,(char*)0,0,0);
# line 382 "templ.f"
f90_endwrite();
# line 383 "templ.f"
stop((char*)0);
# line 384 "templ.f"
}
# line 385 "templ.f"
}
# line 386 "templ.f"
}
# line 391 "templ.f"
if (icross_==0) {
# line 395 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 396 "templ.f"
orlon2_ = -( *orlon_);
# line 397 "templ.f"
x2_ = -(x_);
# line 398 "templ.f"
} else {
# line 399 "templ.f"
orlon2_ =  *orlon_;
# line 400 "templ.f"
x2_ = x_;
# line 401 "templ.f"
}
# line 402 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 403 "templ.f"
orlat2_ = -( *orlat_);
# line 404 "templ.f"
y2_ = -(y_);
# line 405 "templ.f"
} else {
# line 406 "templ.f"
orlat2_ =  *orlat_;
# line 407 "templ.f"
y2_ = y_;
# line 408 "templ.f"
}
# line 409 "templ.f"
xy2ll_(plat_,plon_,&x2_,&y2_,&orlat2_,&orlon2_);
# line 410 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 410 "templ.f"
 *plon_ = -( *plon_);
# line 410 "templ.f"
}
# line 411 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 411 "templ.f"
 *plat_ = -( *plat_);
# line 411 "templ.f"
}
# line 412 "templ.f"
} else if (icross_==1) {
# line 416 "templ.f"
xnew_ = x_ - x2_;
# line 417 "templ.f"
ynew_ = y_ - y2_;
# line 419 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 420 "templ.f"
orlat2_ = -( *orlat_);
# line 421 "templ.f"
y2_ = -(ynew_);
# line 422 "templ.f"
} else {
# line 423 "templ.f"
orlat2_ =  *orlat_;
# line 424 "templ.f"
y2_ = ynew_;
# line 425 "templ.f"
}
# line 426 "templ.f"
orlon2_ = (float) 9.99999974737875164e-05;
# line 427 "templ.f"
x2_ = -(xnew_);
# line 429 "templ.f"
xy2ll_(plat_,plon_,&x2_,&y2_,&orlat2_,&orlon2_);
# line 430 "templ.f"
 *plon_ = -( *plon_);
# line 431 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 431 "templ.f"
 *plat_ = -( *plat_);
# line 431 "templ.f"
}
# line 432 "templ.f"
} else if (icross_==2) {
# line 436 "templ.f"
xnew_ = x_ - x2_;
# line 437 "templ.f"
ynew_ = y_ - y2_;
# line 439 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 440 "templ.f"
orlat2_ = -( *orlat_);
# line 441 "templ.f"
y2_ = -(ynew_);
# line 442 "templ.f"
} else {
# line 443 "templ.f"
orlat2_ =  *orlat_;
# line 444 "templ.f"
y2_ = ynew_;
# line 445 "templ.f"
}
# line 446 "templ.f"
orlon2_ = (float) 1.79999893188476562e+02;
# line 447 "templ.f"
x2_ = -(xnew_);
# line 449 "templ.f"
xy2ll_(plat_,plon_,&x2_,&y2_,&orlat2_,&orlon2_);
# line 450 "templ.f"
 *plon_ = -( *plon_);
# line 451 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 451 "templ.f"
 *plat_ = -( *plat_);
# line 451 "templ.f"
}
# line 452 "templ.f"
} else if (icross_==3) {
# line 456 "templ.f"
xnew_ = x_ - x2_;
# line 457 "templ.f"
ynew_ = y_ - y2_;
# line 459 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 460 "templ.f"
orlat2_ = -( *orlat_);
# line 461 "templ.f"
y2_ = -(ynew_);
# line 462 "templ.f"
} else {
# line 463 "templ.f"
orlat2_ =  *orlat_;
# line 464 "templ.f"
y2_ = ynew_;
# line 465 "templ.f"
}
# line 466 "templ.f"
orlon2_ = (float) 1.79999893188476562e+02;
# line 467 "templ.f"
x2_ = xnew_;
# line 469 "templ.f"
xy2ll_(plat_,plon_,&x2_,&y2_,&orlat2_,&orlon2_);
# line 470 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 470 "templ.f"
 *plat_ = -( *plat_);
# line 470 "templ.f"
}
# line 471 "templ.f"
} else if (icross_==4) {
# line 475 "templ.f"
xnew_ = x_ - x2_;
# line 476 "templ.f"
ynew_ = y_ - y2_;
# line 478 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 479 "templ.f"
orlat2_ = -( *orlat_);
# line 480 "templ.f"
y2_ = -(ynew_);
# line 481 "templ.f"
} else {
# line 482 "templ.f"
orlat2_ =  *orlat_;
# line 483 "templ.f"
y2_ = ynew_;
# line 484 "templ.f"
}
# line 485 "templ.f"
orlon2_ = (float) 9.99999974737875164e-05;
# line 486 "templ.f"
x2_ = xnew_;
# line 488 "templ.f"
xy2ll_(plat_,plon_,&x2_,&y2_,&orlat2_,&orlon2_);
# line 489 "templ.f"
if ( *orlat_<(float) 0.00000000000000000e+00) {
# line 489 "templ.f"
 *plat_ = -( *plat_);
# line 489 "templ.f"
}
# line 490 "templ.f"
} else if (icross_==5) {
# line 494 "templ.f"
xnew_ = x_ - x3_;
# line 495 "templ.f"
ynew_ = y_ - y3_;
# line 497 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 498 "templ.f"
orlon2_ = -( *orlon_);
# line 499 "templ.f"
x3_ = -(xnew_);
# line 500 "templ.f"
} else {
# line 501 "templ.f"
orlon2_ =  *orlon_;
# line 502 "templ.f"
x3_ = xnew_;
# line 503 "templ.f"
}
# line 504 "templ.f"
orlat2_ = (float) 9.99999974737875164e-05;
# line 505 "templ.f"
y3_ = -(ynew_);
# line 507 "templ.f"
xy2ll_(plat_,plon_,&x3_,&y3_,&orlat2_,&orlon2_);
# line 508 "templ.f"
 *plat_ = -( *plat_);
# line 509 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 509 "templ.f"
 *plon_ = -( *plon_);
# line 509 "templ.f"
}
# line 510 "templ.f"
} else if (icross_==6) {
# line 514 "templ.f"
xnew_ = x_ - x3_;
# line 515 "templ.f"
ynew_ = y_ - y3_;
# line 517 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 518 "templ.f"
orlon2_ = -( *orlon_);
# line 519 "templ.f"
x3_ = -(xnew_);
# line 520 "templ.f"
} else {
# line 521 "templ.f"
orlon2_ =  *orlon_;
# line 522 "templ.f"
x3_ = xnew_;
# line 523 "templ.f"
}
# line 524 "templ.f"
orlat2_ = (float) 9.99999974737875164e-05;
# line 525 "templ.f"
y3_ = ynew_;
# line 527 "templ.f"
xy2ll_(plat_,plon_,&x3_,&y3_,&orlat2_,&orlon2_);
# line 528 "templ.f"
if ( *orlon_<(float) 0.00000000000000000e+00) {
# line 528 "templ.f"
 *plon_ = -( *plon_);
# line 528 "templ.f"
}
# line 529 "templ.f"
}
# line 532 "templ.f"
return ;
# line 534 "templ.f"
}
# line 538 "templ.f"
void xy2ll_(deglat_,deglon_,x_,y_,swlat_,swlon_)
     Real *deglat_;
     Real *deglon_;
     Real *x_;
     Real *y_;
     Real *swlat_;
     Real *swlon_;
{
Real tmp1;
Real tmp2;
Real tmp3;
Double tmp4;
auto Double lamda1_;
auto Double cang_;
auto Double sang_;
static Double degarc_ =  1.11353996276855469e+02;
auto Double phi2_;
auto Double lamda2_;
static Integer ientry_ = 0;
auto Double acz_;
auto Double dtr_;
auto Double phi1_;
auto Double theta_;
auto Double rtd_;
auto Double r_;
# line 538 "templ.f"
# line 549 "templ.f"
# line 550 "templ.f"
# line 555 "templ.f"
# line 562 "templ.f"
# line 563 "templ.f"
# line 565 "templ.f"
if (ientry_==1) {
# line 565 "templ.f"
goto L_100;
# line 565 "templ.f"
}
# line 566 "templ.f"
ientry_ = 1;
# line 567 "templ.f"
ASSIGNFLOAT(tmp3,r_atan_((tmp2 = (float) 1.00000000000000000e+00 , &tmp2)));
# line 567 "templ.f"
dtr_ = tmp3/(float) 4.50000000000000000e+01;
# line 568 "templ.f"
rtd_ = (float) 1.00000000000000000e+00/dtr_;
L_100:
# line 569 "templ.f"
;
# line 571 "templ.f"
 *deglat_ =  *swlat_;
# line 572 "templ.f"
 *deglon_ =  *swlon_;
# line 574 "templ.f"
ASSIGNFLOAT(tmp3,r_sqrt_((tmp2 =  *x_* *x_ +  *y_* *y_ , &tmp2)));
# line 574 "templ.f"
r_ = tmp3;
# line 575 "templ.f"
if (r_<(float) 9.99999977648258209e-03) {
# line 575 "templ.f"
goto L_10;
# line 575 "templ.f"
}
# line 577 "templ.f"
ASSIGNFLOAT(tmp2,r_atan2_(x_,y_));
# line 577 "templ.f"
theta_ = tmp2;
# line 578 "templ.f"
r_ = (r_/degarc_)*dtr_;
# line 580 "templ.f"
phi1_ = dtr_* *swlat_;
# line 581 "templ.f"
lamda1_ = dtr_* *swlon_;
# line 583 "templ.f"
sang_ = cos(theta_)*cos(phi1_)*sin(r_) + sin(phi1_)*cos(r_);
# line 584 "templ.f"
if ((sang_>=0.0 ? sang_ : -(sang_))>(float) 1.00000000000000000e+00) {
# line 585 "templ.f"
if (sang_<(float) 0.00000000000000000e+00) {
# line 585 "templ.f"
sang_ = (float)-1.00000000000000000e+00;
# line 585 "templ.f"
}
# line 586 "templ.f"
if (sang_>(float) 0.00000000000000000e+00) {
# line 586 "templ.f"
sang_ = (float) 1.00000000000000000e+00;
# line 586 "templ.f"
}
# line 587 "templ.f"
}
# line 589 "templ.f"
phi2_ = asin(sang_);
# line 591 "templ.f"
cang_ = (cos(r_) - sin(phi1_)*sin(phi2_))/(cos(phi1_)*cos(phi2_));
# line 592 "templ.f"
if ((cang_>=0.0 ? cang_ : -(cang_))>(float) 1.00000000000000000e+00) {
# line 593 "templ.f"
if (cang_<(float) 0.00000000000000000e+00) {
# line 593 "templ.f"
cang_ = (float)-1.00000000000000000e+00;
# line 593 "templ.f"
}
# line 594 "templ.f"
if (cang_>(float) 0.00000000000000000e+00) {
# line 594 "templ.f"
cang_ = (float) 1.00000000000000000e+00;
# line 594 "templ.f"
}
# line 595 "templ.f"
}
# line 597 "templ.f"
acz_ = acos(cang_);
# line 599 "templ.f"
if ( *x_<(float) 0.00000000000000000e+00) {
# line 600 "templ.f"
lamda2_ = lamda1_ + acz_;
# line 601 "templ.f"
} else {
# line 602 "templ.f"
lamda2_ = lamda1_ - acz_;
# line 603 "templ.f"
}
# line 605 "templ.f"
 *deglat_ = rtd_*phi2_;
# line 606 "templ.f"
 *deglon_ = rtd_*lamda2_;
L_10:
# line 608 "templ.f"
;
# line 610 "templ.f"
return ;
# line 611 "templ.f"
}

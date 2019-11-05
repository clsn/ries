/*
    msal_math64.c   Munafo Stand-Alone Library
                    64-bit math routines

 Much of the contents of this library are derived from SunSoft source
 code bearing the following copyright notices:

 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.

 * Copyright 2004 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.


 Additional parts are derived from the GNU Scientific Library (GSL),
 distributed under the       ,= ,-_-. =.
 terms of the GNU General   ((_/)o o(\_))
 Public License 3.0:         `-'(. .)`-'
                                 \_/

 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Gerard Jungman
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 110-1301, USA.


 Most of RIES is licensed under GPL version 2 (June 1991). The GPL version
 3 is applicable to MSAL (this stand-alone library of maths functions).
 The two licenses are available at:

    mrob.com/pub/ries/COPYING.txt
    mrob.com/pub/ries/GPL-3.txt

REVISION HISTORY
 20121203 First version: only has msal_tan() and its supporting functions.
 20121215 Add msal_sin, msal_cos
 20130203 Add msal_lambertw; try to clean up various sign conversion warnings
 20130820 Add long double trig functions (with precision suitable for
64-bit mantissa long doubles).
*/

/* Sometimes it's necessary to define __LITTLE_ENDIAN explicitly
   but these catch some common cases. */

#if defined(__LITTLE_ENDIAN__) || \
    defined(i386) || defined(__x86_64__) || defined(i486) || \
    defined(intel) || defined(x86) || defined(i86pc) || \
    defined(__alpha) || defined(__osf__)
# ifndef __LITTLE_ENDIAN
#  define __LITTLE_ENDIAN
# endif
#endif

typedef union {
  double value;
  unsigned int ints[2];
} msal_union_double_int2;

#ifdef __LITTLE_ENDIAN
# define __HI(x) *(1+(int*)&x)
# define __LO(x) *(int*)&x
# define MSAL_HI(x) (((msal_union_double_int2*)&x)->ints[1])
# define MSAL_LO(x) (((msal_union_double_int2*)&x)->ints[0])
#else
# define __HI(x) *(int*)&x
# define __LO(x) *(1+(int*)&x)
# define MSAL_HI(x) (((msal_union_double_int2*)&x)->ints[0])
# define MSAL_LO(x) (((msal_union_double_int2*)&x)->ints[1])
#endif


#ifdef __STDC__
# define    MSAL__P(p)    p
#else
# define    MSAL__P(p)    ()
#endif

/* 2.7182818284590452353602874713526624... */
#define MSAL_E 2.71828182845904523e+00


/*
 * ANSI/POSIX
 */

#define    MAXFLOAT    ((float)3.40282346638528860e+38)

#define    HUGE        MAXFLOAT

/* 
 * set X_TLOSS = pi*2**52, which is possibly defined in <values.h>
 * (one may replace the following line by "#include <values.h>")
 */

#define X_TLOSS        1.41484755040568800000e+16 

#define    DOMAIN        1
#define    SING        2
#define    OVERFLOW    3
#define    UNDERFLOW    4
#define    TLOSS        5
#define    PLOSS        6

void msal_version_info MSAL__P((void));

extern double msal_sin MSAL__P((double));

extern double msal_fabs MSAL__P((double));
extern double msal_floor MSAL__P((double));

extern double msal_copysign MSAL__P((double, double));
extern double msal_scalbn MSAL__P((double, int));

extern int    __ieee754_rem_pio2 MSAL__P((double,double*));

extern double __kernel_sin MSAL__P((double,double,int));
extern double __kernel_cos MSAL__P((double,double));
extern double __kernel_tan MSAL__P((double,double,int));
extern int    __kernel_rem_pio2 MSAL__P((double*,double*,int,int,int,const int*));

extern double msal_cos MSAL__P((double));
extern double msal_tan MSAL__P((double));

extern long double ldbl_sincos MSAL__P((long double xq, long double zq, long double uq, double i));
extern long double msal_sinl MSAL__P((long double));
extern long double msal_cosl MSAL__P((long double));
extern long double msal_tanl MSAL__P((long double));

double msal_w_approx0 MSAL__P((double x, int branch));
double msal_w0_approx1 MSAL__P((double x));
double msal_w0_approx2 MSAL__P((double x));
double msal_lambertw MSAL__P((double x));
long double msal_lambertwl MSAL__P((long double x));
double msal_lanczos_ln_gamma MSAL__P((double z));
double msal_lanczos_gamma MSAL__P((double z));
double msal_digamma MSAL__P((double x));
double msal_dgamma MSAL__P((double z));
void msal_test_lambert MSAL__P((void));
void msal_test_gamma MSAL__P((void));



void msal_version_info(void)
{
  printf("%s",
"msal_math64 transcendental maths function library by Robert Munafo\n"
"  sine, cosine, and tangent functions adapted from code by SunSoft and\n"
"    Copyright (C) 1993, 2004 by Sun Microsystems, Inc.\n"
"  Lambert W function adapted from work by Darko Veberic, originally\n"
"    published at http://arxiv.org/abs/1003.1628\n"
  );
}


/*
 * msal_fabs(x) returns the absolute value of x.
 */

#ifdef __STDC__
  double msal_fabs(double x)
#else
  double msal_fabs(x)
  double x;
#endif
{
  MSAL_HI(x) &= 0x7fffffff;
  return x;
}


/*
 * floor(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *    Bit twiddling.
 * Exception:
 *    Inexact flag raised if x not equal to floor(x).
 */

#ifdef __STDC__
static const double huge = 1.0e300;
#else
static double huge = 1.0e300;
#endif

#if 0
/* Floor function, with explicit casts to avoid warnings. This is untested. */
#ifdef __STDC__
  double msal_floor(double x)
#else
  double msal_floor(x)
  double x;
#endif
{
  int i0,i1,j0;
  unsigned i,j;

  i0 = (int)MSAL_HI(x);
  i1 = (int)MSAL_LO(x);
  j0 = ((i0>>20)&0x7ff)-0x3ff;
  if(j0<20) {
    if(j0<0) {     /* raise inexact if x != 0 */
      if(huge+x>0.0) {/* return 0*sign(x) if |x|<1 */
        if (i0>=0) {
          i0=i1=0;
        } else if (((i0&0x7fffffff)|i1)!=0) {
          i0=(int)0xbff00000;i1=0;
        }
      }
    } else {
      i = ((unsigned)0x000fffff)>>j0;
      if ((((unsigned)i0&i)|(unsigned)i1)==0)
        return x; /* x is integral */
      if (huge+x>0.0) {    /* raise inexact flag */
        if (i0<0)
          i0 += (0x00100000)>>j0;
        i0 = (int)((unsigned)i0 & (~i)); i1=0;
      }
    }
  } else if (j0>51) {
    if(j0==0x400)
      return x+x;    /* inf or NaN */
    else
      return x;        /* x is integral */
  } else {
    i = ((unsigned)(0xffffffff))>>(j0-20);
    if ((((unsigned)i1)&i)==0)
      return x;    /* x is integral */
    if(huge+x>0.0) {         /* raise inexact flag */
      if(i0<0) {
        if (j0==20)
          i0+=1; 
        else {
          j = (unsigned)i1+((unsigned)1<<(52-(unsigned)j0));
          if(j<((unsigned)i1)) i0 +=1 ;     /* got a carry */
          i1=j;
        }
      }
      i1 &= (~i);
    }
  }
  MSAL_HI(x) = (unsigned)i0;
  MSAL_LO(x) = (unsigned)i1;
  return x;
}
#endif


/*
 * copysign(double x, double y)
 * copysign(x,y) returns a value with the magnitude of x and
 * with the sign bit of y.
 */

#ifdef __STDC__
  double msal_copysign(double x, double y)
#else
  double msal_copysign(x,y)
  double x,y;
#endif
{
    MSAL_HI(x) = (MSAL_HI(x)&0x7fffffff)|(MSAL_HI(y)&0x80000000);
        return x;
}



/* 
 * scalbn (double x, int n)
 * scalbn(x,n) returns x* 2**n  computed by  exponent  
 * manipulation rather than by actually performing an 
 * exponentiation or a multiplication.
 */

#ifdef __STDC__
static const double
#else
static double
#endif
two54   =  1.80143985094819840000e+16, /* 0x43500000, 0x00000000 */
twom54  =  5.55111512312578270212e-17, /* 0x3C900000, 0x00000000 */
tiny   = 1.0e-300;

#ifdef __STDC__
    double msal_scalbn (double x, int n)
#else
    double msal_scalbn (x,n)
    double x; int n;
#endif
{
  int  k,hx,lx;
  hx = (int)MSAL_HI(x);
  lx = (int)MSAL_LO(x);
  k = (hx&0x7ff00000)>>20;        /* extract exponent */
  if (k==0) {                /* 0 or subnormal x */
    if ((lx|(hx&0x7fffffff))==0)
      return x; /* +-0 */
    x *= two54; 
    hx = (int)MSAL_HI(x);
    k = ((hx&0x7ff00000)>>20) - 54; 
    if (n< -50000)
      return tiny*x;     /*underflow*/
  }
  if (k==0x7ff) return x+x;        /* NaN or Inf */
  k = k+n; 
  if (k >  0x7fe) return huge*msal_copysign(huge,x); /* overflow  */
  if (k > 0) {                /* normal result */
    hx = (hx&(int)0x800fffff)|(k<<20);
    MSAL_HI(x) = (unsigned)hx;
    return x;
  }
  if (k <= -54) {
    if (n > 50000)     /* in case integer overflow in n+k */
      return huge*msal_copysign(huge,x);    /*overflow*/
    else return tiny*msal_copysign(tiny,x);     /*underflow*/
  }
  k += 54;                /* subnormal result */
  hx = (hx&(int)0x800fffff)|(k<<20);
  MSAL_HI(x) = (unsigned)hx;
  return x*twom54;
}



/*
 * __kernel_rem_pio2(x,y,e0,nx,prec,ipio2)
 * double x[],y[]; int e0,nx,prec; int ipio2[];
 * 
 * __kernel_rem_pio2 return the last three digits of N with 
 *        y = x - N*pi/2
 * so that |y| < pi/2.
 *
 * The method is to compute the integer (mod 8) and fraction parts of 
 * (2/pi)*x without doing the full multiplication. In general we
 * skip the part of the product that are known to be a huge integer (
 * more accurately, = 0 mod 8 ). Thus the number of operations are
 * independent of the exponent of the input.
 *
 * (2/pi) is represented by an array of 24-bit integers in ipio2[].
 *
 * Input parameters:
 *     x[]    The input value (must be positive) is broken into nx 
 *        pieces of 24-bit integers in double precision format.
 *        x[i] will be the i-th 24 bit of x. The scaled exponent 
 *        of x[0] is given in input parameter e0 (i.e., x[0]*2^e0 
 *        match x's up to 24 bits.
 *
 *        Example of breaking a double positive z into x[0]+x[1]+x[2]:
 *            e0 = ilogb(z)-23
 *            z  = scalbn(z,-e0)
 *        for i = 0,1,2
 *            x[i] = floor(z)
 *            z    = (z-x[i])*2**24
 *
 *
 *    y[]    ouput result in an array of double precision numbers.
 *        The dimension of y[] is:
 *            24-bit  precision    1
 *            53-bit  precision    2
 *            64-bit  precision    2
 *            113-bit precision    3
 *        The actual value is the sum of them. Thus for 113-bit
 *        precison, one may have to do something like:
 *
 *        long double t,w,r_head, r_tail;
 *        t = (long double)y[2] + (long double)y[1];
 *        w = (long double)y[0];
 *        r_head = t+w;
 *        r_tail = w - (r_head - t);
 *
 *    e0    The exponent of x[0]
 *
 *    nx    dimension of x[]
 *
 *      prec    an integer indicating the precision:
 *            0    24  bits (single)
 *            1    53  bits (double)
 *            2    64  bits (extended)
 *            3    113 bits (quad)
 *
 *    ipio2[]
 *        integer array, contains the (24*i)-th to (24*i+23)-th 
 *        bit of 2/pi after binary point. The corresponding 
 *        floating value is
 *
 *            ipio2[i] * 2^(-24(i+1)).
 *
 * External function:
 *    double scalbn(), floor();
 *
 *
 * Here is the description of some local variables:
 *
 *     jk    jk+1 is the initial number of terms of ipio2[] needed
 *        in the computation. The recommended value is 2,3,4,
 *        6 for single, double, extended,and quad.
 *
 *     jz    local integer variable indicating the number of 
 *        terms of ipio2[] used. 
 *
 *    jx    nx - 1
 *
 *    jv    index for pointing to the suitable ipio2[] for the
 *        computation. In general, we want
 *            ( 2^e0*x[0] * ipio2[jv-1]*2^(-24jv) )/8
 *        is an integer. Thus
 *            e0-3-24*jv >= 0 or (e0-3)/24 >= jv
 *        Hence jv = max(0,(e0-3)/24).
 *
 *    jp    jp+1 is the number of terms in PIo2[] needed, jp = jk.
 *
 *     q[]    double array with integral value, representing the
 *        24-bits chunk of the product of x and 2/pi.
 *
 *    q0    the corresponding exponent of q[0]. Note that the
 *        exponent for q[i] would be q0-24*i.
 *
 *    PIo2[]    double precision array, obtained by cutting pi/2
 *        into 24 bits chunks. 
 *
 *    f[]    ipio2[] in floating point 
 *
 *    iq[]    integer array by breaking up q[] in 24-bits chunk.
 *
 *    fq[]    final product of x*(2/pi) in fq[0],..,fq[jk]
 *
 *    ih    integer. If >0 it indicates q[] is >= 0.5, hence
 *        it also indicates the *sign* of the result.
 *
 */


/*
 * Constants:
 * The hexadecimal values are the intended ones for the following 
 * constants. The decimal values may be used, provided that the 
 * compiler will convert from decimal to binary accurately enough 
 * to produce the hexadecimal values shown.
 */

#ifdef __STDC__
static const int init_jk[] = {2,3,4,6}; /* initial value for jk */
#else
static int init_jk[] = {2,3,4,6}; 
#endif

#ifdef __STDC__
static const double PIo2[] = {
#else
static double PIo2[] = {
#endif
  1.57079625129699707031e+00, /* 0x3FF921FB, 0x40000000 */
  7.54978941586159635335e-08, /* 0x3E74442D, 0x00000000 */
  5.39030252995776476554e-15, /* 0x3CF84698, 0x80000000 */
  3.28200341580791294123e-22, /* 0x3B78CC51, 0x60000000 */
  1.27065575308067607349e-29, /* 0x39F01B83, 0x80000000 */
  1.22933308981111328932e-36, /* 0x387A2520, 0x40000000 */
  2.73370053816464559624e-44, /* 0x36E38222, 0x80000000 */
  2.16741683877804819444e-51, /* 0x3569F31D, 0x00000000 */
};

#ifdef __STDC__
static const double            
#else
static double            
#endif
zero =  0.00000000000000000000e+00, /* 0x00000000, 0x00000000 */
one =  1.00000000000000000000e+00, /* 0x3FF00000, 0x00000000 */
two24   =  1.67772160000000000000e+07, /* 0x41700000, 0x00000000 */
twon24  =  5.96046447753906250000e-08; /* 0x3E700000, 0x00000000 */

#ifdef __STDC__
    int __kernel_rem_pio2(double *x, double *y, int e0, int nx, int prec, const int *ipio2) 
#else
    int __kernel_rem_pio2(x,y,e0,nx,prec,ipio2)     
    double x[], y[]; int e0,nx,prec; int ipio2[];
#endif
{
    int jz,jx,jv,jp,jk,carry,n,iq[20],i,j,k,m,q0,ih;
    double z,fw,f[20],fq[20],q[20];
    /* printf("__kernel_rem_pio2(%f,%f)\n", *x, *y); */
    /* initialize jk*/
    jk = init_jk[prec];
    jp = jk;

    /* determine jx,jv,q0, note that 3>q0 */
    jx =  nx-1;
    jv = (e0-3)/24; if(jv<0) jv=0;
    q0 =  e0-24*(jv+1);

    /* set up f[0] to f[jx+jk] where f[jx+jk] = ipio2[jv+jk] */
    j = jv-jx; m = jx+jk;
    for(i=0;i<=m;i++,j++) f[i] = (j<0)? zero : (double) ipio2[j];

    /* compute q[0],q[1],...q[jk] */
    for (i=0;i<=jk;i++) {
        for(j=0,fw=0.0;j<=jx;j++) fw += x[j]*f[jx+i-j]; q[i] = fw;
    }

    jz = jk;
recompute:
    /* distill q[] into iq[] reversingly */
    for(i=0,j=jz,z=q[jz];j>0;i++,j--) {
        fw    =  (double)((int)(twon24* z));
        iq[i] =  (int)(z-two24*fw);
        z     =  q[j-1]+fw;
    }

    /* compute n */
    z  = msal_scalbn(z,q0);        /* actual value of z */
    z -= 8.0*floor(z*0.125);        /* trim off integer >= 8 */
    n  = (int) z;
    z -= (double)n;
    ih = 0;
    if(q0>0) {    /* need iq[jz-1] to determine n */
        i  = (iq[jz-1]>>(24-q0)); n += i;
        iq[jz-1] -= i<<(24-q0);
        ih = iq[jz-1]>>(23-q0);
    } 
    else if(q0==0) ih = iq[jz-1]>>23;
    else if(z>=0.5) ih=2;

    if(ih>0) {    /* q > 0.5 */
        n += 1; carry = 0;
        for(i=0;i<jz ;i++) {    /* compute 1-q */
        j = iq[i];
        if(carry==0) {
            if(j!=0) {
            carry = 1; iq[i] = 0x1000000- j;
            }
        } else  iq[i] = 0xffffff - j;
        }
        if(q0>0) {        /* rare case: chance is 1 in 12 */
            switch(q0) {
            case 1:
               iq[jz-1] &= 0x7fffff; break;
            case 2:
               iq[jz-1] &= 0x3fffff; break;
            }
        }
        if(ih==2) {
        z = one - z;
        if(carry!=0) z -= msal_scalbn(one,q0);
        }
    }

    /* check if recomputation is needed */
    if(z==zero) {
        j = 0;
        for (i=jz-1;i>=jk;i--) j |= iq[i];
        if(j==0) { /* need recomputation */
        for(k=1;iq[jk-k]==0;k++);   /* k = no. of terms needed */

        for(i=jz+1;i<=jz+k;i++) {   /* add q[jz+1] to q[jz+k] */
            f[jx+i] = (double) ipio2[jv+i];
            for(j=0,fw=0.0;j<=jx;j++) fw += x[j]*f[jx+i-j];
            q[i] = fw;
        }
        jz += k;
        goto recompute;
        }
    }

    /* chop off zero terms */
    if(z==0.0) {
        jz -= 1; q0 -= 24;
        while(iq[jz]==0) { jz--; q0-=24;}
    } else { /* break z into 24-bit if necessary */
        z = msal_scalbn(z,-q0);
        if(z>=two24) { 
        fw = (double)((int)(twon24*z));
        iq[jz] = (int)(z-two24*fw);
        jz += 1; q0 += 24;
        iq[jz] = (int) fw;
        } else iq[jz] = (int) z ;
    }

    /* convert integer "bit" chunk to floating-point value */
    fw = msal_scalbn(one,q0);
    for(i=jz;i>=0;i--) {
        q[i] = fw*(double)iq[i]; fw*=twon24;
    }

    /* compute PIo2[0,...,jp]*q[jz,...,0] */
    for(i=jz;i>=0;i--) {
        for(fw=0.0,k=0;k<=jp&&k<=jz-i;k++) fw += PIo2[k]*q[i+k];
        fq[jz-i] = fw;
    }

    /* compress fq[] into y[] */
    switch(prec) {
        case 0:
        fw = 0.0;
        for (i=jz;i>=0;i--) fw += fq[i];
        y[0] = (ih==0)? fw: -fw; 
        break;
        case 1:
        case 2:
        fw = 0.0;
        for (i=jz;i>=0;i--) fw += fq[i]; 
        y[0] = (ih==0)? fw: -fw; 
        fw = fq[0]-fw;
        for (i=1;i<=jz;i++) fw += fq[i];
        y[1] = (ih==0)? fw: -fw; 
        break;
        case 3:    /* painful */
        for (i=jz;i>0;i--) {
            fw      = fq[i-1]+fq[i]; 
            fq[i]  += fq[i-1]-fw;
            fq[i-1] = fw;
        }
        for (i=jz;i>1;i--) {
            fw      = fq[i-1]+fq[i]; 
            fq[i]  += fq[i-1]-fw;
            fq[i-1] = fw;
        }
        for (fw=0.0,i=jz;i>=2;i--) fw += fq[i]; 
        if(ih==0) {
            y[0] =  fq[0]; y[1] =  fq[1]; y[2] =  fw;
        } else {
            y[0] = -fq[0]; y[1] = -fq[1]; y[2] = -fw;
        }
    }
    return n&7;
}


/* __ieee754_rem_pio2(x,y)
 * 
 * return the remainder of x rem pi/2 in y[0]+y[1] 
 * use __kernel_rem_pio2()
 */

/*
 * Table of constants for 2/pi, 396 Hex digits (476 decimal) of 2/pi 
 */
#ifdef __STDC__
static const int two_over_pi[] = {
#else
static int two_over_pi[] = {
#endif
0xA2F983, 0x6E4E44, 0x1529FC, 0x2757D1, 0xF534DD, 0xC0DB62, 
0x95993C, 0x439041, 0xFE5163, 0xABDEBB, 0xC561B7, 0x246E3A, 
0x424DD2, 0xE00649, 0x2EEA09, 0xD1921C, 0xFE1DEB, 0x1CB129, 
0xA73EE8, 0x8235F5, 0x2EBB44, 0x84E99C, 0x7026B4, 0x5F7E41, 
0x3991D6, 0x398353, 0x39F49C, 0x845F8B, 0xBDF928, 0x3B1FF8, 
0x97FFDE, 0x05980F, 0xEF2F11, 0x8B5A0A, 0x6D1F6D, 0x367ECF, 
0x27CB09, 0xB74F46, 0x3F669E, 0x5FEA2D, 0x7527BA, 0xC7EBE5, 
0xF17B3D, 0x0739F7, 0x8A5292, 0xEA6BFB, 0x5FB11F, 0x8D5D08, 
0x560330, 0x46FC7B, 0x6BABF0, 0xCFBC20, 0x9AF436, 0x1DA9E3, 
0x91615E, 0xE61B08, 0x659985, 0x5F14A0, 0x68408D, 0xFFD880, 
0x4D7327, 0x310606, 0x1556CA, 0x73A8C9, 0x60E27B, 0xC08C6B, 
};

#ifdef __STDC__
static const int npio2_hw[] = {
#else
static int npio2_hw[] = {
#endif
0x3FF921FB, 0x400921FB, 0x4012D97C, 0x401921FB, 0x401F6A7A, 0x4022D97C,
0x4025FDBB, 0x402921FB, 0x402C463A, 0x402F6A7A, 0x4031475C, 0x4032D97C,
0x40346B9C, 0x4035FDBB, 0x40378FDB, 0x403921FB, 0x403AB41B, 0x403C463A,
0x403DD85A, 0x403F6A7A, 0x40407E4C, 0x4041475C, 0x4042106C, 0x4042D97C,
0x4043A28C, 0x40446B9C, 0x404534AC, 0x4045FDBB, 0x4046C6CB, 0x40478FDB,
0x404858EB, 0x404921FB,
};

/*
 * invpio2:  53 bits of 2/pi
 * pio2_1:   first  33 bit of pi/2
 * pio2_1t:  pi/2 - pio2_1
 * pio2_2:   second 33 bit of pi/2
 * pio2_2t:  pi/2 - (pio2_1+pio2_2)
 * pio2_3:   third  33 bit of pi/2
 * pio2_3t:  pi/2 - (pio2_1+pio2_2+pio2_3)
 */

#ifdef __STDC__
static const double 
#else
static double 
#endif
half =  5.00000000000000000000e-01, /* 0x3FE00000, 0x00000000 */
invpio2 =  6.36619772367581382433e-01, /* 0x3FE45F30, 0x6DC9C883 */
pio2_1  =  1.57079632673412561417e+00, /* 0x3FF921FB, 0x54400000 */
pio2_1t =  6.07710050650619224932e-11, /* 0x3DD0B461, 0x1A626331 */
pio2_2  =  6.07710050630396597660e-11, /* 0x3DD0B461, 0x1A600000 */
pio2_2t =  2.02226624879595063154e-21, /* 0x3BA3198A, 0x2E037073 */
pio2_3  =  2.02226624871116645580e-21, /* 0x3BA3198A, 0x2E000000 */
pio2_3t =  8.47842766036889956997e-32; /* 0x397B839A, 0x252049C1 */

#ifdef __STDC__
    int __ieee754_rem_pio2(double x, double *y)
#else
    int __ieee754_rem_pio2(x,y)
    double x,y[];
#endif
{
    double z,w,t,r,fn;
    double tx[3];
    int e0,i,j,nx,n,ix,hx;

    hx = (int)MSAL_HI(x);        /* high word of x */
    ix = hx&0x7fffffff;
    if(ix<=0x3fe921fb)   /* |x| ~<= pi/4 , no need for reduction */
        {y[0] = x; y[1] = 0; return 0;}
    if(ix<0x4002d97c) {  /* |x| < 3pi/4, special case with n=+-1 */
        if(hx>0) { 
        z = x - pio2_1;
        if(ix!=0x3ff921fb) {     /* 33+53 bit pi is good enough */
            y[0] = z - pio2_1t;
            y[1] = (z-y[0])-pio2_1t;
        } else {        /* near pi/2, use 33+33+53 bit pi */
            z -= pio2_2;
            y[0] = z - pio2_2t;
            y[1] = (z-y[0])-pio2_2t;
        }
        return 1;
        } else {    /* negative x */
        z = x + pio2_1;
        if(ix!=0x3ff921fb) {     /* 33+53 bit pi is good enough */
            y[0] = z + pio2_1t;
            y[1] = (z-y[0])+pio2_1t;
        } else {        /* near pi/2, use 33+33+53 bit pi */
            z += pio2_2;
            y[0] = z + pio2_2t;
            y[1] = (z-y[0])+pio2_2t;
        }
        return -1;
        }
    }
    if(ix<=0x413921fb) { /* |x| ~<= 2^19*(pi/2), medium size */
        t  = msal_fabs(x);
        n  = (int) (t*invpio2+half);
        fn = (double)n;
        r  = t-fn*pio2_1;
        w  = fn*pio2_1t;    /* 1st round good to 85 bit */
        if(n<32&&ix!=npio2_hw[n-1]) {    
        y[0] = r-w;    /* quick check no cancellation */
        } else {
            j  = ix>>20;
            y[0] = r-w; 
            i = j-((((int)MSAL_HI(y[0]))>>20)&0x7ff);
            if(i>16) {  /* 2nd iteration needed, good to 118 */
            t  = r;
            w  = fn*pio2_2;    
            r  = t-w;
            w  = fn*pio2_2t-((t-r)-w);    
            y[0] = r-w;
            i = j-((((int)MSAL_HI(y[0]))>>20)&0x7ff);
            if(i>49)  {    /* 3rd iteration need, 151 bits acc */
                t  = r;    /* will cover all possible cases */
                w  = fn*pio2_3;    
                r  = t-w;
                w  = fn*pio2_3t-((t-r)-w);    
                y[0] = r-w;
            }
        }
        }
        y[1] = (r-y[0])-w;
        if(hx<0)     {y[0] = -y[0]; y[1] = -y[1]; return -n;}
        else     return n;
    }
    /* 
     * all other (large) arguments
     */
    if(ix>=0x7ff00000) {        /* x is inf or NaN */
        y[0]=y[1]=x-x; return 0;
    }
    /* set z = scalbn(|x|,ilogb(x)-23) */
    MSAL_LO(z) = MSAL_LO(x);
    e0     = (ix>>20)-1046;    /* e0 = ilogb(z)-23; */
    MSAL_HI(z) = (unsigned)(ix - (e0<<20));
    for(i=0;i<2;i++) {
        tx[i] = (double)((int)(z));
        z     = (z-tx[i])*two24;
    }
    tx[2] = z;
    nx = 3;
    while(tx[nx-1]==zero) nx--;    /* skip zero term */
    n  =  __kernel_rem_pio2(tx,y,e0,nx,2,two_over_pi);
    if(hx<0) {y[0] = -y[0]; y[1] = -y[1]; return -n;}
    return n;
}



/* __kernel_sin( x, y, iy)
 * kernel sin function on [-pi/4, pi/4], pi/4 ~ 0.7854
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 * Input iy indicates whether y is 0. (if iy=0, y assume to be 0). 
 *
 * Algorithm
 *    1. Since sin(-x) = -sin(x), we need only to consider positive x. 
 *    2. if x < 2^-27 (hx<0x3e400000 0), return x with inexact if x!=0.
 *    3. sin(x) is approximated by a polynomial of degree 13 on
 *       [0,pi/4]
 *                       3            13
 *           sin(x) ~ x + S1*x + ... + S6*x
 *       where
 *    
 *     |sin(x)         2     4     6     8     10     12  |     -58
 *     |----- - (1+S1*x +S2*x +S3*x +S4*x +S5*x  +S6*x   )| <= 2
 *     |  x                                | 
 * 
 *    4. sin(x+y) = sin(x) + sin'(x')*y
 *            ~ sin(x) + (1-x*x/2)*y
 *       For better accuracy, let 
 *             3      2      2      2      2
 *        r = x *(S2+x *(S3+x *(S4+x *(S5+x *S6))))
 *       then                   3    2
 *        sin(x) = x + (S1*x + (x *(r-y/2)+y))
 */

#ifdef __STDC__
static const double 
#else
static double 
#endif
S1  = -1.66666666666666324348e-01, /* 0xBFC55555, 0x55555549 */
S2  =  8.33333333332248946124e-03, /* 0x3F811111, 0x1110F8A6 */
S3  = -1.98412698298579493134e-04, /* 0xBF2A01A0, 0x19C161D5 */
S4  =  2.75573137070700676789e-06, /* 0x3EC71DE3, 0x57B1FE7D */
S5  = -2.50507602534068634195e-08, /* 0xBE5AE5E6, 0x8A2B9CEB */
S6  =  1.58969099521155010221e-10; /* 0x3DE5D93A, 0x5ACFD57C */

#ifdef __STDC__
    double __kernel_sin(double x, double y, int iy)
#else
    double __kernel_sin(x, y, iy)
    double x,y; int iy;        /* iy=0 if y is zero */
#endif
{
    double z,r,v;
    int ix;
    ix = MSAL_HI(x)&0x7fffffff;    /* high word of x */
    if(ix<0x3e400000)            /* |x| < 2**-27 */
       {if((int)x==0) return x;}        /* generate inexact */
    z    =  x*x;
    v    =  z*x;
    r    =  S2+z*(S3+z*(S4+z*(S5+z*S6)));
    if(iy==0) return x+v*(S1+z*r);
    else      return x-((z*(half*y-v*r)-y)-v*S1);
}

/*
 * __kernel_cos( x,  y )
 * kernel cos function on [-pi/4, pi/4], pi/4 ~ 0.785398164
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x. 
 *
 * Algorithm
 *    1. Since cos(-x) = cos(x), we need only to consider positive x.
 *    2. if x < 2^-27 (hx<0x3e400000 0), return 1 with inexact if x!=0.
 *    3. cos(x) is approximated by a polynomial of degree 14 on
 *       [0,pi/4]
 *                               4            14
 *           cos(x) ~ 1 - x*x/2 + C1*x + ... + C6*x
 *       where the remez error is
 *    
 *     |              2     4     6     8     10    12     14 |     -58
 *     |cos(x)-(1-.5*x +C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  )| <= 2
 *     |                                       | 
 * 
 *                    4     6     8     10    12     14 
 *    4. let r = C1*x +C2*x +C3*x +C4*x +C5*x  +C6*x  , then
 *           cos(x) = 1 - x*x/2 + r
 *       since cos(x+y) ~ cos(x) - sin(x)*y 
 *              ~ cos(x) - x*y,
 *       a correction term is necessary in cos(x) and hence
 *        cos(x+y) = 1 - (x*x/2 - (r - x*y))
 *       For better accuracy when x > 0.3, let qx = |x|/4 with
 *       the last 32 bits mask off, and if x > 0.78125, let qx = 0.28125.
 *       Then
 *        cos(x+y) = (1-qx) - ((x*x/2-qx) - (r-x*y)).
 *       Note that 1-qx and (x*x/2-qx) is EXACT here, and the
 *       magnitude of the latter is at least a quarter of x*x/2,
 *       thus, reducing the rounding error in the subtraction.
 */
#ifdef __STDC__
static const double 
#else
static double 
#endif
C1  =  4.16666666666666019037e-02, /* 0x3FA55555, 0x5555554C */
C2  = -1.38888888888741095749e-03, /* 0xBF56C16C, 0x16C15177 */
C3  =  2.48015872894767294178e-05, /* 0x3EFA01A0, 0x19CB1590 */
C4  = -2.75573143513906633035e-07, /* 0xBE927E4F, 0x809C52AD */
C5  =  2.08757232129817482790e-09, /* 0x3E21EE9E, 0xBDB4B1C4 */
C6  = -1.13596475577881948265e-11; /* 0xBDA8FAE9, 0xBE8838D4 */

#ifdef __STDC__
  double __kernel_cos(double x, double y)
#else
  double __kernel_cos(x, y)
  double x,y;
#endif
{
  double a,hz,z,r,qx;
  int ix;
  ix = MSAL_HI(x)&0x7fffffff;    /* ix = |x|'s high word*/
  if(ix<0x3e400000) {            /* if x < 2**27 */
    if(((int)x)==0) return one;        /* generate inexact */
  }
  z  = x*x;
  r  = z*(C1+z*(C2+z*(C3+z*(C4+z*(C5+z*C6)))));
  if(ix < 0x3FD33333)             /* if |x| < 0.3 */ 
    return one - (0.5*z - (z*r - x*y));
  else {
    if(ix > 0x3fe90000) {        /* x > 0.78125 */
      qx = 0.28125;
    } else {
      MSAL_HI(qx) = (unsigned)(ix-0x00200000);    /* x/4 */
      MSAL_LO(qx) = 0;
    }
    hz = 0.5*z-qx;
    a  = one-qx;
    return a - (hz - (z*r-x*y));
  }
}



/* INDENT OFF */
/* __kernel_tan( x, y, k )
 * kernel tan function on [-pi/4, pi/4], pi/4 ~ 0.7854
 * Input x is assumed to be bounded by ~pi/4 in magnitude.
 * Input y is the tail of x.
 * Input k indicates whether tan (if k = 1) or -1/tan (if k = -1) is returned.
 *
 * Algorithm
 *    1. Since tan(-x) = -tan(x), we need only to consider positive x.
 *    2. if x < 2^-28 (hx<0x3e300000 0), return x with inexact if x!=0.
 *    3. tan(x) is approximated by a odd polynomial of degree 27 on
 *       [0,0.67434]
 *                       3             27
 *           tan(x) ~ x + T1*x + ... + T13*x
 *       where
 *
 *             |tan(x)         2     4            26   |     -59.2
 *             |----- - (1+T1*x +T2*x +.... +T13*x    )| <= 2
 *             |  x                     |
 *
 *       Note: tan(x+y) = tan(x) + tan'(x)*y
 *                  ~ tan(x) + (1+x*x)*y
 *       Therefore, for better accuracy in computing tan(x+y), let
 *             3      2      2       2       2
 *        r = x *(T2+x *(T3+x *(...+x *(T12+x *T13))))
 *       then
 *                     3    2
 *        tan(x+y) = x + (T1*x + (x *(r+y)+y))
 *
 *      4. For x in [0.67434,pi/4],  let y = pi/4 - x, then
 *        tan(x) = tan(pi/4-y) = (1-tan(y))/(1+tan(y))
 *               = 1 - 2*(tan(y) - (tan(y)^2)/(1+tan(y)))
 */

static const double T[] = {
         3.33333333333334091986e-01,    /* 3FD55555, 55555563 */
         1.33333333333201242699e-01,    /* 3FC11111, 1110FE7A */
         5.39682539762260521377e-02,    /* 3FABA1BA, 1BB341FE */
         2.18694882948595424599e-02,    /* 3F9664F4, 8406D637 */
         8.86323982359930005737e-03,    /* 3F8226E3, E96E8493 */
         3.59207910759131235356e-03,    /* 3F6D6D22, C9560328 */
         1.45620945432529025516e-03,    /* 3F57DBC8, FEE08315 */
         5.88041240820264096874e-04,    /* 3F4344D8, F2F26501 */
         2.46463134818469906812e-04,    /* 3F3026F7, 1A8D1068 */
         7.81794442939557092300e-05,    /* 3F147E88, A03792A6 */
         7.14072491382608190305e-05,    /* 3F12B80F, 32F0A7E9 */
        -1.85586374855275456654e-05,    /* BEF375CB, DB605373 */
         2.59073051863633712884e-05,    /* 3EFB2A70, 74BF7AD4 */
/* one */     1.00000000000000000000e+00,    /* 3FF00000, 00000000 */
/* pio4 */     7.85398163397448278999e-01,    /* 3FE921FB, 54442D18 */
/* pio4lo */     3.06161699786838301793e-17    /* 3C81A626, 33145C07 */
};
#define    pio4    T[14]
#define    pio4lo    T[15]
/* INDENT ON */

double
__kernel_tan(double x, double y, int iy) {
    double z, r, v, w, s;
    int ix, hx;
    hx = (int)MSAL_HI(x);        /* high word of x */
    ix = hx & 0x7fffffff;            /* high word of |x| */
    if (ix < 0x3e300000) {            /* x < 2**-28 */
        if ((int) x == 0) {        /* generate inexact */
            if (((ix | (int)MSAL_LO(x)) | (iy + 1)) == 0) {
                return one / msal_fabs(x);
            } else {
                if (iy == 1) {
                    return x;
                } else {    /* compute -1 / (x+y) carefully */
                    double a, t;

                    z = w = x + y;
                    MSAL_LO(z) = 0;
                    v = y - (z - x);
                    t = a = -one / w;
                    MSAL_LO(t) = 0;
                    s = one + t * z;
                    return t + a * (s + t * v);
                }
            }
        }
    }
    if (ix >= 0x3FE59428) {    /* |x| >= 0.6744 */
        if (hx < 0) {
            x = -x;
            y = -y;
        }
        z = pio4 - x;
        w = pio4lo - y;
        x = z + w;
        y = 0.0;
    }
    z = x * x;
    w = z * z;
    /*
     * Break x^5*(T[1]+x^2*T[2]+...) into
     * x^5(T[1]+x^4*T[3]+...+x^20*T[11]) +
     * x^5(x^2*(T[2]+x^4*T[4]+...+x^22*[T12]))
     */
    r = T[1] + w * (T[3] + w * (T[5] + w * (T[7] + w * (T[9] +
        w * T[11]))));
    v = z * (T[2] + w * (T[4] + w * (T[6] + w * (T[8] + w * (T[10] +
        w * T[12])))));
    s = z * x;
    r = y + z * (s * (r + v) + y);
    r += T[0] * s;
    w = x + r;
    if (ix >= 0x3FE59428) {
        v = (double) iy;
        return (double) (1 - ((hx >> 30) & 2)) *
            (v - 2.0 * (x - (w * w / (w + v) - r)));
    }
    if (iy == 1) {
        return w;
    } else {
        /*
         * if allow error up to 2 ulp, simply return
         * -1.0 / (x+r) here
         */
        /* compute -1.0 / (x+r) accurately */
        double a, t;
        z = w;
        MSAL_LO(z) = 0;
        v = r - (z - x);    /* z+v = r+x */
        t = a = -1.0 / w;    /* a = -1.0/w */
        MSAL_LO(t) = 0;
        s = 1.0 + t * z;
        return t + a * (s + t * v);
    }
}



/* @(#)s_sin.c 1.3 95/01/18 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* sin(x)
 * Return sine function of x.
 *
 * kernel function:
 *    __kernel_sin        ... sine function on [-pi/4,pi/4]
 *    __kernel_cos        ... cose function on [-pi/4,pi/4]
 *    __ieee754_rem_pio2    ... argument reduction routine
 *
 * Method.
 *      Let S,C and T denote the sin, cos and tan respectively on 
 *    [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
 *    in [-pi/4 , +pi/4], and let n = k mod 4.
 *    We have
 *
 *          n        sin(x)      cos(x)        tan(x)
 *     ----------------------------------------------------------
 *          0           S           C            T
 *          1           C          -S           -1/T
 *          2          -S          -C            T
 *          3          -C           S           -1/T
 *     ----------------------------------------------------------
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *
 * Accuracy:
 *    TRIG(x) returns trig(x) nearly rounded 
 */
#ifdef __STDC__
    double msal_sin(double x)
#else
    double msal_sin(x)
    double x;
#endif
{
    double y[2],z=0.0;
    int n, ix;

    /* High word of x. */
    ix = (int)MSAL_HI(x);

    /* |x| ~< pi/4 */
    ix &= 0x7fffffff;
    if(ix <= 0x3fe921fb) return __kernel_sin(x,z,0);

    /* sin(Inf or NaN) is NaN */
    else if (ix>=0x7ff00000) return x-x;

    /* argument reduction needed */
    else {
        n = __ieee754_rem_pio2(x,y);
        switch(n&3) {
        case 0: return  __kernel_sin(y[0],y[1],1);
        case 1: return  __kernel_cos(y[0],y[1]);
        case 2: return -__kernel_sin(y[0],y[1],1);
        default:
            return -__kernel_cos(y[0],y[1]);
        }
    }
}

/* cos(x)
 * Return cosine function of x.
 *
 * kernel function:
 *    __kernel_sin        ... sine function on [-pi/4,pi/4]
 *    __kernel_cos        ... cosine function on [-pi/4,pi/4]
 *    __ieee754_rem_pio2    ... argument reduction routine
 *
 * Method.
 *      Let S,C and T denote the sin, cos and tan respectively on 
 *    [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
 *    in [-pi/4 , +pi/4], and let n = k mod 4.
 *    We have
 *
 *          n        sin(x)      cos(x)        tan(x)
 *     ----------------------------------------------------------
 *        0           S       C         T
 *        1           C      -S        -1/T
 *        2          -S      -C         T
 *        3          -C       S        -1/T
 *     ----------------------------------------------------------
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *
 * Accuracy:
 *    TRIG(x) returns trig(x) nearly rounded 
 */
#ifdef __STDC__
  double msal_cos(double x)
#else
  double msal_cos(x)
  double x;
#endif
{
  double y[2],z=0.0;
  int n, ix;

  /* High word of x. */
  ix = (int)MSAL_HI(x);

  /* |x| ~< pi/4 */
  ix &= 0x7fffffff;
  if(ix <= 0x3fe921fb) return __kernel_cos(x,z);

  /* cos(Inf or NaN) is NaN */
  else if (ix>=0x7ff00000) return x-x;

  /* argument reduction needed */
  else {
    n = __ieee754_rem_pio2(x,y);
    switch(n&3) {
    case 0: return  __kernel_cos(y[0],y[1]);
    case 1: return -__kernel_sin(y[0],y[1],1);
    case 2: return -__kernel_cos(y[0],y[1]);
    default:
            return  __kernel_sin(y[0],y[1],1);
    }
  }
}



/* tan(x)
 * Return tangent function of x.
 *
 * kernel function:
 *    __kernel_tan        ... tangent function on [-pi/4,pi/4]
 *    __ieee754_rem_pio2    ... argument reduction routine
 *
 * Method.
 *      Let S,C and T denote the sin, cos and tan respectively on 
 *    [-PI/4, +PI/4]. Reduce the argument x to y1+y2 = x-k*pi/2 
 *    in [-pi/4 , +pi/4], and let n = k mod 4.
 *    We have
 *
 *          n        sin(x)      cos(x)        tan(x)
 *     ----------------------------------------------------------
 *        0           S       C         T
 *        1           C      -S        -1/T
 *        2          -S      -C         T
 *        3          -C       S        -1/T
 *     ----------------------------------------------------------
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *
 * Accuracy:
 *    TRIG(x) returns trig(x) nearly rounded 
 */

#ifdef __STDC__
  double msal_tan(double x)
#else
  double msal_tan(x)
  double x;
#endif
{
  double y[2],z=0.0;
  int n, ix;
  /* High word of x. */
  ix = (int)MSAL_HI(x);
  /* |x| ~< pi/4 */
  ix &= 0x7fffffff;
  if (ix <= 0x3fe921fb) {
    return __kernel_tan(x,z,1);
  }

  /* tan(Inf or NaN) is NaN */
  else if (ix>=0x7ff00000) return x-x;        /* NaN */

  /* argument reduction needed */
  else {
    n = __ieee754_rem_pio2(x,y);
    return __kernel_tan(y[0],y[1],1-((n&1)<<1)); /*   1 -- n even
                                                     -1 -- n odd */
  }
}

/* Your typical Taylor series sine-cosine routine */
#ifdef __STDC__
  long double ldbl_sincos(long double xq, long double zq, long double uq,
    double i)
#else
  long double ldbl_sincos(xq, zq, uq, i)
  long double xq;
  long double zq;
  long double uq;
  double i;
#endif
{
  long double k;
  int n;
  long double prev;
  xq = xq * xq; /* x^2 */
  prev = zq;
  k = 2.0;
  for(n=30; n>0; n--) {
    /* char out[40]; f107_spfg(out, sizeof(out), 0, 32, zq); */
    /* printf("%2d %s\n", n, out); */
    uq = -uq * xq/(k*(k+i)); /* -x^2/2 or -x^3/6 */
    zq = zq + uq;
    if (zq == prev) {
      n = 0; /* exit loop early */
    } else {
      prev = zq;
      k += 2.0;
    }
  }
  return zq;
}

static long double ld_1o2pi = 0.63661977236758134307553;
static long double ld_p05 = 1.570796326794896619231;
static long double ld_pi = 3.141592653589793238462643;
static long double ld_2pi = 6.28318530717958647692529;


#ifdef __STDC__
    long double msal_sinl(long double x)
#else
    long double msal_sinl(x)
    long double x;
#endif
{
  long i;
  int neg = 0;

  /* for x<0, sin(x) = -sin(-x) */
  if (x < 0.0) {
    x = -x;
    neg = 1;
  }
  /* remove factor of 2 pi */
  if (x >= ld_2pi) {
    i = (long) (x * ld_1o2pi);
    x = x - ld_2pi * ((long double) i);
  }
  /* for x in [pi..2pi], sin(x)=-sin(x - pi) */
  if (x > ld_pi) {
    x = x - ld_pi;
    neg = 1-neg;
  }
  /* for x in [pi/2..pi], sin(x)=sin(pi-x)  */
  if (x > ld_p05) {
    x = ld_pi - x;
  }

  if (neg) {
    return(- ldbl_sincos(x, x, x, 1.0));
  }
  return(ldbl_sincos(x, x, x, 1.0));
}

#ifdef __STDC__
    long double msal_cosl(long double x)
#else
    long double msal_cosl(x)
    long double x;
#endif
{
  long i;
  int neg = 0;
  /* for x<0, cos(x) = cos(-x) */
  if (x < 0.0) {
    x = -x;
  }
  /* remove factor of 2 pi */
  if (x >= ld_2pi) {
    i = (long) (x * ld_1o2pi);
    x = x - ld_2pi * ((long double) i);
  }
  /* for x in [pi..2pi], cos(x)=cos(2 pi - x) */
  if (x > ld_pi) {
    x = ld_2pi - x;
  }
  /* for x in [pi/2..pi], cos(x)=-cos(pi-x)  */
  if (x > ld_p05) {
    x = ld_pi - x;
    neg = 1;
  }
  if (neg) {
    return(- ldbl_sincos(x, 1.0, 1.0, -1.0));
  }
  return(ldbl_sincos(x, 1.0, 1.0, -1.0));
}

#ifdef __STDC__
    long double msal_tanl(long double x)
#else
    long double msal_tanl(x)
    long double x;
#endif
{
  return(msal_sinl(x)/msal_cosl(x));
}


 /* -------------------------------------------------------------------------.
 |       _             _              _  _   ___                             |
 |       |   ._  ._ _  |_   _  ._ |   |  |   |_  . . ._   _ | "  _  ._       |
 |       | , ,-| | | | | ) (-` |  T   |/\|   |   | | | | (  T | ( ) | |      |
 |       ~~~ `~` ~ ~ ~ '~   ~' ~  `   ~  ~   ~    ~  ~ ~  ~ ` ~  ~  ~ ~      |
 |                                                                           |
 `------------------------------------------------------------------------- */

/*   This code for the Lambert W function is derived from the work by
     Darko Veberic. This version is heavily adapted, but still has bits
     in common (most notably the coefficients) with the original. That
     source code came with this copyright notice:

  Copyright (C) 2009 Darko Veberic, darko.veberic@ung.si

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/* msal_w_approx0 gives an initial estimate for the W_0 or W_-1 branch,
   suitable over the domain (-1/e, -0.32358) */
#ifdef __STDC__
  double msal_w_approx0(double x, int branch)
#else
  double msal_w_approx0(x, branch)
  double x;
  int branch;
#endif
{
  double esign, p;

  if (branch == 0) {
    esign = 1.0;
  } else {
    esign = -1.0;
  }
  p = esign * sqrt(2.0*(1.0 + MSAL_E*x));
  return      /* The coefficients are from Veberic's 2010 paper at */
       -1 +   /* arXiv:1003.1628v1 see the table right after eqn (37) */
    p*( 1 +
    p*(-0.333333333333333333e0 +  /*     -1/3        */
    p*( 0.152777777777777777e0 +  /*     11/72       */
    p*(-0.79629629629629630e-1 +  /*    -43/540      */
    p*( 0.44502314814814814e-1 +  /*    769/17280    */
    p*(-0.25984714873603760e-1 +  /*   -221/8505     */
    p*( 0.15635632532333920e-1    /* 680863/43545600 */
    )))))));
}

/* msal_w0_approx1 gives an initial estimate for the W_0 branch, suitable
   over the domain (-1/e, 8) */
#ifdef __STDC__
  double msal_w0_approx1(double x)
#else
  double msal_w0_approx1(x)
  double x;
#endif
{
  double est, a1, a2, a3, a4, b1, b2, b3, b4;

  if (x < 0.145) {
    a1 = 5.931375839364438;
    a2 = 11.39220550532913;
    a3 = 7.33888339911111;
    a4 = 0.653449016991959;
    b1 = 6.931373689597704;
    b2 = 16.82349461388016;
    b3 = 16.43072324143226;
    b4 = 5.115235195211697;
  } else {
    a1 = 2.445053070726557;
    a2 = 1.343664225958226;
    a3 = 0.148440055397592;
    a4 = 0.0008047501729130;
    b1 = 3.444708986486002;
    b2 = 3.292489857371952;
    b3 = 0.916460018803122;
    b4 = 0.0530686404483322;
  }

  est = x * (1.0 + a1*(x + a2 *(x + a3 *(x + a4*x))))
          / (1.0 + b1*(x + b2 *(x + b3 *(x + b4*x))));

  return est;
}

/* msal_w0_approx2 gives an initial estimate for the W_0 branch, suitable
   over the domain (8, inf) */
#ifdef __STDC__
  double msal_w0_approx2(double x)
#else
  double msal_w0_approx2(x)
  double x;
#endif
{
  double a, b, ia;

  a = log(x);
  b = log(a);
  ia = 1.0 / a;

  return a - b + b / a *
    (1.0 + ia *
      (0.5*(-2.0 + b) + ia *
        (1.0/6.0*(6.0 + b*(-9.0 + b*2.0)) + ia *
          (1.0/12.0*(-12.0 + b*(36.0 + b*(-22.0 + b*3.0))) + ia *
            1.0/60.0*(60.0 + b*(-300.0 + b*(350.0 + b*(-125.0 + b*12.0))))
          )
        )
      )
    );
}

#ifdef __STDC__
  double msal_lambertw(double x)
#else
  double msal_lambertw(x)
  double x;
#endif
{
  double a1, a2;
  double est, ee, s, t, u;
  int i, mh;

  if (x < (-1.0/MSAL_E)) {
    a1 = huge * huge; /* Inf */
    a2 = (-1.0 * huge) * huge; /* Ninf */
    return (a1+a2); /* Nan */
  }

  mh = 3;
  if (x < -0.32358170806015724) {
    est = msal_w_approx0(x, 0);
  } else if (x < 8.707) {
    est = msal_w0_approx1(x);
    if (x < -0.25) { mh = 4; }
  } else {
    est = msal_w0_approx2(x);
  }

  /* Perform Halley's iteration */
  for(i=0; i<mh; i++) {
    ee = exp(est);
    t = est*ee - x;
    s = (est+2.0)/(2.0*(est+1.0));
    u = (est+1.0)*ee;
    est = est + t/(s*t-u);
  }

  return est;
}

#ifdef __STDC__
  long double msal_lambertwl(long double x)
#else
  long double msal_lambertwl(x)
  long double x;
#endif
{
  long double a1, a2;
  long double est, ee, s, t, u;
  int i, mh;

  if (x < (-1.0/MSAL_E)) {
    a1 = huge * huge; /* Inf */
    a2 = (-1.0 * huge) * huge; /* Ninf */
    return (a1+a2); /* Nan */
  }

  mh = 4;
  if (x < -0.32358170806015724) {
    est = msal_w_approx0((double)x, 0);
  } else if (x < 8.707) {
    est = msal_w0_approx1((double)x);
    if (x < -0.25) { mh = 5; }
  } else {
    est = msal_w0_approx2((double)x);
  }

  /* Perform Halley's iteration */
  for(i=0; i<mh; i++) {
    ee = expl(est);
    t = est*ee - x;
    s = (est+2.0)/(2.0*(est+1.0));
    u = (est+1.0)*ee;
    est = est + t/(s*t-u);
  }

  return est;
}

 /* -------------------------------------------------------------------------.
 |            _                        ___                                   |
 |           / ` ._  ._ _  ._ _  ._    |_  . . ._   _ | "  _  ._             |
 |           \ T ,-| | | | | | | ,-|   |   | | | | (  T | ( ) | |            |
 |            ~  `~` ~ ~ ~ ~ ~ ~ `~`   ~    ~  ~ ~  ~ ` ~  ~  ~ ~            |
 |                                                                           |
 `------------------------------------------------------------------------- */

/*

The easiest way to provide Gamma in RIES is by the Lanczos
approximation.

Older source code, and other relevant info is in:
   .../pub/ries/lanczos-gamma.rhtf
   .../pub/ries/gamma-sources.hid

When I go to quad precision, new Lanczos coefficients need to be
generated; a program to do this is in
.../proj/rily-pi/tars/viktor-toth-lanczos-gamma-function.tgz

An unresolved issue is what to do about quad-precision digamma. The de
Moivre coefficients are probably not good enough. I might be able to
get away with it due to the fact that iterated Newton works even when
the derivative functions have error. Digamma functions can be tested
with Gauss' digamma theorem, which gives an exact formula for all
"proper fractions" m/k (i.e., m and k are integers with 0<m<k), and
"euler" is the Euler-Mascheroni constant:

    digamma(m/k) = - euler
                   - ln(2 k)
                   - pi/2 * cot(pi*m/k)
                   + 2 SIGMA_[n=1 .. floor((k-1)/2)]
                         cos(2*pi*n*m/k) ln(sin(pi*n/k))

For higher precision Gamma, I can use Bill Gosper's proposal:
 [
 From: "Bill Gosper" <address removed>
 To: "math-fun mailing list" <address removed>
 Date: Sun, 8 Jan 2012 23:24:37 -0800
 Subject: Re: [math-fun] Gamma function (formulas collection)

I missed where Warren's paper probably says that, for "unrestricted"
(arbitrary precision) numerics, the divergence of the log Gamma and
polygamma series is rather irrelevant. A plausible algorithm is simply
to add some positive integer n to z so that the Stirling series z+n
swoops within the desired accuracy. Then divide by (z+1)(z+2)...(z+n).
Windschitl's and Warren's improvements make this even more attractive.
Looking to provide Gamma numerics for Robert's RIES, I accidentally
overkilled the problem by Remezing up

f[z]:=(0.011987912746518014561506488044010 +
   z^2 (-0.028678142684262583619372798888834 +
      z^2 (-0.10376325088888750555218325558841 +
         z^2 (-0.034612435282031905677869242849747 -
            0.0024691358024691358024691358024691 z^2))))/(z^7 *
(45.984169004371332413538940188341 +
     z^2 (54.768197084214553421696753890699 +
        z^2 (14.960893432079977923321408051895 + z^2))))

Then plotting

z! /(Sqrt[2 Pi] (E^(-f[z] - 4 z) z^(2 + 6 z) Sinh[1/z]^(2 z))^(1/4) -1

9 < z < oo shows maximum error 6*10^-26 (at only one ripple because I
neglected the weight function). So up to nine divides could be
required for this method. But RIES only needs 10^-17, for which f[z]
could be a lot simpler and n smaller than 9.

To squeeze some actual fun out of this:  Why is f an odd function?

--rwg

PS, I wasn't confused in that 2001 Remez posting--just confusing. In
the next sentences I explained why truncating the Chebys doesn't
minimax. I was just using "Chebychef optimal" to mean the best you
could do by subtracting off Chebys.
 ]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

  /* The following constants LG_g and LG_N are the "g" and "n" parameters
  /  for the table of coefficients that follows them; several alternative
  /  sets of coefficients are available at:
  /  http://mrob.com/pub/ries/lanczos-gamma.html                    */
#if 1
/* A lower-precision version from "Numerical Recipes" */
# define LG_g 5.0      /* Lanczos parameter "g" */
# define LG_N 6        /* Range of coefficients i=[0..N] */
  const double lct[LG_N+1] = {
     1.000000000190015,
    76.18009172947146,
   -86.50532032941677,
    24.01409824083091,
    -1.231739572450155,
     0.1208650973866179e-2,
    -0.5395239384953e-5
  };

  const double lcnum[LG_N+1] = {
    8.7919924759596111106039069839910954707405381645486e7,
    2.9416541482343776499806562571350221096212266811349e9,
    4.100006789687650376061766307985658465119862514162e10,
    3.0470491395554593783543149688032484055692798817589e11,
    1.2735125543888013032240994786889999327005736243542e12,
    2.8381487223293835117356885925163088875090863795676e12,
    2.6349245000341171829201220803164421672401218788693e12
  };

  const double lcden[LG_N+1] = {
    8.7919924742890022080764214474411305172689073093183e7,
    1.8463184196006904636960485039626374086264705349568e9,
    1.5385986830005753864133737533021978405220587791307e10,
    6.4621144686024166229361697638692309301926468723489e10,
    1.4278195778245339585916108430644395960044705470333e11,
    1.5509074724645799895046807433286154232462352493637e11,
    6.3302345814880815898150234421576139724336132627092e10
  };
#elif 0
/* A version that was used in the GNU scientific library */
# define LG_g 7.0      /* Lanczos parameter "g" */
# define LG_N 8        /* Range of coefficients i=[0..N] */
  const double lct[LG_N+1] = {
     0.99999999999980993227684700473478,
   676.520368121885098567009190444019,
 -1259.13921672240287047156078755283,
   771.3234287776530788486528258894,
  -176.61502916214059906584551354,
    12.507343278686904814458936853,
    -0.13857109526572011689554707,
     9.984369578019570859563e-6,
     1.50563273514931155834e-7
  };
#elif 0
/* From Godfrey, 2001. He says this version gives 13 digits of accuracy. */
# define LG_g 9.0      /* Lanczos parameter "g" */
# define LG_N 10       /* Range of coefficients i=[0..N] */
  const double lct[LG_N+1] = {
        1.000000000000000174663,
     5716.400188274341379136,
   -14815.30426768413909044,
    14291.49277657478554025,
    -6348.160217641458813289,
     1301.608286058321874105,
     -108.1767053514369634679,
        2.605696505611755827729,
       -0.7423452510201416151527e-2,
        0.5384136432509564062961e-7,
       -0.4023533141268236372067e-8
  };
#else
/* From Godfrey, 2001. He says this version gives 15 digits of accuracy. */
# define LG_g 4.7421875    /* Lanczos parameter "g" */
# define LG_N 14           /* Range of coefficients i=[0..N] */
  const double lct[LG_N+1] = {
     0.99999999999999709182,
    57.156235665862923517,
   -59.597960355475491248,
    14.136097974741747174,
    -0.49191381609762019978,
      .33994649984811888699e-4,
      .46523628927048575665e-4,
     -.98374475304879564677e-4,
      .15808870322491248884e-3,
     -.21026444172410488319e-3,
      .21743961811521264320e-3,
     -.16431810653676389022e-3,
      .84418223983852743293e-4,
     -.26190838401581408670e-4,
      .36899182659531622704e-5
  };
#endif
const double ln_sqrt_2_pi = 0.91893853320467274178;
const double         g_pi = 3.14159265358979323846;

/* Compute the logarithm of the Gamma function using the Lanczos method. */
double msal_lanczos_ln_gamma(double z)
{
  double sum, num, den;
  double base;

  int i;
  if (z < 0.5) {
    /* Use Euler's reflection formula:
       Gamma(z) = Pi / [Sin[Pi*z] * Gamma[1-z]]; */
    return log(g_pi / msal_sin(g_pi * z)) - msal_lanczos_ln_gamma(1.0 - z);
  }
  z = z - 1.0;
  base = z + LG_g + 0.5;  /* Base of the Lanczos exponential */

#ifdef GAMMA_LANCZOS_METHOD
  sum = 0;
  /* We start with the terms that have the smallest coefficients and largest
  /  denominator. */
  for(i=LG_N; i>=1; i--) {
    sum += lct[i] / (z + ((double) i));
  }
  sum = sum + lct[0];
#else
  /* Use the Boost C++ library method */
  num = lcnum[0]; den = lcden[0];
  for(i=1; i<=LG_N; i++) {
    num = num*z + lcnum[i]; den = den*z + lcden[i];
  }
  sum = num/den;
#endif

  /* This printf is just for debugging
  printf("ls2p %7g  l(b^e) %7g   -b %7g  l(s) %7g\n", ln_sqrt_2_pi,
            log(base)*(z+0.5), -base, log(sum)); */
  /* Gamma[z] = Sqrt(2*Pi) * sum * base^[z + 0.5] / E^base */
  return ((ln_sqrt_2_pi + log(sum)) - base) + log(base)*(z+0.5);
}

/* Compute the Gamma function, which is e to the power of ln_gamma. */
double msal_lanczos_gamma(double z)
{
  return(exp(msal_lanczos_ln_gamma(z)));
}

/* The digamma function is the derivative of gammaln.
/
/  Reference:
/   J Bernardo,
/   Psi ( Digamma ) Function,
/   Algorithm AS 103,
/   Applied Statistics,
/   Volume 25, Number 3, pages 315-317, 1976.
/
/   From http://www.psc.edu/~burkardt/src/dirichlet/dirichlet.f
/   (with modifications for negative numbers and extra precision)  */
double msal_digamma(double x)
{
  double neginf = -INFINITY;
  static const double c = 12,
    digamma1 = -0.57721566490153286,
    trigamma1 = 1.6449340668482264365, /* pi^2/6 */
    s = 1e-6,
    s3 = 1./12,
    s4 = 1./120,
    s5 = 1./252,
    s6 = 1./240,
    s7 = 1./132;
  /* static const double s8 = 691./32760,
       s9 = 1./12, s10 = 3617./8160; */
  double result;
  /* Illegal arguments */
  if((x == neginf) || isnan(x)) {
    return NAN;
  }
  /* Singularities */
  if((x <= 0) && (floor(x) == x)) {
    return neginf;
  }
  /* Negative values
  /  Use the reflection formula (Jeffrey 11.1.6):
  /  digamma(-x) = digamma(x+1) + pi*cot(pi*x)
  / 
  /  This is related to the identity
  /  digamma(-x) = digamma(x+1) - digamma(z) + digamma(1-z)
  /  where z is the fractional part of x
  /  For example:
  /  digamma(-3.1) = 1/3.1 + 1/2.1 + 1/1.1 + 1/0.1 + digamma(1-0.1)
  /                = digamma(4.1) - digamma(0.1) + digamma(1-0.1)
  /  Then we use
  /  digamma(1-z) - digamma(z) = pi*cot(pi*z)
  */
  if(x < 0) {
    return msal_digamma(1-x) + g_pi/msal_tan(-g_pi*x);
  }
  /* Use Taylor series if argument <= S */
  if(x <= s) return digamma1 - 1/x + trigamma1*x;
  /* Reduce to digamma(X + N) where (X + N) >= C */
  result = 0;
  while(x < c) {
    result -= 1/x;
    x++;
  }
  /* Use de Moivre's expansion if argument >= C
  /  This expansion can be computed in Maple via asympt(Psi(x),x) */
  if(x >= c) {
    double r = 1/x, t;
    result += log(x) - 0.5*r;
    r *= r;
    /* this version for lame compilers */
    t = (s5 - r * (s6 - r * s7));
    result -= r * (s3 - r * (s4 - r * t));
  }
  return result;
}

double msal_dgamma(double z)
{
  return(msal_digamma(z)*msal_lanczos_gamma(z));
}


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
|                                 Test functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

void msal_test_lambert(void)
{
  double x, w, v, er;
  x = -0.375; /* Deliberately less than -1/e */
  while (x < 1.0e20) {
    printf("W(%.9g) = ", x);
    w = msal_lambertw(x);
    printf("%.17g", w);
    v = w*exp(w); er = (x-v)/(1.0+x);
    printf(" err ");
    printf("%.17g", er);
    printf("\n");

    /* With these increments and limits all the x values will
       be "round numbers" in binary */
    if (x<-0.3125) { x+=0.0078125; }
    else if (x<2.0) { x+=0.25; }
    else if (x<=1000.0) { x*=2.0; }
    else if (x<=30000.0) { x*=5.0; }
    else { x *= 10.0; }
  }
}

void msal_test_gamma(void)
{
  double z, g, g2, dz, dg, di;
  dz = 0.01;
  for(z=0.5; z<=6.0; z+=0.5) {
    g = msal_lanczos_gamma(z);
    g2 = msal_lanczos_gamma(z+dz);
    dg = (g2-g)/dz;
    di = (msal_dgamma(z)+msal_dgamma(z+dz))/2.0;;
    printf("Gamma[%.1f] ~ %15.12g; diff %g, digamma %15.12g\n",
           z, g, dg, di);
  }
}


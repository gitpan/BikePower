#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#ifdef __cplusplus
}
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* XXX andere Compiler ? */
#ifndef __inline__
#ifndef __GNUC__
#define __inline__
#endif /* __GNUC__ */
#endif /* __inline__ */

#undef MYDEBUG

/* XXX Bei Bedarf die folgenden Makros zusammenfassen */
#define BIKEPOWER_ACCSTRINGNUMBER_VAR(member,var) \
	  ENTER; \
	  SAVETMPS; \
	  PUSHMARK(SP); \
	  XPUSHs(self); \
	  PUTBACK; \
	  count = perl_call_method(#member, G_SCALAR); \
	  SPAGAIN; \
	  if (count != 1) \
	    croak("method call " #member " returned nothing"); \
	  { SV *s; s = POPs; var = atof(SvPV(s,na)); }\
	  PUTBACK; \
	  FREETMPS; \
	  LEAVE;

#define BIKEPOWER_ACCSTRING_VAR(member,var) \
	  ENTER; \
	  SAVETMPS; \
	  PUSHMARK(SP); \
	  XPUSHs(self); \
	  PUTBACK; \
	  count = perl_call_method(#member, G_SCALAR); \
	  SPAGAIN; \
	  if (count != 1) \
	    croak("method call " #member " returned nothing"); \
	  { SV *s; s = POPs; strncpy(var, SvPV(s,na), 10); }\
	  PUTBACK; \
	  FREETMPS; \
	  LEAVE;

#define BIKEPOWER_ACCBOOL_VAR(member,var) \
	  ENTER; \
	  SAVETMPS; \
	  PUSHMARK(SP); \
	  XPUSHs(self); \
	  PUTBACK; \
	  count = perl_call_method(#member, G_SCALAR); \
	  SPAGAIN; \
	  if (count != 1) \
	    croak("method call " #member " returned nothing"); \
	  { SV *s; s = POPs; var = SvTRUE(s); }\
	  PUTBACK; \
	  FREETMPS; \
	  LEAVE;

#define BIKEPOWER_ACC_VAR(member,type,var) \
	  ENTER; \
	  SAVETMPS; \
	  PUSHMARK(SP); \
	  XPUSHs(self); \
	  PUTBACK; \
	  count = perl_call_method(#member, G_SCALAR); \
	  SPAGAIN; \
	  if (count != 1) \
	    croak("method call " #member " returned nothing"); \
	  var = POP ## type; \
	  PUTBACK; \
	  FREETMPS; \
	  LEAVE;

#define BIKEPOWER_ACC(member,type) \
	BIKEPOWER_ACC_VAR(member,type,member)

#define BIKEPOWER_MUT(member,arg) \
	  ENTER; \
	  SAVETMPS; \
	  PUSHMARK(SP); \
	  XPUSHs(self); \
	  XPUSHs(sv_2mortal(arg)); \
	  PUTBACK; \
	  count = perl_call_method(#member, G_DISCARD); \
	  FREETMPS; \
	  LEAVE; \

#define SQR(a) ((a)*(a))

typedef SV* BikePower;

MODULE = BikePower		PACKAGE = BikePower

PROTOTYPES: DISABLE

void
calcXS(self)
	BikePower self;

	PREINIT:
	double eff_H, A_c, R, A2;
	double F_a, F_r, F_g, F, BM, consumption, human_efficiency;
	double headwind;
	int cross_wind, imperial;
	char *A_c_str, *R_str, given[10];
	double V_lo = 0, V = 64, V_hi = 128;
	double A1, transmission_efficiency, grade, total_weight_N, power;
	double P_try, P_t;
	HV *out;
	SV **tmp;
	int count;
	
	CODE:
	dSP;

	BIKEPOWER_ACC(headwind,n);
	BIKEPOWER_ACC(cross_wind,i);
	eff_H = headwind * (cross_wind ? .7 : 1);
	BIKEPOWER_ACCSTRINGNUMBER_VAR(A_c,A_c);
	BIKEPOWER_ACCSTRINGNUMBER_VAR(R,R);
	BIKEPOWER_ACC_VAR(calc_A2,n,A2);
	BIKEPOWER_ACCSTRING_VAR(given,given);
	BIKEPOWER_ACC(A1,n);
	BIKEPOWER_ACC(total_weight_N,n);
	BIKEPOWER_ACC(grade,n);
	BIKEPOWER_ACC(transmission_efficiency,n);

	if (*given == 'P' || *given == 'C') {
	  /* Given P, solve for V by bisection search
	     True Velocity lies in the interval [V_lo, V_hi].
	     */
	  BIKEPOWER_ACC(power,n);

	  while (V - V_lo > 0.001) {
	    F_a = A2 * SQR(V+eff_H) + A1 * (V + eff_H);
	    if (V + eff_H < 0)
	      F_a *= -1;
	    P_try = (V/transmission_efficiency) * 
	      (F_a + (R + grade) * total_weight_N);
	    if (P_try < power)
	      V_lo = V;
	    else
	      V_hi = V;
	    V = 0.5 * (V_lo + V_hi);
	  }
	  BIKEPOWER_MUT(velocity, newSVnv(V));
	} else {
	  BIKEPOWER_ACC_VAR(velocity,n,V);
	}
	
	/* Calculate the force (+/-) of the air */
	F_a = A2 * SQR(V + eff_H) + A1 * (V + eff_H);
	if (V + eff_H < 0)
	  F_a *= -1;

	/* Calculate the force or rolling restance */
	F_r  =  R * total_weight_N;

	/* Calculate the force (+/-) of the grade */
	F_g  =  grade * total_weight_N;

	/* Calculate the total force */
	F  =  F_a + F_r + F_g;

	/* Calculate Power in Watts */
	power = V * F / transmission_efficiency;
	BIKEPOWER_MUT(power, newSVnv(power));

	/* Calculate Calories and drivetrain loss */
	BIKEPOWER_ACC(BM,n);
	if (power > 0) {
	  double human_efficiency, BM;
	  BIKEPOWER_ACC(human_efficiency,n);
	  consumption = power/human_efficiency + BM;
	  P_t  =  (1.0 - transmission_efficiency) * power;
	} else {
	  consumption = power/human_efficiency + BM;
	  P_t  =  0.0;
	}
	BIKEPOWER_MUT(consumption, newSVnv(consumption));

	tmp = hv_fetch((HV*)SvRV(self), "_out", 4, 1);
	if (!SvROK(*tmp) || SvTYPE(SvRV(*tmp)) != SVt_PVHV) {
	  out = newHV();
	  hv_store((HV*)SvRV(self), "_out", 4, newRV_inc((SV*)out), 1); /* inc oder noinc XXXX? */
	} else {
	  out = (HV*)SvRV(*tmp);
	}

	hv_store(out, "Pa", 2, sv_2mortal(newSVnv(V * F_a)),1);
	hv_store(out, "Pr", 2, sv_2mortal(newSVnv(V * F_r)),1);
	hv_store(out, "Pg", 2, sv_2mortal(newSVnv(V * F_g)),1);
	hv_store(out, "Pt", 2, sv_2mortal(newSVnv(P_t)),1);
	hv_store(out, "P",  1, sv_2mortal(newSVnv(power)),1);
	hv_store(out, "hp", 2, sv_2mortal(newSVnv(power/SvNV(perl_get_sv("BikePower::Watts__per__horsepower",1)))),1);
	hv_store(out, "heat", 4, sv_2mortal(newSVnv(consumption-(BM+power))),1);
	hv_store(out, "C",  1,  sv_2mortal(newSVnv(consumption)),1);
	hv_store(out, "B",  1,  sv_2mortal(newSVnv(BM)),1);
	BIKEPOWER_ACCBOOL_VAR(imperial,imperial);
	if (!imperial) {
	  double velocity_kmh;
	  BIKEPOWER_ACC(velocity_kmh,n);
	  hv_store(out, "V", 1, sv_2mortal(newSVnv(velocity_kmh)),1);
	  hv_store(out, "F", 1, sv_2mortal(newSVnv(SvNV(perl_get_sv("BikePower::kg__per__Nt",1))*F)),1);
	  hv_store(out, "kJh", 3, sv_2mortal(newSVnv(consumption*SvNV(perl_get_sv("BikePower::Watts__per__Cal_hr",1)))),1); /* really Cal/hr */
	} else {
	  hv_store(out, "V", 1, sv_2mortal(newSVnv(V)),1);
	  hv_store(out, "F", 1, sv_2mortal(newSVnv(F/SvNV(perl_get_sv("BikePower::Nt__per__lb",1))*F)),1);
	  hv_store(out, "kJh", 3, sv_2mortal(newSVnv(consumption*SvNV(perl_get_sv("BikePower::Watts__per__Cal_hr",1)))),1); /* really Cal/hr */
	}

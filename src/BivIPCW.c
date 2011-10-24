/************************************/
/*** IPCW BIVARIATE PROBABILITIES ***/
/************************************/

#include <R.h>

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	This function reads bidimensional data in a way
		identical to how humans read a cartesian plot.

Parameters:
	x[in]			pointer to x first element
	y[in]			pointer to y first element
	n[in]			pointer to length of x and y
	i[inout]		pointer to index to start the search
	a[in]			pointer to abscissa value to read the ordinate at
	o[out]			pointer to ordinate value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors x and y must have the same length.
	For this function to work properly vector x must
		have been previously sorted by increasing order
		with the elements of vector y indexed at it.
*/

static void getOrdinate(
	const double *const x,
	const double *const y,
	const int *const n,
	int *const i,
	const double *const a,
	double *const o)
{
	for (; *i < *n; (*i)++) {
		if (x[*i] > *a) break;
	}
	*o = y[*i-1];
	return;
} // getOrdinate

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes the bivariate probability P(T2<=t2,T1<=t1).

Parameters:
	time1[in]		pointer to time1 first element
	event1[in]		pointer to event1 first element
	time2[in]		pointer to time2 first element
	event2[in]		pointer to event2 first element
	Stime[in]		pointer to Stime first element
	len[in]			pointer to length of time1, event1, time2, event2 and Stime
	t1[in]			pointer to time1 value to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2, event2 and Stime must have the same length.
*/

void BivDistIPCW1(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const double *const Stime,
	const int *const len,
	const double *const t1,
	const double *const t2,
	double *const p)
{
	register int i, j, k;
	int e = *len/2, n, d;
	double surv, survS;
	if (time1[e] > *t1) e = 0;
	for (; e < *len; e++) {
		if (time1[e] > *t1) break; // determine last index
	}
	for (i = 0, j = 0, k = 0, surv = 1, survS = 1, *p = 0; i < e; i++) { // loop through the sample
		if (j < e && time1[j] == time1[i]) {
			n = *len-j; // count the living
			d = 1-event1[j]; // initialize dead count
			for (j++; j < e && time1[j] == time1[j-1]; j++) { // loop until time changes or last index is reached
				d += 1-event1[j]; // count the dead
			}
			surv *= 1-(double)d/n; // compute survival probability
		}
		if (surv > 0) *p += (time2[i] > 0)/surv; // add term
		while (k < *len && Stime[k] <= time1[i]+*t2) {
			n = *len-k; // count the living
			d = 1-event2[k]; // initialize dead count
			for (k++; k < *len && Stime[k] == Stime[k-1]; k++) { // loop until time changes or last index is reached
				d += 1-event2[k]; // count the dead
			}
			survS *= 1-(double)d/n; // compute survival probability
		}
		if (survS > 0) *p -= (time2[i] > *t2)/survS; // subtract term
	}
	*p /= *len; // compute bivariate probability
	return;
} // BivDistIPCW1

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes vectors of times and probabilities to be used
		in the plot of the marginal distribution of time2.

Parameters:
	time1[in]		pointer to time1 first element
	event1[in]		pointer to event1 first element
	time2[in]		pointer to time2 first element
	event2[in]		pointer to event2 first element
	Stime[in]		pointer to Stime first element
	len[in]			pointer to length of time1, event1, time2, event2 and Stime
	times[in]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	np[in]			pointer to length of times and probs

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2, event2 and Stime must have the same length.
*/

void BivMarginalIPCW1(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const double *const Stime,
	const int *const len,
	const double *const times,
	double *const probs,
	const int *const np)
{
	register int i = 0, j;
	int n, d, k;
	double s = 1, p1 = 0, p2, *survS, t;
	while (i < *len) { // loop through the sample
		n = *len-i; // count the living
		d = 1-event1[i]; // initialize dead count
		for (i++; i < *len && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event1[i]; // count the dead
		}
		s *= 1-(double)d/n; // compute survival probability
		if (s > 0) for (j = *len-n; j < i; j++) p1 += (time2[j] > 0)/s; // add term
	}
	survS = (double*)Calloc(*len, double); // dynamically allocate memory
	i = 0;
	s = 1;
	while (i < *len) {
		n = *len-i; // count the living
		d = 1-event2[i]; // initialize dead count
		for (i++; i < *len && Stime[i] == Stime[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event2[i]; // count the dead
		}
		s *= 1-(double)d/n; // compute survival probability
		for (j = *len-n; j < i; j++) survS[j] = s;
	}
	for (i = 0; i < *np; i++) {
		for (j = 0, p2 = 0, k = 1, s = 1; j < *len; j++) {
			t = time1[j]+times[i];
			if (t >= Stime[0]) getOrdinate(Stime, survS, len, &k, &t, &s); // get survival probability
			if (s > 0) p2 += (time2[j] > times[i])/s; // add term
		}
		probs[i] = (p1-p2)/(*len); // compute bivariate probability
	}
	Free(survS); // free memory block
	return;
} // BivMarginalIPCW1

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes a matrix of probabilities to be used in the 'persp' and
		'filled.contour' plots of the bivariate distribution.

Parameters:
	time1[in]		pointer to time1 first element
	event1[in]		pointer to event1 first element
	time2[in]		pointer to time2 first element
	event2[in]		pointer to event2 first element
	Stime[in]		pointer to Stime first element
	len[in]			pointer to length of time1, event1, time2, event2 and Stime
	gridx[in]		pointer to vector of time1 values
	gridy[in]		pointer to vector of time2 values
	nx[in]			pointer to length of gridx
	ny[in]			pointer to length of gridy
	z[out]			pointer to a nx by ny matrix

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2, event2 and Stime must have the same length.
*/

void BivMatrixIPCW1(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const double *const Stime,
	const int *const len,
	const double *const gridx,
	const double *const gridy,
	const int *const nx,
	const int *const ny,
	double *const z)
{
	register int i = 0, j;
	int n, d;
	double *p1, *survS, *p2, p = 0, t, s = 1;
	p1 = (double*)Calloc(*len, double);
	while (i < *len) { // loop through the sample
		n = *len-i; // count the living
		d = 1-event1[i]; // initialize dead count
		for (i++; i < *len && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event1[i]; // count the dead
		}
		s *= 1-(double)d/n; // compute survival probability
		for (j = *len-n; j < i; j++) {
			if (s > 0) p += (time2[j] > 0)/s; // add term
			p1[j] = p;
		}
	}
	survS = (double*)Calloc(*len, double); // dynamically allocate memory
	i = 0;
	s = 1;
	while (i < *len) {
		n = *len-i; // count the living
		d = 1-event2[i]; // initialize dead count
		for (i++; i < *len && Stime[i] == Stime[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event2[i]; // count the dead
		}
		s *= 1-(double)d/n; // compute survival probability
		for (j = *len-n; j < i; j++) survS[j] = s; // save survival probability
	}
	p2 = (double*)Calloc(*len, double); // dynamically allocate memory
	for (i = 0; i < *ny; i++) { // loop through the columns
		for (j = 0, d = 1, p = 0, s = 1; j < *len; j++) { // loop through the sample
			t = time1[j]+gridy[i];
			if (t >= Stime[0]) getOrdinate(Stime, survS, len, &d, &t, &s); // get survival probability
			if (s > 0) p += (time2[j] > gridy[i])/s; // add term
			p2[j] = (p1[j]-p)/(*len); // compute and save probability
		}
		for (j = 0, d = 1, p = 0; j < *nx; j++) { // loop through the rows
			if (gridx[j] >= time1[0]) getOrdinate(time1, p2, len, &d, &gridx[j], &p); // get probability
			z[j+(*nx)*i] = p; // save probability
		}
	}
	Free(p1); // free memory block
	Free(survS); // free memory block
	Free(p2); // free memory block
	return;
} // BivMatrixIPCW1

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes the bivariate probability P(T2<=t2,T1<=t1).

Parameters:
	time2[in]		pointer to time2 first element
	surv[in]		pointer to surv first element
	survS[in]		pointer to survS first element
	len[in]			pointer to length of time2, surv and survS
	e[in]			pointer to last index to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time2, surv and survS must have the same length.
*/

void BivDistIPCW2(
	const double *const time1,
	const double *const surv,
	const double *const time2,
	const double *const Stime,
	const double *const survS,
	const int *const len,
	const double *const t1,
	const double *const t2,
	double *const p)
{
	register int i;
	int e = *len/2, x;
	double t, s;
	if (time1[e] > *t1) e = 0;
	for (; e < *len; e++) {
		if (time1[e] > *t1) break; // determine last index
	}
	for (i = 0, x = 1, *p = 0, s = 1; i < e; i++) { // loop through the sample
		if (surv[i] > 0) *p += (time2[i] > 0)/surv[i]; // add term
		t = time1[i]+*t2;
		if (t >= Stime[0]) getOrdinate(Stime, survS, len, &x, &t, &s); // get survival probability
		if (s > 0) *p -= (time2[i] > *t2)/s; // subtract term
	}
	*p /= *len; // compute bivariate probability
	return;
} // BivDistIPCW2

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes vectors of times and probabilities to be used
		in the plot of the marginal distribution of time2.

Parameters:
	time1[in]		pointer to time1 first element
	surv[in]		pointer to surv first element
	time2[in]		pointer to time2 first element
	Stime[in]		pointer to Stime first element
	survS[in]		pointer to survS first element
	len[in]			pointer to length of time1, surv, time2, Stime and survS
	times[in]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	np[in]			pointer to length of times and probs

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, surv, time2, Stime and survS must have the same length.
*/

void BivMarginalIPCW2(
	const double *const time1,
	const double *const surv,
	const double *const time2,
	const double *const Stime,
	const double *const survS,
	const int *const len,
	const double *const times,
	double *const probs,
	const int *const np)
{
	register int i, j;
	int k;
	double p1, p2, t, s;
	for (i = 0, p1 = 0; i < *len; i++) { // loop through the sample
		if (surv[i] > 0) p1 += (time2[i] > 0)/surv[i]; // add term
	}
	for (i = 0; i < *np; i++) {
		for (j = 0, k = 1, p2 = 0, s = 1; j < *len; j++) {
			t = time1[j]+times[i];
			if (t >= Stime[0]) getOrdinate(Stime, survS, len, &k, &t, &s); // get survival probability
			if (s > 0) p2 += (time2[j] > times[i])/s; // add term
		}
		probs[i] = (p1-p2)/(*len); // compute bivariate probability
	}
	return;
} // BivMarginalIPCW2

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes a matrix of probabilities to be used in the 'persp' and
		'filled.contour' plots of the bivariate distribution.

Parameters:
	time1[in]		pointer to time1 first element
	surv[in]		pointer to surv first element
	time2[in]		pointer to time2 first element
	Stime[in]		pointer to Stime first element
	survS[in]		pointer to survS first element
	len[in]			pointer to length of time1, surv, time2, Stime and survS
	gridx[in]		pointer to vector of time1 values
	gridy[in]		pointer to vector of time2 values
	nx[in]			pointer to length of gridx
	ny[in]			pointer to length of gridy
	z[out]			pointer to a nx by ny matrix

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, surv, time2, Stime and survS must have the same length.
*/

void BivMatrixIPCW2(
	const double *const time1,
	const double *const surv,
	const double *const time2,
	const double *const Stime,
	const double *const survS,
	const int *const len,
	const double *const gridx,
	const double *const gridy,
	const int *const nx,
	const int *const ny,
	double *const z)
{
	register int i, j;
	int x;
	double *p1, *p2, p, t, s;
	p1 = (double*)Calloc(*len, double); // dynamically allocate memory
	for (j = 0, p = 0; j < *len; j++) { // loop through the sample
		if (surv[j] > 0) p += (time2[j] > 0)/surv[j]; // add term
		p1[j] = p; // save probability
	}
	p2 = (double*)Calloc(*len, double); // dynamically allocate memory
	for (i = 0; i < *ny; i++) { // loop through the columns
		for (j = 0, x = 1, p = 0, s = 1; j < *len; j++) { // loop through the sample
			t = time1[j]+gridy[i];
			if (t >= Stime[0]) getOrdinate(Stime, survS, len, &x, &t, &s); // get survival probability
			if (s > 0) p += (time2[j] > gridy[i])/s; // add term
			p2[j] = (p1[j]-p)/(*len); // compute and save probability
		}
		for (j = 0, x = 1, p = 0; j < *nx; j++) { // loop through the rows
			if (gridx[j] >= time1[0]) getOrdinate(time1, p2, len, &x, &gridx[j], &p); // get probability
			z[j+(*nx)*i] = p; // save probability
		}
	}
	Free(p1); // free memory block
	Free(p2); // free memory block
	return;
} // BivMatrixIPCW2

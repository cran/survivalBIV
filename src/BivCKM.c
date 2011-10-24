/***********************************/
/*** CKM BIVARIATE PROBABILITIES ***/
/***********************************/

#include <R.h>
#include "BivSORT.h"

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
	len[in]			pointer to length of time1, event1, time2 and event2
	t1[in]			pointer to time1 value to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2 and event2 must have the same length.
*/

void BivDistCKM(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const int *const len,
	const double *const t1,
	const double *const t2,
	double *const p)
{
	register int i = 0, j = 0;
	int e = *len/2, n, d, *event;
	double surv = 1, *time;
	if (time1[e] > *t1) e = 0;
	for (; e < *len; e++) {
		if (time1[e] > *t1) break; // determine last index
	}
	while (i < e) { // loop through the sample until last index is reached
		n = *len-i; // count the living
		d = event1[i]; // initialize dead count
		for (i++; i < e && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += event1[i]; // count the dead
		}
		surv *= 1-(double)d/n; // compute survival probability
		j += d;
	}
	event = (int*)Calloc(j, int); // dynamically allocate memory
	time = (double*)Calloc(j, double); // dynamically allocate memory
	for (i = 0, j = 0; i < e; i++) { // subset sample
		if (event1[i]) {
			time[j] = time2[i];
			event[j++] = event2[i];
		}
	}
	e = j/2;
	i = 0;
	*p = 1;
	rsort_with_index(time, event, j); // use internal R sorting to sort time and event
	if (time[e] > *t2) e = 0;
	for (; e < j; e++) {
		if (time[e] > *t2) break; // determine last index
	}
	while (i < e) { // loop through the sample until last index is reached
		n = j-i; // count the living
		d = event[i]; // initialize dead count
		for (i++; i < e && time[i] == time[i-1]; i++) { // loop until time changes or last index is reached
			d += event[i]; // count the dead
		}
		*p *= 1-(double)d/n; // compute survival probability
	}
	*p = (1-surv)*(1-*p); // compute bivariate probability
	Free(event); // free memory block
	Free(time); // free memory block
	return;
} // BivDistCKM

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
	len[in]			pointer to length of time1, event1, time2 and event2
	times[out]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	np[out]			pointer to length of times and probs

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2 and event2 must have the same length.
*/

void BivMarginalCKM(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const int *const len,
	double *const times,
	double *const probs,
	int *const np)
{
	register int i = 0, j = 0;
	int n, d;
	double surv1 = 1, surv2 = 1;
	while (i < *len) { // loop through the sample until last index is reached
		n = *len-i; // count the living
		d = event1[i]; // initialize dead count
		for (i++; i < *len && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += event1[i]; // count the dead
		}
		surv1 *= 1-(double)d/n; // compute survival probability
	}
	for (i = 0, *np = 0; i < *len; i++) { // subset sample
		if (event1[i]) {
			times[*np] = time2[i];
			probs[(*np)++] = event2[i];
		}
	}
	i = 0;
	sort_dd(times, probs, *np);
	while (i < *np) { // loop through the sample until last index is reached
		n = *np-i; // count the living
		d = probs[i]; // initialize dead count
		for (i++; i < *np && times[i] == times[i-1]; i++) { // loop until time changes or last index is reached
			d += probs[i]; // count the dead
		}
		surv2 *= 1-(double)d/n; // compute survival probability
		times[j] = times[i-1]; // save time
		probs[j++] = (1-surv1)*(1-surv2); // save bivariate probability
	}
	*np = j;
	return;
} // BivMarginalCKM

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
	len[in]			pointer to length of time1, event1, time2 and event2
	gridx[in]		pointer to vector of time1 values
	gridy[in]		pointer to vector of time2 values
	nx[in]			pointer to length of gridx
	ny[in]			pointer to length of gridy
	z[out]			pointer to a nx by ny matrix

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2 and event2 must have the same length.
*/

void BivMatrixCKM(
	const double *const time1,
	const int *const event1,
	const double *const time2,
	const int *const event2,
	const int *const len,
	const double *const gridx,
	const double *const gridy,
	const int *const nx,
	const int *const ny,
	double *const z)
{
	register int x, y, i, j;
	int n, d, e1, e2, e, *event;
	double s1, s2, *time;
	event = (int*)Calloc(*len, int); // dynamically allocate memory
	time = (double*)Calloc(*len, double); // dynamically allocate memory
	for (x = 0, i = 0, e1 = 0, e2 = 0, s1 = 1; x < *nx; x++) { // loop through the rows
		for (; e1 < *len; e1++) {
			if (time1[e1] > gridx[x]) break; // determine last index
		}
		while (i < e1) { // loop through the sample until last index is reached
			n = *len-i; // count the living
			d = event1[i]; // initialize dead count
			if (d) { // subset sample
				time[e2] = time2[i];
				event[e2++] = event2[i];
			}
			for (i++; i < e1 && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
				if (event1[i]) { // subset sample
					d++; // count the dead
					time[e2] = time2[i];
					event[e2++] = event2[i];
				}
			}
			s1 *= 1-(double)d/n; // compute survival probability
		}
		rsort_with_index(time, event, e2); // use internal R sorting to sort time and event
		for (y = 0, j = 0, e = 0, s2 = 1; y < *ny; y++) { // loop through the columns
			for (; e < e2; e++) {
				if (time[e] > gridy[y]) break; // determine last index
			}
			while (j < e) { // loop through the sample until last index is reached
				n = e2-j; // count the living
				d = event[j]; // initialize dead count
				for (j++; j < e && time[j] == time[j-1]; j++) { // loop until time changes or last index is reached
					d += event[j]; // count the dead
				}
				s2 *= 1-(double)d/n; // compute survival probability
			}
			z[x+(*nx)*y] = (1-s1)*(1-s2); // save probability
		}
	}
	Free(event); // free memory block
	Free(time); // free memory block
	return;
} // BivMatrixCKM

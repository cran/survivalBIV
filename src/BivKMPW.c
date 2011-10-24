/************************************/
/*** KMPW BIVARIATE PROBABILITIES ***/
/************************************/

#include <R.h>
#include "BivSORT.h"

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes the bivariate probability P(T2<=t2,T1<=t1).

Parameters:
	time1[in]		pointer to time1 first element
	time2[in]		pointer to time2 first element
	m[in]			pointer to m first element
	len[in]			pointer to length of time1, time2 and m
	t1[in]			pointer to time1 value to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2 and m must have the same length.
*/

void BivDistKMPW(
	const double *const time1,
	const double *const time2,
	const double *const m,
	const int *const len,
	const double *const t1,
	const double *const t2,
	double *const p)
{
	register int i;
	double aux[3];
	for (i = 0, aux[0] = 1, *p = 0; i < *len; i++) { // loop through the sample until last index is reached
		aux[2] = m[i]/(*len-i); // compute needed factor
		aux[1] = 1-aux[2]; // factor needed for the computation
		aux[2] *= aux[0]; // compute and save weight
		aux[0] *= aux[1]; // compute and save factor needed for next iteration
		*p += aux[2]*(time1[i] <= *t1 && time2[i] <= *t2); // compute bivariate probability
	}
	return;
} // BivDistKMPW

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes vectors of times and probabilities to be used
		in the plot of the marginal distribution of time2.

Parameters:
	time2[in]		pointer to time2 first element
	m[in]			pointer to m first element
	len[in]			pointer to length of time2 and m
	times[out]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	np[out]			pointer to length of times and probs

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time2 and m must have the same length.
*/

void BivMarginalKMPW(
	const double *const time2,
	const double *const m,
	const int *const len,
	double *const times,
	double *const probs,
	int *const np)
{
	register int i;
	double aux[2];
	for (i = 0, aux[0] = 1; i < *len; i++) { // loop through the sample until last index is reached
		times[i] = time2[i];
		probs[i] = m[i]/(*len-i); // compute needed factor
		aux[1] = 1-probs[i]; // factor needed for the computation
		probs[i] *= aux[0]; // compute and save weight
		aux[0] *= aux[1]; // compute and save factor needed for next iteration
	}
	sort_dd(times, probs, *len);
	for (i = 1, *np = 0; i < *len; i++) {
		if (times[i] != times[i-1]) {
			times[*np] = times[i-1];
			probs[(*np)++] = probs[i-1];
		}
		probs[i] += probs[i-1];
	}
	times[*np] = times[i-1];
	probs[*np] = probs[i-1];
	(*np)++;
	return;
} // BivMarginalKMPW

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Computes a matrix of probabilities to be used in the 'persp' and
		'filled.contour' plots of the bivariate distribution.

Parameters:
	time1[in]		pointer to time1 first element
	time2[in]		pointer to time2 first element
	m[in]			pointer to m first element
	len[in]			pointer to length of time1, time2 and m
	gridx[in]		pointer to vector of time1 values
	gridy[in]		pointer to vector of time2 values
	nx[in]			pointer to length of gridx
	ny[in]			pointer to length of gridy
	z[out]			pointer to a nx by ny matrix

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2 and m must have the same length.
*/

void BivMatrixKMPW(
	const double *const time1,
	const double *const time2,
	const double *const m,
	const int *const len,
	const double *const gridx,
	const double *const gridy,
	const int *const nx,
	const int *const ny,
	double *const z)
{
	register int i, j, k;
	int e;
	double aux[2], *t1, *t2, *w;
	t1 = (double*)Calloc(*len, double); // dynamically allocate memory
	t2 = (double*)Calloc(*len, double); // dynamically allocate memory
	w = (double*)Calloc(*len, double); // dynamically allocate memory
	for (i = 0, aux[0] = 1; i < *len; i++) { // loop through the sample until last index is reached
		t1[i] = time1[i]; // save time1
		t2[i] = time2[i]; // save time2
		w[i] = m[i]/(*len-i); // compute needed factor
		aux[1] = 1-w[i]; // factor needed for the computation
		w[i] *= aux[0]; // compute and save weight
		aux[0] *= aux[1]; // compute and save factor needed for next iteration
	}
	sort_ddd(t1, t2, w, *len); // sort t1, t2 and w
	for (i = 0, e = 0; i < *nx; i++) { // loop through the rows
		for (; e < *len; e++) if (t1[e] > gridx[i]) break; // determine last index
		for (j = 0; j < *ny; j++) { // loop through the columns
			for (k = 0, z[i+(*nx)*j] = 0; k < e; k++) { // loop through the sample until last index is reached
				z[i+(*nx)*j] += w[k]*(t2[k] <= gridy[j]); // compute and save bivariate probability
			}
		}
	}
	Free(t1); // free memory block
	Free(t2); // free memory block
	Free(w); // free memory block
	return;
} // BivMatrixKMPW

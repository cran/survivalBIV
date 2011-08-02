#include <R.h>

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rcmp')

Description:
	Compares two double values.

Parameters:
	x[in]		first double
	y[in]		second double

Return value:
	Returns -1 if x is lower than y.
	Returns 1 if x is greater than y.
	Returns 0 otherwise.
*/

static int cmp_doubles(
	const double x,
	const double y)
{
	if (x < y) return -1;
	if (x > y) return 1;
	return 0;
} // cmp_doubles

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'indx', 'y' and 'indy' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	indx[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	indy[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_biv(
	double *const x,
	int *const indx,
	double *const y,
	int *const indy,
	const int n)
{
	double v, u;
	int i, j, h, iv, iu;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			v = x[i];
			iv = indx[i];
			u = y[i];
			iu = indy[i];
			j = i;
			while (j >= h && cmp_doubles(x[j - h], v) > 0) {
				x[j] = x[j-h];
				indx[j] = indx[j-h];
				y[j] = y[j-h];
				indy[j] = indy[j-h];
				j -= h;
			}
			x[j] = v;
			indx[j] = iv;
			y[j] = u;
			indy[j] = iu;
		}
	}
} // sort_biv

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'indx', 'y' and 'z' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	indx[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	z[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_biv_stime(
	double *const x,
	int *const indx,
	double *const y,
	double *const z,
	const int n)
{
	double v, u, iu;
	int i, j, h, iv;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			v = x[i];
			iv = indx[i];
			u = y[i];
			iu = z[i];
			j = i;
			while (j >= h && cmp_doubles(x[j - h], v) > 0) {
				x[j] = x[j-h];
				indx[j] = indx[j-h];
				y[j] = y[j-h];
				z[j] = z[j-h];
				j -= h;
			}
			x[j] = v;
			indx[j] = iv;
			y[j] = u;
			z[j] = iu;
		}
	}
} // sort_biv_stime

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'indx' and 'y' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	indx[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_biv_time(
	double *const x,
	int *const indx,
	double *const y,
	const int n)
{
	double v, u;
	int i, j, h, iv;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			v = x[i];
			iv = indx[i];
			u = y[i];
			j = i;
			while (j >= h && cmp_doubles(x[j - h], v) > 0) {
				x[j] = x[j-h];
				indx[j] = indx[j-h];
				y[j] = y[j-h];
				j -= h;
			}
			x[j] = v;
			indx[j] = iv;
			y[j] = u;
		}
	}
} // sort_biv_time

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'y' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_doubles(
	double *const x,
	double *const y,
	const int n)
{
	double v, u;
	int i, j, h;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			v = x[i];
			u = y[i];
			j = i;
			while (j >= h && cmp_doubles(x[j - h], v) > 0) {
				x[j] = x[j-h];
				y[j] = y[j-h];
				j -= h;
			}
			x[j] = v;
			y[j] = u;
		}
	}
} // sort_doubles

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'indx', 'y' and 'z' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	indx[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	z[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_biv_data(
	double *const x,
	double *const indx,
	double *const y,
	double *const z,
	const int n)
{
	double v, iv, u, iu;
	int i, j, h;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			v = x[i];
			iv = indx[i];
			u = y[i];
			iu = z[i];
			j = i;
			while (j >= h && cmp_doubles(x[j - h], v) > 0) {
				x[j] = x[j-h];
				indx[j] = indx[j-h];
				y[j] = y[j-h];
				z[j] = z[j-h];
				j -= h;
			}
			x[j] = v;
			indx[j] = iv;
			y[j] = u;
			z[j] = iu;
		}
	}
} // sort_biv_data

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'indx' by decreasing order with x and y alongside.

Parameters:
	indx[inout]		pointer to vector's first element
	x[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_double_rev(
	double *const indx,
	double *const x,
	double *const y,
	const int n)
{
	double iv, v, u;
	int i, j, h;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			iv = indx[i];
			v = x[i];
			u = y[i];
			j = i;
			while (j >= h && cmp_doubles(indx[j - h], iv) < 0) {
				indx[j] = indx[j-h];
				x[j] = x[j-h];
				y[j] = y[j-h];
				j -= h;
			}
			indx[j] = iv;
			x[j] = v;
			y[j] = u;
		}
	}
} // sort_double_rev

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'icmp')

Description:
	Compares two int values.

Parameters:
	x[in]		first int
	y[in]		second int

Return value:
	Returns -1 if x is lower than y.
	Returns 1 if x is greater than y.
	Returns 0 otherwise.
*/

static int cmp_ints(
	const int x,
	const int y)
{
	if (x < y) return -1;
	if (x > y) return 1;
	return 0;
} // cmp_ints

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'indx' by decreasing order with x and y alongside.

Parameters:
	indx[inout]		pointer to vector's first element
	x[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

static void sort_int_rev(
	int *const indx,
	double *const x,
	double *const y,
	const int n)
{
	double v, u;
	int i, j, h, iv;

	for (h = 1; h <= n / 9; h = 3 * h + 1);
	for (; h > 0; h /= 3) {
		for (i = h; i < n; i++) {
			iv = indx[i];
			v = x[i];
			u = y[i];
			j = i;
			while (j >= h && cmp_ints(indx[j - h], iv) < 0) {
				indx[j] = indx[j-h];
				x[j] = x[j-h];
				y[j] = y[j-h];
				j -= h;
			}
			indx[j] = iv;
			x[j] = v;
			y[j] = u;
		}
	}
} // sort_int_rev

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts Stime by increasing order with event2 alongside.
	In the subsets of constant Stime observations,
		events are put first and censures last.

Parameters:
	Stime[inout]	pointer to Stime first element
	event2[inout]	pointer to event2 first element
	time1[inout]	pointer to time1 first element
	time2[inout]	pointer to time2 first element
	len[in]			length of Stime, event2, time1 and time2

Return value:
	This function doesn't return a value.

Remarks:
	Vectors Stime, event2, time1 and time2 must have the same length.
*/

static void sort_biv_surv_stime(
	double *const Stime,
	int *const event2,
	double *const time1,
	double *const time2,
	const int len)
{
	register int i = 0;
	int j, k;
	sort_biv_stime(Stime, event2, time1, time2, len); // sort data
	while (i < len) { // loop through the sample until last index is reached
		for (i++, j = 1; i < len && Stime[i] == Stime[i-1]; i++) { // loop through the sample until Stime changes or last index is reached
			j++; // count equal Stimes
		}
		if (j > 1) { // if there are equal Stimes
			k = i-j;
			sort_int_rev(&event2[k], &time1[k], &time2[k], j); // put censored observations last
		}
	}
	return;
} // sort_biv_surv_stime

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts Stime by increasing order with m alongside.
	In the subsets of constant Stime observations,
		m observations are sorted by decreasing order.

Parameters:
	Stime[inout]	pointer to Stime first element
	m[inout]		pointer to m first element
	time1[inout]	pointer to time1 first element
	time2[inout]	pointer to time2 first element
	len[in]			length of Stime, m, time1 and time2

Return value:
	This function doesn't return a value.

Remarks:
	Vectors Stime, m, time1 and time2 must have the same length.
*/

static void sort_biv_surv(
	double *const Stime,
	double *const m,
	double *const time1,
	double *const time2,
	const int len)
{
	register int i = 0;
	int j, k;
	sort_biv_data(Stime, m, time1, time2, len); // sort data
	while (i < len) { // loop through the sample until last index is reached
		for (i++, j = 1; i < len && Stime[i] == Stime[i-1]; i++) { // loop through the sample until Stime changes or last index is reached
			j++; // count equal Stimes
		}
		if (j > 1) { // if there are equal Stimes
			k = i-j;
			sort_double_rev(&m[k], &time1[k], &time2[k], j); // put censored observations last
		}
	}
	return;
} // sort_biv_surv

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	This function reads bidimensional data in a way
		identical to how humans read a plot.

Parameters:
	uniqueS[in]		pointer to uniqueS first element
	survS[in]		pointer to survS first element
	len[in]			pointer to length of uniqueS and survS
	end[inout]		pointer to index to start the search
	time[in]		pointer to time to read the probability at
	surv[out]		pointer to survival probability

Return value:
	This function doesn't return a value.

Remarks:
	Vectors uniqueS and survS must have the same length.
	For this function to work properly vector uniqueS must
		have been previously sorted by increasing order
		with the elements of vector survS indexed at it.
*/

static void getSurv(
	const double *const uniqueS,
	const double *const survS,
	const int *const len,
	int *const end,
	const double *const time,
	double *const surv)
{
	if (uniqueS[*end] > *time) {
		*end /= 2;
		if (uniqueS[*end] > *time) *end = 1;
	}
	for (; *end < *len; (*end)++) {
		if (uniqueS[*end] > *time) break;
	}
	(*end)--;
	*surv = survS[*end];
	return;
} // getSurv

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Copies the elements of the in vector to the out vector

Parameters:
	in[in]			pointer to in first element
	out[out]		pointer to out first element
	len[in]			pointer to the number of elements to copy

Return value:
	This function doesn't return a value.

Remarks:
	The length of vector out must be equal or greater than
		the length of vector in.
*/

void doubleCopy(
	const double *const in,
	double *const out,
	const int *const len)
{
	register int i;
	for (i = 0; i < *len; i++) out[i] = in[i];
	return;
} // doubleCopy

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistCKM' function.

Parameters:
	time1[inout]	pointer to time1 first element
	event1[inout]	pointer to event1 first element
	time2[inout]	pointer to time2 first element
	event2[inout]	pointer to event2 first element
	len[in]			pointer to length of time1, event1, time2 and event2

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2 and event2 must have the same length.
*/

void BivSortCKM(
	double *const time1,
	int *const event1,
	double *const time2,
	int *const event2,
	const int *const len)
{
	sort_biv(time1, event1, time2, event2, *len); // sort data
	return;
} // BivSortCKM

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	register int i = 0;
	int end = *len/2, n, d, j = 0;
	double surv = 1;
	if (time1[end] > *t1) end = 0;
	for (; end < *len; end++) {
		if (time1[end] > *t1) break; // determine last index
	}
	while (i < end) { // loop through the sample until last index is reached
		n = *len-i; // count the living
		d = event1[i]; // initialize dead count
		for (i++; i < end && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += event1[i]; // count the dead
		}
		surv *= 1-(double)d/n; // compute survival probability
		j += d;
	}
	double time3[j];
	int event3[j];
	for (i = 0, j = 0; i < end; i++) { // subset data
		if (event1[i]) {
			time3[j] = time2[i];
			event3[j++] = event2[i];
		}
	}
	end = j/2;
	i = 0;
	*p = 1;
	rsort_with_index(time3, event3, j); // use internal R sorting to sort time and event
	if (time3[end] > *t2) end = 0;
	for (; end < j; end++) {
		if (time3[end] > *t2) break; // determine last index
	}
	while (i < end) { // loop through the sample until last index is reached
		n = j-i; // count the living
		d = event3[i]; // initialize dead count
		for (i++; i < end && time3[i] == time3[i-1]; i++) { // loop until time changes or last index is reached
			d += event3[i]; // count the dead
		}
		*p *= 1-(double)d/n; // compute survival probability
	}
	*p = (1-surv)*(1-*p); // compute bivariate probability
	return;
} // BivDistCKM

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	j[out]			pointer to length of times and probs

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
	int *const j)
{
	register int i = 0;
	int n, d, k = 0;
	double surv1 = 1, surv2 = 1;
	while (i < *len) { // loop through the sample until last index is reached
		n = *len-i; // count the living
		d = event1[i]; // initialize dead count
		for (i++; i < *len && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += event1[i]; // count the dead
		}
		surv1 *= 1-(double)d/n; // compute survival probability
	}
	for (i = 0, *j = 0; i < *len; i++) { // subset data
		if (event1[i]) {
			times[*j] = time2[i];
			probs[(*j)++] = event2[i];
		}
	}
	i = 0;
	sort_doubles(times, probs, *j);
	while (i < *j) { // loop through the sample until last index is reached
		n = *j-i; // count the living
		d = probs[i]; // initialize dead count
		for (i++; i < *j && times[i] == times[i-1]; i++) { // loop until time changes or last index is reached
			d += probs[i]; // count the dead
		}
		surv2 *= 1-(double)d/n; // compute survival probability
		times[k] = times[i-1];
		probs[k++] = (1-surv1)*(1-surv2);
	}
	*j = k;
	return;
} // BivMarginalCKM

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistKMW' function.

Parameters:
	time1[inout]	pointer to time1 first element
	time2[inout]	pointer to time2 first element
	event2[inout]	pointer to event2 first element
	Stime[inout]	pointer to Stime first element
	len[in]			pointer to length of time1, time2, event2 and Stime

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2, event2 and Stime must have the same length.
*/

void BivSortKMW(
	double *const time1,
	double *const time2,
	int *const event2,
	double *const Stime,
	const int *const len)
{
	sort_biv_surv_stime(Stime, event2, time1, time2, *len); // sort data
	return;
} // BivSortKMW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Computes the bivariate probability P(T2<=t2,T1<=t1).

Parameters:
	time1[in]		pointer to time1 first element
	time2[in]		pointer to time2 first element
	event2[in]		pointer to event2 first element
	len[in]			pointer to length of time1, time2 and event2
	t1[in]			pointer to time1 value to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2 and event2 must have the same length.
*/

void BivDistKMW(
	const double *const time1,
	const double *const time2,
	const int *const event2,
	const int *const len,
	const double *const t1,
	const double *const t2,
	double *const p)
{
	register int i;
	double aux[3];
	for (i = 0, aux[0] = 1, *p = 0; i < *len; i++) { // loop through the sample until last index is reached
		aux[2] = (double)event2[i]/(*len-i); // compute needed factor
		aux[1] = 1-aux[2]; // factor needed for the computation
		aux[2] *= aux[0]; // compute and save weight
		aux[0] *= aux[1]; // compute and save factor needed for next iteration
		*p += aux[2]*(time1[i] <= *t1 && time2[i] <= *t2); // compute bivariate probability
	}
	return;
} // BivDistKMW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Computes vectors of times and probabilities to be used
		in the plot of the marginal distribution of time2.

Parameters:
	time2[in]		pointer to time2 first element
	event2[in]		pointer to event2 first element
	len[in]			pointer to length of time2 and event2
	times[out]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	j[out]			pointer to length of times and probs

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time2 and event2 must have the same length.
*/

void BivMarginalKMW(
	const double *const time2,
	const int *const event2,
	const int *const len,
	double *const times,
	double *const probs,
	int *const j)
{
	register int i;
	double aux[2];
	for (i = 0, aux[0] = 1; i < *len; i++) { // loop through the sample until last index is reached
		times[i] = time2[i];
		probs[i] = (double)event2[i]/(*len-i); // compute needed factor
		aux[1] = 1-probs[i]; // factor needed for the computation
		probs[i] *= aux[0]; // compute and save weight
		aux[0] *= aux[1]; // compute and save factor needed for next iteration
	}
	sort_doubles(times, probs, *len);
	for (i = 1, *j = 0; i < *len; i++) {
		if (times[i] != times[i-1]) {
			times[*j] = times[i-1];
			probs[(*j)++] = probs[i-1];
		}
		probs[i] += probs[i-1];
	}
	times[*j] = times[i-1];
	probs[*j] = probs[i-1];
	(*j)++;
	return;
} // BivMarginalKMW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistKMPW' function.

Parameters:
	time1[inout]	pointer to time1 first element
	time2[inout]	pointer to time2 first element
	m[inout]		pointer to m first element
	Stime[inout]	pointer to Stime first element
	len[in]			pointer to length of time1, time2, m and Stime

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2, m and Stime must have the same length.
*/

void BivSortKMPW(
	double *const time1,
	double *const time2,
	double *const m,
	double *const Stime,
	const int *const len)
{
	sort_biv_surv(Stime, m, time1, time2, *len); // sort data
	return;
} // BivSortKMPW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Computes vectors of times and probabilities to be used
		in the plot of the marginal distribution of time2.

Parameters:
	time2[in]		pointer to time2 first element
	m[in]			pointer to m first element
	len[in]			pointer to length of time2 and m
	times[out]		pointer to vector of times
	probs[out]		pointer to vector of probabilities
	j[out]			pointer to length of times and probs

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
	int *const j)
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
	sort_doubles(times, probs, *len);
	for (i = 1, *j = 0; i < *len; i++) {
		if (times[i] != times[i-1]) {
			times[*j] = times[i-1];
			probs[(*j)++] = probs[i-1];
		}
		probs[i] += probs[i-1];
	}
	times[*j] = times[i-1];
	probs[*j] = probs[i-1];
	(*j)++;
	return;
} // BivMarginalKMPW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistIPCW' function.

Parameters:
	time1[inout]	pointer to time1 first element
	event1[inout]	pointer to event1 first element
	time2[inout]	pointer to time2 first element
	event2[inout]	pointer to event2 first element
	Stime[inout]	pointer to Stime first element
	len[in]			pointer to length of time1, event1, time2, event2 and Stime

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, event1, time2, event2 and Stime must have the same length.
*/

void BivSortIPCW(
	double *const time1,
	int *const event1,
	double *const time2,
	int *const event2,
	double *const Stime,
	const int *const len)
{
	sort_biv_time(time1, event1, time2, *len); // sort time1, event1 and time2
	rsort_with_index(Stime, event2, *len); // sort Stime and event2
	return;
} // BivSortIPCW

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	int end = *len/2, n, d;
	double surv, survS;
	if (time1[end] > *t1) end = 0;
	for (; end < *len; end++) {
		if (time1[end] > *t1) break; // determine last index
	}
	for (i = 0, j = 0, k = 0, surv = 1, survS = 1, *p = 0; i < end; i++) { // loop through the sample
		if (j < end && time1[j] == time1[i]) {
			n = *len-j; // count the living
			d = 1-event1[j]; // initialize dead count
			for (j++; j < end && time1[j] == time1[j-1]; j++) { // loop until time changes or last index is reached
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
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	j[in]			pointer to length of times and probs

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
	const int *const j)
{
	register int i = 0, k;
	int n, d, u;
	double s = 1, p1 = 0, p2, *uniqueS, *survS, t;
	while (i < *len) { // loop through the sample
		n = *len-i; // count the living
		d = 1-event1[i]; // initialize dead count
		for (i++; i < *len && time1[i] == time1[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event1[i]; // count the dead
		}
		s *= 1-(double)d/n; // compute survival probability
		if (s > 0) {
			for (k = *len-n; k < i; k++) {
				p1 += (time2[k] > 0)/s; // add term
			}
		}
	}
	uniqueS = (double*)Calloc( (*len)+1, double );
	survS = (double*)Calloc( (*len)+1, double );
	if (Stime[0] != 0) {
		uniqueS[0] = 0;
		survS[0] = 1;
		u = 1;
	} else u = 0;
	i = 0;
	s = 1;
	while (i < *len) {
		n = *len-i; // count the living
		d = 1-event2[i]; // initialize dead count
		for (i++; i < *len && Stime[i] == Stime[i-1]; i++) { // loop until time changes or last index is reached
			d += 1-event2[i]; // count the dead
		}
		if (d > 0) {
			s *= 1-(double)d/n; // compute survival probability
			survS[u] = s;
			uniqueS[u++] = Stime[i-1];
		}
	}
	uniqueS = (double*)Realloc(uniqueS, u, double);
	survS = (double*)Realloc(survS, u, double);
	for (i = 0; i < *j; i++) {
		for (k = 0, d = u/2, p2 = 0; k < *len; k++) {
			t = time1[k]+times[i];
			getSurv(uniqueS, survS, &u, &d, &t, &s); // get survival probability
			if (s > 0) p2 += (time2[k] > times[i])/s; // add term
		}
		probs[i] = (p1-p2)/(*len); // compute bivariate probability
	}
	Free(uniqueS);
	Free(survS);
	return;
} // BivMarginalIPCW1

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

Description:
	Computes the bivariate probability P(T2<=t2,T1<=t1).

Parameters:
	time2[in]		pointer to time2 first element
	surv[in]		pointer to surv first element
	survS[in]		pointer to survS first element
	len[in]			pointer to length of time2, surv and survS
	end[in]			pointer to last index to compute the probability at
	t2[in]			pointer to time2 value to compute the probability at
	p[out]			pointer to probability value

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time2, surv and survS must have the same length.
*/

void BivDistIPCW2(
	const double *const time2,
	const double *const surv,
	const double *const survS,
	const int *const len,
	const int *const end,
	const double *const t2,
	double *const p)
{
	register int i;
	for (i = 0, *p = 0; i < *end; i++) { // loop through the sample
		if (surv[i] > 0) *p += (time2[i] > 0)/surv[i]; // add term
		if (survS[i] > 0) *p -= (time2[i] > *t2)/survS[i]; // subtract term
	}
	*p /= *len; // compute bivariate probability
	return;
} // BivDistIPCW2

/*
Author:
	Artur Agostinho Araújo <b5492@math.uminho.pt>

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
	j[in]			pointer to length of times and probs

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
	const int *const j)
{
	register int i, k;
	int d;
	double p1, p2, t, s;
	for (i = 0, p1 = 0; i < *len; i++) { // loop through the sample
		if (surv[i] > 0) p1 += (time2[i] > 0)/surv[i]; // add term
	}
	for (i = 0; i < *j; i++) {
		for (k = 0, d = (*len)/2, p2 = 0; k < *len; k++) {
			t = time1[k]+times[i];
			getSurv(Stime, survS, len, &d, &t, &s); // get survival probability
			if (s > 0) p2 += (time2[k] > times[i])/s; // add term
		}
		probs[i] = (p1-p2)/(*len); // compute bivariate probability
	}
	return;
} // BivMarginalIPCW2

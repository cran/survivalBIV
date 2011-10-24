/**********************/
/*** SORT FUNCTIONS ***/
/**********************/

#include <R.h>

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rcmp')

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
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_didi(
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
} // sort_didi

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_didd(
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
} // sort_didd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_did(
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
} // sort_did

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

Description:
	Sorts 'x' by increasing order with 'y' alongside.

Parameters:
	x[inout]		pointer to vector's first element
	y[inout]		pointer to vector's first element
	n[in]			vector's length

Return value:
	This function doesn't return a value.
*/

void sort_dd(
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
} // sort_dd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

void sort_ddd(
	double *const x,
	double *const indx,
	double *const y,
	const int n)
{
	double v, u, iv;
	int i, j, h;

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
} // sort_ddd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_dddd(
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
} // sort_dddd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_rev_ddd(
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
} // sort_rev_ddd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'icmp')

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
	Artur Agostinho Araújo <b5498@math.uminho.pt> (adapted from 'rsort_with_index')

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

static void sort_rev_idd(
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
} // sort_rev_idd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

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

static void sort_surv_didd(
	double *const Stime,
	int *const event2,
	double *const time1,
	double *const time2,
	const int len)
{
	register int i = 0;
	int j, k;
	sort_didd(Stime, event2, time1, time2, len); // sort data
	while (i < len) { // loop through the sample until last index is reached
		for (i++, j = 1; i < len && Stime[i] == Stime[i-1]; i++) { // loop through the sample until Stime changes or last index is reached
			j++; // count equal Stimes
		}
		if (j > 1) { // if there are equal Stimes
			k = i-j;
			sort_rev_idd(&event2[k], &time1[k], &time2[k], j); // put censored observations last
		}
	}
	return;
} // sort_surv_didd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

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

static void sort_surv_dddd(
	double *const Stime,
	double *const m,
	double *const time1,
	double *const time2,
	const int len)
{
	register int i = 0;
	int j, k;
	sort_dddd(Stime, m, time1, time2, len); // sort data
	while (i < len) { // loop through the sample until last index is reached
		for (i++, j = 1; i < len && Stime[i] == Stime[i-1]; i++) { // loop through the sample until Stime changes or last index is reached
			j++; // count equal Stimes
		}
		if (j > 1) { // if there are equal Stimes
			k = i-j;
			sort_rev_ddd(&m[k], &time1[k], &time2[k], j); // put censored observations last
		}
	}
	return;
} // sort_surv_dddd

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

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
	sort_didi(time1, event1, time2, event2, *len); // sort data
	return;
} // BivSortCKM

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

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
	sort_surv_didd(Stime, event2, time1, time2, *len); // sort data
	return;
} // BivSortKMW

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

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
	sort_surv_dddd(Stime, m, time1, time2, *len); // sort data
	return;
} // BivSortKMPW

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistIPCW1' function.

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

void BivSortIPCW1(
	double *const time1,
	int *const event1,
	double *const time2,
	int *const event2,
	double *const Stime,
	const int *const len)
{
	sort_did(time1, event1, time2, *len); // sort time1, event1 and time2
	rsort_with_index(Stime, event2, *len); // sort Stime and event2
	return;
} // BivSortIPCW1

/*
Author:
	Artur Agostinho Araújo <b5498@math.uminho.pt>

Description:
	Sorts data to be inputed to the 'BivDistIPCW2' function.

Parameters:
	time1[inout]	pointer to time1 first element
	time2[inout]	pointer to time2 first element
	Stime[inout]	pointer to Stime first element
	len[in]			pointer to length of time1, time2 and Stime

Return value:
	This function doesn't return a value.

Remarks:
	Vectors time1, time2 and Stime must have the same length.
*/

void BivSortIPCW2(
	double *const time1,
	double *const time2,
	double *const Stime,
	const int *const len)
{
	sort_dd(time1, time2, *len); // sort time1 and time2
	R_rsort(Stime, *len); // sort Stime
	return;
} // BivSortIPCW2

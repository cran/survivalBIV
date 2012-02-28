\name{NEWS}

\title{NEWS file for the surivalBIV package}

\section{Changes in version 1.3}{
	\itemize{
		\item The entries for S3 methods for plot.survBIV and summary.survBIV was changed.

		\item Added a file CITATION
		
	}
}

\section{Changes in version 1.2}{
	\itemize{
		\item Implemented efficient algorithms for the computation of probability
		matrices to be used internally in the 'plot.survBIV' function.

		\item Fixed IPCW algorithm returning wrong results when using 'prodlim' package.
		
		\item Renamed corr.BIV function to corrBIV.
		
		\item Renamed data.gen.BIV function to dgpBIV.
	}
}

\section{Changes in version 1.1}{
	\itemize{
		\item Rewritten part of the original code in C, resulting in significant
		increases in performance. The R code for functions bivCKM, bivIPCW
		and bivKMW was entirely rewritten.

		\item Removed KMW function.

		\item Removed adapt function. Function survBIV was written to serve the
		same purpose. Added function is.survBIV.

		\item Fixed function bivKMW returning wrong results for the pre-smoothed
		estimators.

		\item Function bivKMW now only computes the non pre-smoothed version of
		the KMW estimator.

		\item Added bivKMPW function for the pre-smoothed estimator based on the
		logit generalized linear model.

		\item Removed code for the pre-smoothed estimator based on the generalized
		additive model.

		\item Renamed bladder2 dataset to bladderBIV.
	}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_DT_performance.R
\name{calculate_DT_performance}
\alias{calculate_DT_performance}
\title{Calculate the performances of a DT}
\usage{
calculate_DT_performance(pathDT, testMetrics = NULL,
  testPressures = NULL, low = "low", impaired = "impaired",
  smoothROC = TRUE)
}
\arguments{
\item{pathDT}{character string, the path where the built models will be saved}

\item{low}{character vectors with the labels of the pressure classes
(in \code{pressures}) corresponding to low impact and impaired situations,
respectively.}

\item{impaired}{character vectors with the labels of the pressure classes
(in \code{pressures}) corresponding to low impact and impaired situations,
respectively.}

\item{smoothROC}{if TRUE, the ROC curve is passed to \link[pROC:smooth]{smooth}
to be smoothed.}
}
\value{
a list with two elements:
- \code{AUC}: a data frame with the calculated AUC in both the training and
test data sets;
- \code{ROC}: a list of ggplot objects (one per DT model) containing the ROC
curves
}
\description{
This function allows to quantify the performances of a DT model built using
the  \link{build_DT} function, ideally using a different data set than
the one used to train the DT.
}
\details{
The function will calculate the ROC curves (smoothed or not depending on
argument \code{smoothROC}) and the corresponding Area Under the Curve (AUC) for
each of the DT models using (i) the test data set stored with the models
and (ii) a different test data set (provided using \code{testPressures} and
\code{testMetrics}).
}
\seealso{
\link[pROC:roc]{roc} \link{build_DT}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{obs_v_pred_plot}
\alias{obs_v_pred_plot}
\title{Observed against predicted plot}
\usage{
obs_v_pred_plot(
  yobs,
  predsums,
  segments = TRUE,
  summarystat = "median",
  plotit = TRUE
)
}
\arguments{
\item{yobs}{A vector containing the actual observations}

\item{predsums}{A data frame containing predictive summary 
statistics with the same number of rows as the length of the vector yobs. 
The data frame must have columns named as meanpred, medianpred, sd, low and up. 
Ideally this argument should be the output of the command 
\code{\link{get_validation_summaries}}.}

\item{segments}{Logical: whether to draw line segments for the prediction intervals.}

\item{summarystat}{Can take one of two values "median" (default) or "mean" 
indicating which one to use for the plot.}

\item{plotit}{Logical scalar value: whether to plot the predictions against the observed values.}
}
\value{
Draws a plot only after removing the missing observations.  It also returns a list of two ggplot2 
objects: (i) a plot with intervals drawn \code{pwithseg} and (ii) a plot without the segments drawn: 
\code{pwithoutseg} and (iii) a simple plot not showing the range of the prediction intervals.
}
\description{
Observed against predicted plot
}
\examples{
set.seed(4)
vrows <- sample(nrow(nysptime), 100)
M1 <- Bsptime(model="lm", formula=y8hrmax~xmaxtemp+xwdsp+xrh, data=nysptime, 
validrows=vrows, scale.transform = "SQRT")
psums <-  get_validation_summaries(M1$valpreds)
oplots <- obs_v_pred_plot(yobs=M1$yobs_preds$y8hrmax, predsum=psums)
names(oplots)
plot(oplots$pwithoutseg)
plot(oplots$pwithseg)
}

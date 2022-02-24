\name{rf.null_test}
\alias{rf.null_test}

\title{ (Non)Parametric Hypothesis testing for Leave-One-Out Models}

\description{
Function to test a generated Leave-One-Out model against the bootstrapped estimations for hypothesis testing. Requires a LOO model and a bootstrapped model built using the randomForestLOO pacakge.
}
\usage{
rf.null_test(loo_input, bootstrap_error, model_type, method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loo_input}{A LOO model estimated using the randomForestLOO package. }
  \item{bootstrap_error}{Bootstrap error model derived from the randomForestLOO package. }
  \item{xmodel_type}{String. Can be "norm" or "actual". "actual" returns results for the null test using the full distribution of the errors estimated from the bootstrapped models. "norm" returns results of the null test based on the overall mean and standard deviations under assumptions of normality. "actual" recommended. }
  \item{method}{String. Indicates if the model used is a "LOSO" or "LOBO" model.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    \item{p_val }{List of p-values obtained for each participant.}
    \item{stouffer }{A combined p-value across all participants using Stouffer's Method.}
%% ...
}
\references{
Using wearable biosensors and ecological momentary assessments for the detection of prolonged stress in real life
Rayyan Tutunji, Nikos Kogias, Bob Kapteijns, Martin Krentz, Florian Krause, Eliana Vassena, Erno Hermans
bioRxiv 2021.06.29.450360; doi: https://doi.org/10.1101/2021.06.29.450360
}
\author{
Rayyan Tutunji | rayyan.tutunji[at]donders.ru.nl
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ randomForest }% use one of  RShowDoc("KEYWORDS")
\keyword{ parametric }% __ONLY ONE__ keyword per line

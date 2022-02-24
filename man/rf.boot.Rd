\name{rf.boot}
\alias{rf.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap analysis for null hypothesis testing of random forests with Leave-One-Out procedures.
}
\description{
Function to run a bootstrap analysis with leave-one-out analysis. Returns a dataframe with all bootstrap errors for the estimations, and the average errors per subject.
}
\usage{
rf.boot(data, sub_id, xvar, yvar, method, ntree, iterations, ncore)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    Dataframe to be used by model
}
  \item{sub_id}{
    String indicating what variable to use as subject ID
}
  \item{xvar}{
   Vector of strings to be used for model training
}
  \item{yvar}{
   A response string. Must be of type factor.
}
  \item{method}{
   String, can be "LOSO" or "LOBO". Default="LOBO".
}
  \item{ntree}{
   Integer indicating number of trees to simulate. Default=1000.
}
  \item{iterations}{
    Number of times to run bootstrap. Default=5000.
}
  \item{ncore}{
  Number of cores assigned for parallel processing. Default=1 (Not recommended). Will always use n-1 to ensure at least one core remains available and free for smooth operaitons.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Results of the bootstrap analysis as a list:
 \item{model}{Information on the model that was run}
 \item{subject}{Results for each subject, including the predictions made, a confusion matrix, and the mean error}
 \item{errors}{Mean error rate returned per subject}
  \item{mean_errors}{Mean error rate across subjects}
%% ...
}

\author{
Rayyan Tutunji | rayyan.tutunji[at]donders.ru.nl
}
\note{
Currenlty implemented only for factors. Random forests are not optimal for regressions, limiting the utility of such applicaitons. Note that sometimes the total number of iterations may be slightly different. This is because failed bootstraps are discarded from the final dataframe.
}

\references{
Using wearable biosensors and ecological momentary assessments for the detection of prolonged stress in real life
Rayyan Tutunji, Nikos Kogias, Bob Kapteijns, Martin Krentz, Florian Krause, Eliana Vassena, Erno Hermans
bioRxiv 2021.06.29.450360; doi: https://doi.org/10.1101/2021.06.29.450360
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bootstrap }% use one of  RShowDoc("KEYWORDS")
\keyword{ randomForest }% __ONLY ONE__ keyword per line

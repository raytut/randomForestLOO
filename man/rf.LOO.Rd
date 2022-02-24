\name{rf.LOO}
\alias{rf.LOO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random Forests with Leave-One-Out Procedures
}
\description{
Function to run a Leave-One-Out random forest model. Two methods can be specified: LOSO and LOBO.

LOSO (Leave-One-Subject-Out) will remove one entire subject from the training set, and then test the model on the removed subject, repeating on all subjects in the dataframe. This offers population level classification.

LOBO (Leave-One-Beep-Out) offers within subject level predictions for EMA(Ecological Momentary Assessment) type data by training models on single subjects minus one Beep. Classification are thus derived for each subject this way. This analysis can be implemented on other types of trial based data.
}
\usage{
rf.LOO(data, sub_id, xvars, yvar, NoTree, method, progress)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    Dataframe to be used by model
}
  \item{sub_id}{
    String indicating what variable to use as subject ID
}
  \item{xvars}{
   Vector of strings to be used for model training
}
  \item{yvar}{
   A response string. Must be a character/factor. Otherwise, use rf.LOOreg for regression version (in development).
}
  \item{ntree}{
   Integer indicating number of trees to simulate. Defaults to 1000.
}
  \item{method}{
   String, can be "LOSO" or "LOBO". Defaults to "LOBO".
}
  \item{progress}{
  Logical indicating if progress to be printed. Default True.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Results of the LOO analysis as a list:
 \item{model}{Information on the model that was run}
 \item{subject}{Results for each subject, including the predictions made as a list, a confusion matrix, and the mean error}
 \item{errors}{Mean error rate returned per subject}
  \item{mean_errors}{Population level error rate across subjects}
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
Currenlty implemented only for factors, and tested for two and three levels.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LOBO }% use one of  RShowDoc("KEYWORDS")
\keyword{ randomForest }% __ONLY ONE__ keyword per line

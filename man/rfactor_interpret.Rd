\name{rfactor_interpret}
\alias{rfactor_interpret}
\title{
R Factor for Liver Injury Interpreted
}
\description{
Using alanine aminotransferase and alkaline phosphatase calculate an index that can distinguish between hepatocellulal, cholestatic, or a combination of both in patients presenting with acute liver injury. Index value is then interpreted in the ocntext of hepatocellular or cholestatic disease. Recommended for use in risk stratification and diagnosis by the American College of Gastroenterology.
}
\usage{
rfactor_interpret(alt, alp)
}
\arguments{
  \item{alt}{
Numeric value of alanine aminotransferase (U/L).
}
  \item{alp}{
Numeric value of alkaline phosphatase (U/L).
}
}
\value{
A numeric value/vector with predicted R factor score.
}
\references{
Bénichou C. Criteria of drug-induced liver disorders. Report of an international consensus meeting. J Hepatol. 1990;11(2):272-6.
Chalasani NP, Hayashi PH, Bonkovsky HL, et al. ACG Clinical Guideline: the diagnosis and management of idiosyncratic drug-induced liver injury. Am J Gastroenterol. 2014;109(7):950-66.
}
\examples{

# The function is defined as
rfactor_interpret(40, 50)
}

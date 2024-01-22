\name{eAG}
\alias{eAG}
\title{
Calculate eAG (Estimated Average Glucose)
}
\description{
Using HbA1C (hemoglobin A1C) levels, estimate the average amount of glucose in one's body, derived by Nathan et al. 2008. Usage represents the average glucose level for a cumulative period of 3 months and has been shown to conocordant with fasting glucose levels. HbA1C levels range between 4-5.6 in non-diabetic populations.
}
\usage{
eAG(a1c)
}
\arguments{
  \item{a1c}{
Numeric value of percent glycolated hemoglobin (HbA1C).
}
}
\value{
A numeric value/vector with predicted level of estimated average glucose.
}
\references{
Nathan DM, Kuenen J, Borg R, et al. Translating the A1C assay into estimated average glucose values [published correction appears in Diabetes Care. 2009 Jan;32(1):207]. Diabetes Care. 2008;31(8):1473-1478. doi:10.2337/dc08-0545
}
\examples{

# The function is defined as
eAG(4)

}


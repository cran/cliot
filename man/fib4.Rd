\name{fib4}
\alias{fib4}
\title{
Calculate Fibrosis-4 (FIB-4) Index for Liver Fibrosis
}
\description{
Calculate the extent of liver fibrosis/scarring in HCV/HBV patients without biopsy or ultrasound, as deduced by Sterling et al. 2006. Patient validation has not been included in patients <35 or above 65. As validated by Kim et al. 2010 and McPherson et al. 2017, a FIB-4 score of <1.45 is a fibrosis stage of 0-1, FIB-4 1.45-3.25 is a stage of 2-3, and FIB-4 of >3.25 is a tage of 4-6.
}
\usage{
fib4(age, ast, alt, plt)
}
\arguments{
  \item{age}{
Numeric value of age in years.
}
  \item{ast}{
Numeric value of aspartate aminotransferase in U/L.
}
  \item{alt}{
Numeric value of alanine aminotransferase in U/L.
}
  \item{plt}{
Numeric value of platelets in 10^3/uL or 10^9/L.
}
}
\value{
A numeric value/vector with predicted level of FIB-4 index liver fibrosis.
}
\references{
Sterling, R.K., Lissen, E., Clumeck, N., Sola, R., Correa, M.C., Montaner, J., S. Sulkowski, M., Torriani, F.J., Dieterich, D.T., Thomas, D.L., Messinger, D. and Nelson, M. (2006), Development of a simple noninvasive index to predict significant fibrosis in patients with HIV/HCV coinfection. Hepatology, 43: 1317-1325. https://doi.org/10.1002/hep.21178
Kim BK, Kim DY, Park JY, et al. Validation of FIB-4 and comparison with other simple noninvasive indices for predicting liver fibrosis and cirrhosis in hepatitis B virus-infected patients. Liver Int. 2010;30(4):546-553. doi:10.1111/j.1478-3231.2009.02192.x
Mcpherson S, Hardy T, Dufour JF, et al. Age as a Confounding Factor for the Accurate Non-Invasive Diagnosis of Advanced NAFLD Fibrosis. Am J Gastroenterol. 2017;112(5):740-751.
}
\examples{

# The function is defined as
fib4(40,15,7,175)

}


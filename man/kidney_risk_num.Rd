\name{kidney_risk_num}
\alias{kidney_risk_num}
\title{
Calculate Kidney Failure Risk Index
}
\description{
Using commonly available lab values, calculate the risk index of developing kidney failure in patients with CKD, as devised by Tangri et al. 2011 and validated by Tangri et al. 2016.
}
\usage{
kidney_risk_num(egfr, sex, acr, age, alb, phos, bicarb, calc, units="")
}
\arguments{
  \item{egfr}{
Numeric value of eGFR in mL/min/1.73m^2.
}
  \item{sex}{
Char of M (male) or F (female).
}
  \item{acr}{
Numeric value of urine albumin/creatinine ratio in mg/g.
}
  \item{age}{
Numeric value of age in years.
}
  \item{alb}{
Numeric value of serum albumin in either non-SI (g/dL) or SI (g/L).
}
  \item{phos}{
Numeric value of serum phosphorous in either non-SI (mg/dL) or SI (mmol/L).
}
  \item{bicarb}{
Numeric value of serum bicarbonate in either non-SI (mEq/L) or SI (mmol/L).
}
  \item{calc}{
Numeric value of serum calcium in either non-SI (mg/dL) or SI (mmol/L).
}
  \item{units}{
String to specify non-SI units by not including units argument or SI units by including units="SI".
}
}
\value{
A numeric value/vector of the chance of developing kidney failure.
}
\references{
Tangri N, Stevens LA, Griffith J, et al. A predictive model for progression of chronic kidney disease to kidney failure. JAMA. 2011;305(15):1553-9.
Tangri N, Grams ME, Levey AS, et al. Multinational Assessment of Accuracy of Equations for Predicting Risk of Kidney Failure: A Meta-analysis. JAMA. 2016;315(2):164-74.
}
\examples{

# For non-SI measurements, the function is defined as
kidney_risk_num(10, "F", 29, 31, 2.7, 3.6, 18, 10)

# For SI measurements, the function is defined as
kidney_risk_num(10, "F", 29, 31, 2.7, 2, 18, 2.5, units = "SI")

}


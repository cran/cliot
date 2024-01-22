\name{meld_xi}
\alias{meld_xi}
\title{
Calculate MELD-XI (Model for End Stage Liver Disease Excluding INR)
}
\description{
Using serum bilirubin and serum creatinine, calculate the MELD Exclusing INR, which models closely with MELD, as devised by Heuman et al. 2007. Validated for use by Wernly et al. 2017, cutoffs for MELD and MELD-XI appear identical in practice, the former of which used by UNOS for organ transplantation need. More research needs to be done to calculate associated survival or cutoff values.
}
\usage{
meld_xi(sbill, screat)
}
\arguments{
  \item{sbill}{
Numeric value of serum bilirubin in mg/dL.
}
  \item{screat}{
Numeric value of serum creatinine in mg/dL.
}

}
\value{
A numeric value/vector with MELD-XI score (comparable to MELD scores).
}
\references{
Heuman, D.M., Mihas, A.A., Habib, A., Gilles, H.S., Stravitz, R.T., Sanyal, A.J. and Fisher, R.A. (2007), MELD-XI: A rational approach to “sickest first” liver transplantation in cirrhotic patients requiring anticoagulant therapy. Liver Transpl, 13: 30-37. https://doi.org/10.1002/lt.20906
Wernly B, Lichtenauer M, Franz M, et al. Model for End-stage Liver Disease excluding INR (MELD-XI) score in critically ill patients: Easily available and of prognostic relevance. PLoS One. 2017;12(2):e0170987. Published 2017 Feb 2. doi:10.1371/journal.pone.0170987
}
\examples{

# The function is defined as
meld_xi(0.6,0.65)

}


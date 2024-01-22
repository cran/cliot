\name{apri}
\alias{apri}
\title{
Calculate APRI (AST to Platelet Ratio)
}
\description{
Using aspartate aminotransferase and platelet values, calculate the APRI index, devised by Wait et al 2003, estimates hepatic fibrosis and cirrhosis in patients with hepatitis C. APRI value <0.7 is indicative of little to no evidence of fibrosis, 0.7-1.0 is moderate fibrosis, and 1.0+ is evidence of cirrhosis and significant fibrosis, as validated by Khan et al. 2008 and Lin et al 2011.
}
\usage{
apri(ast, plt)
}
\arguments{
  \item{ast}{
Numeric value of aspartate aminotransferase in U/L.
}
  \item{plt}{
Numeric value of platelets in 10^3/uL or 10^9/L.
}
}
\value{
A numeric value/vector with predicted APRI index score.
}
\references{
Wai CT, Greenson JK, Fontana RJ, et al. A simple noninvasive index can predict both significant fibrosis and cirrhosis in patients with chronic hepatitis C. Hepatology. 2003;38(2):518-526. doi:10.1053/jhep.2003.50346
Khan DA, Fatima-Tuz-Zuhra, Khan FA, Mubarak A. Evaluation of diagnostic accuracy of APRI for prediction of fibrosis in hepatitis C patients. J Ayub Med Coll Abbottabad. 2008;20(4):122-126.
Lin, Z.-H., Xin, Y.-N., Dong, Q.-J., Wang, Q., Jiang, X.-J., Zhan, S.-H., Sun, Y. and Xuan, S.-Y. (2011), Performance of the aspartate aminotransferase-to-platelet ratio index for the staging of hepatitis C-related fibrosis: An updated meta-analysis. Hepatology, 53: 726-736. https://doi.org/10.1002/hep.24105
}
\examples{

# The function is defined as
apri(16,150)

}


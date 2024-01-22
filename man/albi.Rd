\name{albi}
\alias{albi}
\title{
Calculate ALBI (Albumin-Bilirubin) Grade for HCC (Hepatocellular Carcinoma)
}
\description{
Using non-SI/SI measurements of serum bilirubin and serum albumin, calculate the ALBI grade for patients with HCC. Developed by Johnson et al. 2015, an ALBI score of =<-2.60 is grade 1 with a median survival of 18.5-85.6 months, an ALBI score of -2.60 to (and including) -1.39 is grade 2 with a median survival of 5.3-46.5 months, and an ALBI score of >-1.39 is grade 3 with a median survival of 2.3-15.5 months. Validated by Hiroaka et al. 2017 and Chen et al. 2017.
}
\usage{
albi(sbill, salb, units = "")
}
\arguments{
  \item{sbill}{
Numeric value of serum bilirubin in either non-SI (mg/dL) or SI (umol/ml).
}
  \item{salb}{
Numeric value of serum albumin in either non-SI (g/dL) or SI (g/L)
}
  \item{units}{
String to specify non-SI units by not including units argument or SI units by including units="SI"
}
}
\value{
A numeric value/vector with predicted ALBI score.
}
\references{
Johnson PJ, Berhane S, Kagebayashi C, et al. Assessment of liver function in patients with hepatocellular carcinoma: a new evidence-based approach-the ALBI grade. J Clin Oncol. 2015;33(6):550-558. doi:10.1200/JCO.2014.57.9151
Hiraoka A, Michitaka K, Kumada T, et al. Validation and Potential of Albumin-Bilirubin Grade and Prognostication in a Nationwide Survey of 46,681 Hepatocellular Carcinoma Patients in Japan: The Need for a More Detailed Evaluation of Hepatic Function. Liver Cancer. 2017;6(4):325-336. doi:10.1159/000479984
Chen B, Lin S. Albumin-bilirubin (ALBI) score at admission predicts possible outcomes in patients with acute-on-chronic liver failure. Medicine (Baltimore). 2017;96(24):e7142. doi:10.1097/MD.0000000000007142
}
\examples{

# For non-SI measurements, the function is defined as
albi(1,3.7)

# For SI measurements, the function is defined as
albi(31,37, units = "SI")
}


\name{nafld_score}
\alias{nafld_score}
\title{
Non-Alcoholic Fatty Liver Disease (NAFLD) Fibrosis Score
}
\description{
Using SI/non-SI measurements of albumin, and age, bmi, impaired fasting glucose/diabetes, aspartate trasnferase, alanine transferase, and platelet counts, calcualte the level of scarring in the liver from F0-F2 (no-moderate fibrosis) to F3-F4 (severe fibrosis - cirrhosis). Scores below -1.455 are associated with F0-F2 and scores above 0.675 are associated with F3-F4, with inbetween scores being indeterminate. Created by Angulo et al. (2007) and validated by Treeprasertsuk et al. (2013).
}
\usage{
nafld_score(age, bmi, diabetes, ast, alt, platelet, albumin, units = "")
}
\arguments{
  \item{age}{
Numeric value of age.
}
  \item{bmi}{
Numeric value of bmi (kg/m^2).
}
  \item{diabetes}{
If impaired fasting glucose or type 2 diabetes mellitus is true, 1, if not, 0.
}
  \item{ast}{
Numeric value of aspartate transferase (U/L).
}
  \item{alt}{
Numeric value of alanine transferase (U/L).
}
  \item{platelet}{
Numeric value of platelet count in either 10^9/L or 10^3/uL.
}
  \item{albumin}{
Numeric value of serum albumin in either non-SI (g/dL) or SI (g/L).
}
  \item{units}{
String to specify non-SI units by not including units argument or SI units by including units="SI".
}
}
\value{
A numeric value/vector with predicted NAFLD fibrosis score.
}
\references{
Angulo P, Hui JM, Marchesini G, Bugianesi E, George J, Farrell GC, Enders F, Saksena S, Burt AD, Bida JP, Lindor K, Sanderson SO, Lenzi M, Adams LA, Kench J, Therneau TM, Day CP. The NAFLD fibrosis score: a noninvasive system that identifies liver fibrosis in patients with NAFLD. Hepatology. 2007 Apr;45(4):846-54. PMID: 17393509.
Treeprasertsuk S, Björnsson E, Enders F, Suwanwalaikorn S, Lindor KD. NAFLD fibrosis score: A prognostic predictor for mortality and liver complications among NAFLD patients. World Journal of Gastroenterology. 2013;19(8):1219-1229. doi:10.3748/wjg.v19.i8.1219.
Tapper EB, Lok AS. Use of Liver Imaging and Biopsy in Clinical Practice. N Engl J Med. 2017;377(8):756-768.
}
\examples{

# For non-SI measurements, the function is defined as
nafld_score(50, 28, 1, 15, 2, 170, 3.5, units = "")

# For SI measurements, the function is defined as
nafld_score(50, 28, 1, 15, 2, 170, 3.5, units = "SI")
}

\name{bard_score_interpret}
\alias{bard_score_interpret}
\title{
BARD Score for assessing Fibrosis in NAFLD Interpretation
}
\description{
By using a patient's bmi, lab values of aspartate transferae, alanine transferase, and whether they have type 2 diabetes, calculate the patient's risk for fibrosis, especially if they have diagnosed or suspected NAFLD. Created by Harrison et al. 2008. Validated by Ruffillo et al. (2011) and Raszeja-wyszomirska et al. (2010).
}
\usage{
bard_score_interpret(bmi, ast, alt, diabetes)
}
\arguments{
  \item{bmi}{
Numeric value of bmi (kg/m^2).
}
  \item{ast}{
Numeric value of aspartate transferase (U/L).
}
  \item{alt}{
Numeric value of alanine transferase (U/L).
}
  \item{diabetes}{
Numeric value of type 2 diabetes mellitus status yes = 1, no = 0.
}
}
\value{
A string with interpreted BARD score as "high" or "low" fibrosis.
}
\references{
Harrison SA, Oliver D, Arnold HL, Gogia S, Neuschwander-tetri BA. Development and validation of a simple NAFLD clinical scoring system for identifying patients without advanced disease. Gut. 2008;57(10):1441-7.
Raszeja-wyszomirska J, Szymanik B, Ławniczak M, et al. Validation of the BARD scoring system in Polish patients with nonalcoholic fatty liver disease (NAFLD). BMC Gastroenterol. 2010;10:67.
Ruffillo G, Fassio E, Alvarez E, et al. Comparison of NAFLD fibrosis score and BARD score in predicting fibrosis in nonalcoholic fatty liver disease. J Hepatol. 2011;54(1):160-3.
Cichoż-lach H, Celiński K, Prozorow-król B, Swatek J, Słomka M, Lach T. The BARD score and the NAFLD fibrosis score in the assessment of advanced liver fibrosis in nonalcoholic fatty liver disease. Med Sci Monit. 2012;18(12):CR735-40.
}
\examples{

# The function is defined as
bard_score_interpret(25, 40, 32, 1)

}

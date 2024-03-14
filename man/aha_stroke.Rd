\name{aha_stroke}
\alias{aha_stroke}

\title{
Calculate Stroke Risk for General Population based off AHA Patient Stroke risk calculator
}
\description{
Using common patient history of comorbidities and conditions, in addition to traditional lifestyle factors, assess the total risk of suffering a stroke. Developed by the AHA and revised in 2023 for patient use, a positive score is associated with a high likelihood of suffering a stroke while a negative score has a low likelihood. Has not been validated for clinical use.
}
\usage{
aha_stroke(bp, afib, sugar, bmi, diet, chol, dm, physical, hx, smoke)

}
\arguments{
  \item{bp}{
Blood pressure greater than 120/80? 1 for yes or unsure / 0 for no.
}
  \item{afib}{
Diagnosed atrial fibrillation? 1 for yes or unsure / 0 for no.
}
  \item{sugar}{
Blood sugar greater than 100 mg/dL? 1 for yes or unsure / 0 for no.
}
 \item{bmi}{
BMI >25 kg/m^2? 1 for yes or unsure / 0 for no.
}
  \item{diet}{
Diet high in saturated/trans fats, sweetened bevs, salt, excess calories? 1 for yes or unsure / 0 for no.
}
  \item{chol}{
Total blood cholesterol >160 mg/dL? 1 for yes or unsure / 0 for no.
}
 \item{dm}{
Diagnosis of T2 diabetes mellitus? 1 for yes or unsure / 0 for no.
}
  \item{physical}{
Less than 150 minutes of moderate to vigorous intensite activity per week? 1 for yes or unsure / 0 for no.
}
  \item{hx}{
Personal or family hx of stroke, TIA, or heart attack? 1 for yes or unsure / 0 for no.
}
  \item{smoke}{
Use tobacco or vape? 1 for yes or unsure / 0 for no.
}
}
\value{
A numeric value/vector with predicted stroke risk score.
}
\references{
“Stroke Risk Assessment.” www.stroke.org, December 14, 2023. https://www.stroke.org/en/about-stroke/stroke-risk-factors/stroke-risk-assessment.
}
\examples{

# The function is defined as
aha_stroke(1, 0, 1, 1, 0, 0, 0, 0, 1, 0)
}

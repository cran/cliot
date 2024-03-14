\name{fli}
\alias{fli}
\title{
Calculate FLI (Fatty Liver Index)
}
\description{
Using non-SI/SI lab values and measurements, diagnose the risk of having a fatty liver without the need for liver ultrasound. Created by Bedogni et al. 2006, this index has been validated by Cuthberson et al. 2014 and Koehler et al. 2013 for use in multimodal populations, with FLI values of <30 being low risk of fatty liver (LR= -0.2), FLI betwen 30 and 60 being indeterminate of risk, and FLI >60 indicative of high risk of fatty liver (LR = 4.3).
}
\usage{
fli(trigly, bmi, waist, ggt, units = "")
}
\arguments{
  \item{trigly}{
Numeric value of triglycerides in either SI (mmol/L) or non-SI (mg/dL).
}
  \item{bmi}{
Numeric value of BMI in kg/m^2.
}
  \item{waist}{
Numeric value of waist circumference in cm.
}
  \item{ggt}{
Numeric value of gamma glutyl transpeptidase (GGT) in U/L.
}
  \item{units}{
String to specify non-SI units by not including units argument or SI units by including units="SI".
}
}
\value{
A numeric value/vector with predicted fatty liver index value.
}
\references{
Bedogni, G., Bellentani, S., Miglioli, L. et al. The Fatty Liver Index: a simple and accurate predictor of hepatic steatosis in the general population. BMC Gastroenterol 6, 33 (2006). https://doi.org/10.1186/1471-230X-6-33
Cuthbertson DJ, Weickert MO, Lythgoe D, et al. External validation of the fatty liver index and lipid accumulation product indices, using 1H-magnetic resonance spectroscopy, to identify hepatic steatosis in healthy controls and obese, insulin-resistant individuals. Eur J Endocrinol. 2014;171(5):561-569. doi:10.1530/EJE-14-0112
Koehler EM, Schouten JN, Hansen BE, Hofman A, Stricker BH, Janssen HL. External validation of the fatty liver index for identifying nonalcoholic fatty liver disease in a population-based study. Clin Gastroenterol Hepatol. 2013;11(9):1201-4.

}
\examples{

# For non-SI measurements, the function is defined as
fli(125, 20, 25, 16)

# For SI measurements, the function is defined as
fli(1.5, 20, 25, 16, units = "SI")
}


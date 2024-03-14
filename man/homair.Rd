\name{homair}
\alias{homair}
\title{
Calculate HOMA-IR (Homeostatic Model Assessment for Insulin Resistance)
}
\description{
Using non-SI/SI measurements of glucose and insulin, approximate the levels of insulin resistance (or performance of B-cells) by means of the Matthews et al. 1985 formula. According to Cacho et al. 2008, normal measurements vary from 0.7-2, but will depend on the exact reference population being used. A value of above 2 indicates insulin resistance and a value below 0.7 is inconclusive.
}
\usage{
homair(insulin,glucose, units = "")
}
\arguments{
  \item{insulin}{
Numeric value of insulin in either SI (pmol/L) or non-SI (uIU/ml).
}
  \item{glucose}{
Numeric value of glucose in either non-SI (mg/dL) or non-SI (mmol/L).
}
  \item{units}{
String to specify non-SI units by not including units argument or SI units by including units="SI".
}
}
\value{
A numeric value/vector with predicted level of insulin resistance.
}
\references{
Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC. Homeostasis model assessment: insulin resistance and beta-cell function from fasting plasma glucose and insulin concentrations in man. Diabetologia. 1985;28(7):412-419. doi:10.1007/BF00280883
Cacho J, Sevillano J, de Castro J, Herrera E, Ramos MP. Validation of simple indexes to assess insulin sensitivity during pregnancy in Wistar and Sprague-Dawley rats. Am J Physiol Endocrinol Metab. 2008;295(5):E1269-E1276. doi:10.1152/ajpendo.90207.2008
}
\examples{

# For non-SI measurements, the function is defined as
homair(20,70)

# For SI measurements, the function is defined as
homair(100,4, units = "SI")
}


\name{ast_alt_ratio_interpret}
\alias{ast_alt_ratio_interpret}
\title{
De Ritis Ratio (Ast/Alt Ratio)
}
\description{
The De Ritis Ratio has been used clinically and in research for almost 50 years and is simply the ratio of aspartate transferase to alanine transferase. Validated in many cohorts, interpretations vary, and as such, I simply define a ratio of above 0.8 as indicative of liver injury, commonly agreed upon in the literature.
}
\usage{
ast_alt_ratio_interpret(ast, alt)
}
\arguments{
  \item{ast}{
Numeric value of aspartate transferase (U/L).
}
  \item{alt}{
Numeric value of alanine transferase (U/L).
}
}
\value{
A numeric value/vector with whether by De Ritis ratio, there is liver injury.
}
\references{
Botros M, Sikaris KA. The de ritis ratio: the test of time. Clin Biochem Rev. 2013 Nov;34(3):117-30. PMID: 24353357; PMCID: PMC3866949.
}
\examples{

# The function is defined as
ast_alt_ratio_interpret(30, 25)

}

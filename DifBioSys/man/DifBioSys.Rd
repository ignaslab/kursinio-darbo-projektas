\name{Kursinio darbo projektas}
\alias{Kursinio darbo projektas}
\title{Kursinio darbo projektas}
\usage{
}
\description{
Kursinio darbo projektas.
}
\examples{
t <- (0:10000)/100
Dif <- data.frame(d = c(0.025,0.025),x=c(0,1))
dF <- data.frame(t = t, f0 = abs(2*sin(t/5)), f1 = abs(2*cos(t/5)))
solveDifEq(1,30,30, dF, Dif)
plotDiffusion(Df)
}

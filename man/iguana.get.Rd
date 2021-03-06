% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iguana.get.R, R/iguanaSources.R
\name{getNews}
\alias{getNews}
\alias{iguanaSources}
\title{Get data from the Iguana API}
\usage{
getNews(token, fonte, incerteza, datainicio, datafim, limite,
  categoria = c("Cotidiano", "Educacao", "Esporte", "Poder", "Mundo",
  "Ilustrada", "Mercado", "Ciencia", "Equilibrio", "Turismo", "BBC Brasil",
  "Tec", "Podcasts", "Veiculos", "Colunistas", "Opiniao", "Comida", "Imoveis",
  "Negocios", "Especial", "Equilibrio e Saude", "Ambiente", "Empregos",
  "Folha Corrida"))

iguanaSources()
}
\arguments{
\item{token}{A \code{string}. The access code to use Iguana API}

\item{fonte}{PARAM_DESCRIPTION}

\item{datainicio}{PARAM_DESCRIPTION}

\item{datafim}{PARAM_DESCRIPTION}

\item{categoria}{PARAM_DESCRIPTION}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
This function acts as a wrapper to the Iguana API, providing an easy access to the data through R.

This function show the sources available to request news in iguana api
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\keyword{get}
\keyword{news}

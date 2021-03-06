% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fastq.R
\name{gc_content}
\alias{gc_content}
\title{Calculate GC content of DNA sequence}
\usage{
gc_content(seq)
}
\arguments{
\item{seq}{DNA sequence (character string)}
}
\value{
single value of the proportion of G and C bases in the DNA sequence
}
\description{
Calculate GC content of DNA sequence
}
\examples{
gc_content("ACGTTTTGACGCATACCGAGCGCC")
}

\name{tRnslate-package}
\alias{tRnslate-package}
\alias{tRnslate}
\title{
'Translate R Code in Source Files'
}
\description{
Evaluate inline or chunks of R code in template files and replace with their output modifying the resulting template.
}
\details{
Function \code{translate_r_code} receives a character vector with the lines of a template file which contains R code inline and in R chunks. This R code is evaluated in an environment defined by the user and replaces its output in the template returning a character vector with the lines of the resulting template.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~ 
\examples{
library(tRnslate)
# Read template containing R code inline or in chunks
T <- readLines(system.file("examples/template.txt", package = "tRnslate"))
# Create and environment to evaluate the R code in the template.
# Define objects in the environmet which are used to modify the template.
renv <- new.env(parent = parent.frame())
renv$s <- list(
intro = "#SBATCH",
partition = "hpc01",
nodes = 4,
tasks = 10,
memory = "2gb",
time = "01:00:00",
array = FALSE,
modules = 'module load openmpi/chosen/module R/chosen/module',
workdir = 'cd ${SLURM_SUBMIT_DIR}'
)
# Evaluate the R sentences in the template using the objects in the input environment.
TT <- translate_r_code(T, envir = renv)
# See the lines of the resulting template (or using 'cat' and newline as separator)
TT
}


\name{Translate code}
\alias{Translate code}
\alias{translate_r_code}
\title{
Translate code
}
\description{
Evaluate inline or chunks of R code present in general template files to produce variable content depending on some input arguments.
}
\usage{
translate_r_code(x, chunk_prefix = NULL, chunk_char = "@",
        chunk_times = 1, inline_open = "<", inline_char = "@",
        inline_close = ">", char_begin = "", char_clean = "<:NULL:>",
        char_drop = "<:NULL:>",
        envir = new.env(parent = parent.frame()), comments = TRUE,
        reduce = TRUE, allow_file = FALSE, ...)
}
\arguments{
	\item{x}{
		if \code{allow_file = FALSE} (default) it is a character vector with the file lines containing R code. It can also be a unique character element where lines are assumed from the newline character \code{\\n}. If \code{allow_file = TRUE} then it can also be a (unique) file path.
	}
	\item{chunk_prefix}{
		prefix character to identify chunks with R code. For example if \code{chunk_prefix = "#"} then chunks of R code can be placed only in comments (for an R script file, for instance). If \code{chunk_prefix = NULL} then the chunks of R code can be placed anywhere in the file (the default).
	}
	\item{chunk_char}{
		character to mark the chunks of R code. Default to \code{"@"}.
	}
	\item{chunk_times}{
		number of times that \code{chunk_char} must be repeated (default to 1).
	}
	\item{inline_open}{
		character that opens inline R code. Default to \code{<}.
	}
	\item{inline_char}{
		character in between inline R code is placed. Default to \code{@}.
	}
	\item{inline_close}{
		character that closes inline R code. Default to \code{>}.
	}
	\item{char_begin}{
		character to print at the beginning of line before output when adding lines at translating R code. Default is blank, but for comments (if \code{chunk_prefix = "#"} for instance) it should start with \code{#}.
	}
	\item{char_clean}{
		character to print to replace r code with empty or NULL output generating empty line for assignation R code. Default to \code{<:NULL:>}.
	}
	\item{char_drop}{
		character to print to indicate which lines should be dropped. Can be a regular expression. Default to \code{<:NULL:>}.
	}
	\item{envir}{
		environment where to evaluate R code.
	}
	\item{comments}{
		keep comments before evaluate R code? Default to TRUE.
	}
	\item{reduce}{
		logical. Delete consecutive empty lines? Default to TRUE.
	}
	\item{allow_file}{
		let x to be a file.path and/or vector of lines read using readLines. Default to FALSE.
	}
	\item{\ldots}{
		other arguments to pass to functions. At the moment only \code{debug} works to trigger debugging using \code{browser}. It can be a logical value to enable/disable debugging at all levels or a character string with the name of the function at whose level we want to trigger debugging.
	}

}
\details{
The input of \code{translate_r_code} is a file path, a character vector such as those obtained
using \code{readLines} or just a unique character element (where each line is assumed using the
newline character) with the content of a template file containing inline or chunks with R code.
Users can define an environment (including objects) where to evaluate this R code.
Once the template's R code is evaluated, its output is replaced in the template.
\code{translate_r_code} returns a character vector where each element is the corresponding line
of the file so that its content can be written to disk easily using \code{cat}.
Characters to identify inline and chunk R code can be defined by the user.
Assuming the default values for the input argument of \code{translate_r_code}, 
file lines starting with \code{@r} can contain R code in the whole line while 
code in between \code{<r@ code @>} evaluates R code only for the portion in between
the opening \code{<r@} and the closing \code{@>}. These characters to mark chunks
and inline openings and closings can be modified by the user.
The R code is evaluated by order of appearance (top to bottom, left to right)
and the behaviour of the output depends on the pressence of assignation \code{<-}.
Thus, to control the output, it is necessary to consider two main rules:
	\itemize{
		\item{Do not mix assignation (\code{<-}) with printing (assignation is only evaluated, not printed).}
		\item{Separate chunks using an empty line.}
	}
For more details see \href{../doc/tRnslate.html}{tRnslate package vignette} or run \code{vignette("tRnslate")}.
For an example see \link{tRnslate-package}.
}
\value{
Once the chunks or inline R code are evaluated by \code{translate_r_code}, it returns a character vector where each element corresponds to the original line in the template file where the chunks and inline code has been replaced by its output. This content can be seen in console or written to disc, for example, by using \code{cat} (it requires to use \code{sep = "\\n"}).
}
\author{
Mario Martinez Araya, \email{r@marioma.me}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~ 
\examples{
## To see an example in R console run:
##
## ?tRnslate::tRnslate
}


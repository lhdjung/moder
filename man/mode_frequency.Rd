% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequencies.R
\name{mode_frequency}
\alias{mode_frequency}
\title{Modal frequency}
\usage{
mode_frequency(x, na.rm = FALSE, max_unique = NULL)
}
\arguments{
\item{x}{A vector to check for its modal frequency.}

\item{na.rm}{Logical. Should missing values in \code{x} be removed before
computation proceeds? Default is \code{FALSE}.}

\item{max_unique}{Numeric or string. If the maximum number of unique values
in \code{x} is known, set \code{max_unique} to that number. This rules out that \code{NA}s
represent values beyond that number (see examples). Set it to \code{"known"}
instead if no values beyond those already known can occur. Default is
\code{NULL}, which assumes no maximum.}
}
\value{
Integer (length 1) or \code{NA}.
}
\description{
Call \code{mode_frequency()} to get the number of times that a
vector's mode appears in the vector.

See \code{\link[=mode_frequency_range]{mode_frequency_range()}} for bounds on an unknown frequency.
}
\details{
By default (\code{na.rm = FALSE}), the function returns \code{NA} if any
values are missing. That is because missings make the frequency uncertain
even if the mode is known: any missing value may or may not be the mode,
and hence count towards the modal frequency.
}
\examples{
# The mode, `9`, appears three times:
mode_frequency(c(7, 8, 8, 9, 9, 9))

# With missing values, the frequency
# is unknown, even if the mode isn't:
mode_frequency(c(1, 1, NA))

# You can ignore this problem and
# determine the frequency among known values
# (there should be good reasons for this!):
mode_frequency(c(1, 1, NA), na.rm = TRUE)
}
\seealso{
\code{\link[=mode_first]{mode_first()}}, which the function wraps.
}

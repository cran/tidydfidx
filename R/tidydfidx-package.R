#' tidydfidx package: tidyverse complements for dfidx
#' 
#' **dfidx** used to offer support for tibbles and **dplyr**'s
#' verbs. It was actually a mistake because the user couldn't use
#' **dfidx** and some packages like **mlogit** without installing many
#' dependencies. The **dfidx** package is now free of any
#' **tidyverse** dependencies and the **tidydfidx** package provides
#' the complement. It re-exports:
#'
#' - `dfidx`, `idx`, `idx_name` and `fold_idx` from **dfidx**, -
#' `as_tibble`, `%>%`, `filter arrange`, `slice`, `pull`, `mutate`,
#' `transmute` and `select` from **dplyr**
#'
#' For a gentle and comprehensive introduction to the package, please
#' see the package's vignette.
#' @name tidydfidx-package
#' @docType package
#' @keywords package


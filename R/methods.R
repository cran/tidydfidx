# Imports from dfidx

#' @importFrom dfidx dfidx
#' @export
dfidx::dfidx

#' @importFrom dfidx idx
#' @export
dfidx::idx

#' @importFrom dfidx idx_name
#' @export
dfidx::idx_name


#' @importFrom dfidx fold_idx
#' @export
dfidx::fold_idx

#' @importFrom dfidx unfold_idx
#' @export
dfidx::unfold_idx

#' Methods for dfidx
#'
#' A `dfidx` is a `data.frame` with a "sticky" data.frame column
#' which contains the indexes. Specific methods of functions that
#' extract lines and/or columns of a `data.frame` are provided.
#'
#' @name methods.dfidx
#' @param x a `dfidx` object
#' @param n the number of rows for the print method
#' @param prefix_named,suffix_shape see [vctrs::vec_ptype_abbr]
#' @param ... further arguments
#' @export
#' @author Yves Croissant
#' @return `as.data.frame` and `mean` return a `data.frame`, `[[` and
#'     `$` a vector, `[` either a `dfidx` or a vector, `$<-`
#'     and `[[<-` modify the values of an existing column or create a
#'     new column of a `dfidx` object, `print` is called for its side
#'     effect
#' @importFrom pillar new_pillar_shaft_simple tbl_sum pillar_shaft
#' @importFrom vctrs new_rcrd field vec_ptype_abbr
#' @importFrom dplyr bind_cols
#' @importFrom Rdpack reprompt
#' @examples
#' data("munnell", package = "dfidx")
#' mn <- dfidx(munnell)
#' # extract a series (returns as a xseries object)
#' mn$gsp
#' # or
#' mn[["gsp"]]
#' # extract a subset of series (returns as a dfidx object)
#' mn[c("gsp", "unemp")]
#' # extract a subset of rows and columns
#' mn[mn$unemp > 10, c("utilities", "water")]
#' # dfidx, idx and xseries have print methods as (like tibbles), a n
#' # argument
#' print(mn, n = 3)
#' print(idx(mn), n = 3)
#' print(mn$gsp, n = 3)
#' # a dfidx object can be coerced to a data.frame
#' head(as.data.frame(mn))
print.tbl_dfidx <- function(x, ..., n = NULL){
    .nms_idx <- names(idx_name(x))
    .pos_idx <- as.numeric(idx_name(x))
    class(x) <- c("tbl_dfidx2", "tbl_df", "tbl", "data.frame")
    tbl2vctr <- function(x) vctrs::new_rcrd(unclass(x), class = "vecidx")
#        x$idx <- tbl2vctr(x$idx)
#        pos_idx <- match("idx", names(x))
#        x <- bind_cols(x[pos_idx], x[- pos_idx])
    x[[.nms_idx]] <- tbl2vctr(x[[.nms_idx]])
#        x <- bind_cols(x[.pos_idx], x[- .pos_idx])
    print(x, ..., n = n)  
}

#' @rdname methods.dfidx
#' @method vec_ptype_abbr vecidx
#' @export
vec_ptype_abbr.vecidx <- function(x, ..., prefix_named, suffix_shape) "idx"

#' @rdname methods.dfidx
#' @method format vecidx
#' @export
format.vecidx <- function(x, ...){
    .cls <- attr(x, "ids")
    ids <- match(c(1, 2), .cls)
    id1 <- field(x, ids[1])
    id2 <- field(x, ids[2])
    paste(id1, id2, sep = ":")
}

#' @rdname methods.dfidx
#' @method pillar_shaft vecidx
#' @export
pillar_shaft.vecidx <- function(x, ...){
    out <- format(x)
    pillar::new_pillar_shaft_simple(out, min_width = 8, shorten = "mid")
}

index_structure <- function(x){
    nms <- attr(x, "names")
    ids <- attr(x, "ids")
    idp <- match(c(1, 2), ids)
    names(ids) <- nms
    cards <- sapply(nms, function(f) length(unique(field(x, f))))
    cards <- cards[idp]
    .indexes <- paste(cards[1], " (", names(cards)[1], ") x ", cards[2], " (", names(cards)[2], ") ", sep = "")
    .balanced <- ifelse(prod(cards) == length(x), "yes", "no")
    result <- c(Index = .indexes, Balanced = .balanced)
    if (length(idp) != length(ids)){
        nesting <- ids[- idp]
        nested <- names(cards[nesting])
        nesting <- names(nesting)
        nesting_structure <- paste(sapply(1:length(nested),
                                          function(i) paste(nested[i], " (", nesting[i], ")", sep = "")),
                                   collapse = ", ")
        .nesting <- nesting_structure
        result <- c(result, Nesting = .nesting)
    }
    result
}


#' @rdname methods.dfidx
#' @method tbl_sum tbl_dfidx2
#' @export
tbl_sum.tbl_dfidx2 <- function(x, ...) {
    default_header <- NextMethod()
    .idx <- names(which(sapply(x, function(aserie) inherits(aserie, "vecidx"))))
    c(default_header, index_structure(x[[.idx]]))
}




dindex_structure <- function(x){
    nms <- attr(x, "names")
    ids <- attr(x, "ids")
    idp <- match(c(1, 2), ids)
    names(ids) <- nms
    cards <- sapply(nms, function(f) length(unique(field(x, f))))
    cards <- cards[idp]
    .indexes <- paste(cards[1], " (", names(cards)[1], ") x ", cards[2], " (", names(cards)[2], ") ", sep = "")
    .balanced <- ifelse(prod(cards) == length(x), "yes", "no")
    result <- c(Index = .indexes, Balanced = .balanced)
    if (length(idp) != length(ids)){
        nesting <- ids[- idp]
        nested <- names(cards[nesting])
        nesting <- names(nesting)
        nesting_structure <- paste(sapply(1:length(nested),
                                          function(i) paste(nested[i], " (", nesting[i], ")", sep = "")),
                                   collapse = ", ")
        .nesting <- nesting_structure
        result <- c(result, Nesting = .nesting)
    }
    result
}

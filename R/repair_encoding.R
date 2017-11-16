#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param from PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealson
#'  \code{\link[stringi]{stri_conv}}
#' @rdname repair_encoding
#' @importFrom stringi stri_conv


repair_encoding=function (x, from = NULL)  {

    if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("stringi package required for encoding operations")
    }
    if (is.null(from)) {

        best_guess <- guess_encoding(x)[1, , drop = FALSE]
        from <- best_guess$encoding
        conf <- best_guess$confidence * 100
        if (conf < 50) {
            stop()
        }
        #message("Best guess: ", from, " (", conf, "% confident)")
    }
    stringi::stri_conv(x, from = from)



}

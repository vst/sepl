##' Wraps an sepl "error" result.
##'
##' @param msg The error message.
##' @param call The call expression which yields the error.
##' @param trace Indicates if the traceback should be included in the error result.
##' @return A list representing the "success" result.
.seplerror <- function (msg, call, trace=FALSE) {
    list(message=jsonlite::unbox(msg),
         call=jsonlite::unbox(as.character(as.expression(call))),
         trace=traceback(ifelse(trace, 1, NA)),
         status=jsonlite::unbox("error"))
}

##' Wraps an sepl "success" result.
##'
##' @param result The result data.
##' @return A list representing the "success" result.
.seplsuccess <- function (result) {
    list(result=result, status=jsonlite::unbox("success"))
}

##' Evaluates the input under a try-catch statement.
##'
##' @param input The input to be evaluated.
##' @param isfile Indicates if the input is a file or raw character string.
##' @return The result of the evaluation.
.sepleval <- function (input, isfile) {
    ## Define the error catching/handling function:
    error <- function (error) {
        structure(error, class="sepl-error")
    }

    ## Source or eval according to `isfile` flag:
    if (isfile) {
        tryCatch(source(input)$value, error=error)
    }
    else {
        tryCatch(eval(parse(text=input)), error=error)
    }
}

##' Evaluates the input and returns a sepl result.
##'
##' The result can be either a "success" result or an "error"
##' result. In both cases, the return value indicates what it is and
##' provides useful(ish) information in case of "error".
##'
##' @param input The input to be evaluated.
##' @param isfile Indicates if the input is a file or raw character string.
##' @param trace Indicates if the traceback should be included in the
##'     error result, if any.
##' @return The sepl result.
##' @import jsonlite
##' @export
sepl <- function (input, isfile=FALSE, trace=FALSE) {
    ## Eval/Source the input under try-catch:
    retval <- .sepleval(input, isfile)

    ## Success?
    if (class(retval) == "sepl-error") {
        .seplerror(retval$message, retval$call, trace)
    }
    else {
        .seplsuccess(retval)
    }
}

##' Renders the markdown file with the params provided as PARAMS
##' global.
##'
##' @param file The markdown file provided.
##' @param params Parameters to be injected into PARAMS global.
##' @param ... Additional arguments to rmarkdown::render
##' @return rmarkdown::render result.
##' @import rmarkdown
##' @export
render <- function (file, params=list(), ...) {
    ## Create a new environment:
    myEnv <- new.env()

    ## Assign the params in the environment:
    assign("PARAMS", params, envir=myEnv)

    ## Render the document:
    rmarkdown::render(file, envir=myEnv, ...)
}

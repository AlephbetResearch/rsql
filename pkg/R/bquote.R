#' Just like regular bquote, but unlists results
#'
#' Very similar to the base:bquote, but when the expression evaluates to a list,
#' the results are inserted into the quoted expression.
#'
#' @param expr the quoted expression
#' @param where the environment in which to evaluate any expressions
#'
#' @export
#' @examples
#' a= list(quote(b),quote(c))
#' bquote(quote(f(.(a)))) #f(list(b, c))
#' library(rsql)
#' sql_bquote(quote(f(.(a)))) #f(b, c)
sql_bquote <- function (expr, where = parent.frame()) 
{
  unquote <- function(e) {
    if (length(e) <= 1L) {
      e
    } else if (e[[1L]] == as.name(".")) {
      if (length(e)<2) {
        stop("Invalid quote format: ",e)
      }
      e[[1L]] <- quote(rsql:::dot)
      e.eval <- eval(e,where)
#     lapply(e.eval,as.name)
      unquote(e.eval)
#      e.eval
    }else if (is.pairlist(e)) {
      as.pairlist(lapply(e, unquote))
    }else {
      if (is.call(e)) {
        as.call(unlist(lapply(e, unquote)))
      }else {
        unlist(lapply(e, unquote))
      }
    }
  }
  unquote(substitute(expr))
}

dot <- function(expr,deparse=FALSE){
  expr <- as.list(match.call())$expr
  expr = eval(expr,parent.frame())
  if (deparse){
    deparse(expr)
  }else {
    expr
  }
}



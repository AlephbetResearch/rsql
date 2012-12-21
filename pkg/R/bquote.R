# just like regular bquote, but unlists results
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



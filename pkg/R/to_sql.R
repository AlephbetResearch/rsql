##' Format an rsql object to SQL
##'
##' @rdname to_sql
##' @export
#to_sql <- function(x){
#  UseMethod("to_sql")
#}
#
##' @rdname to_sql
##' @export
##setGeneric('to_sql',function(x)standardGeneric("to_sql"))
#
##' @rdname to_sql
##' @S3method to_sql default
##' @export
#to_sql.default <- function(x){
#  x
#}
#
#' @rdname to_sql
#' @S3method to_sql list
#' @export
to_sql.list <- function(x) {
  lapply(x,to_sql)
}

#' @rdname to_sql
#' @S3method to_sql expression
#' @export
to_sql.expression <- function(x){
  sapply(as.list(x),to_sql)
}

#' @rdname to_sql
#' @S3method to_sql quoted
#' @export
to_sql.quoted <- function(x){
  sapply(x,to_sql)
}

#' @rdname to_sql
#' @S3method to_sql name
#' @export
to_sql.name <- function(x) {
  deparse(x)
}

#' @rdname to_sql
#' @export
to_sql.function <- function(x) {
  x()
}

#' @rdname to_sql
#' @export
to_sql.NULL <- function(x) {
  NULL
}

#' @rdname to_sql
#' @export
setMethod("to_sql","NULL",to_sql.NULL)

#' @rdname to_sql
#' @S3method to_sql call
#' @export
to_sql.call <- function(x) {
  x.vars = all.vars(x,functions=FALSE)
  x.names = all.vars(x,functions=TRUE)
  x.calls <- x.names[! x.names %in% x.vars]
  x.calls.exists = sapply(x.calls,exists,
      envir=rsql_to_sql_env,inherits=FALSE)
  for (x.call in x.calls[!x.calls.exists]) {
    x.FUN <- function_to_sql(x.call)
    rsql_to_sql_env[[x.call]] <- x.FUN
  }
  get_values <- function(expr){
   if (!is.call(expr)){
    if (is.numeric(expr)) {
        deparse(expr)
      }else if (is.character(expr)) {
        deparse(expr)
      }else {
        expr
      }   
    }else {
      as.call(lapply(expr,get_values))
    }
  }
  x = get_values(x)
  x.vars = as.list(x.vars)
  names(x.vars) = x.vars
  expr = do.call(substitute,list(x,x.vars))
  eval(expr,envir=rsql_to_sql_env)
}

#' @S3method to_sql (
#' @rdname to_sql
#' @export
`to_sql.(` <- function(x)
    to_sql.call(x)

#' @S3method to_sql (
#' @rdname to_sql
#' @export
to_sql.if <- function(x)
  to_sql.call(x)


#' @export
setGeneric (
    name = "coltypes",
    def=function(data){standardGeneric("coltypes")})

#' @export
setGeneric (
    name = "from",
    def=function(data,...){standardGeneric("from")})

#' @rdname to_sql
#' @export
setGeneric('to_sql',function(x)standardGeneric("to_sql"))

#' Format an rsql object to SQL
#'
#' @rdname to_sql
#' @export
to_sql <- function(x){
  UseMethod("to_sql")
}

#' @rdname to_sql
#' @S3method to_sql default
#' @export
to_sql.default <- function(x){
  x
}
#

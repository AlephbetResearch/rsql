#' Create a SQL expression from an R expression
#' An R expression is passed as an argument where it is parsed 
#' and stored unevaluated.  This stored expression can be passed 
#' to 'to_sql'
#' 
#' Certain syntactic structures of SQL are not valid expressions in R
#' and must be modified in order to use rsql.  In particular, R does
#' not support statements like 'DISTINCT X'.  Such expressions can be 
#' represented using functions in rsql, ie distinct(X).  In addition, 
#' certain binary operators in SQL cannot be used in R expressions.  
#' For example, 'A AND B' must instead be represented as 'A && B'
#' of 'and(A,B)'
#' 
#' A full list of currently allowed expressions can be found below in the 
#' examples.  However, rsql also supports arbitrary custom functions, 
#' interpreting them as valid functions in SQL.
#'
#' @export
#' @rdname rsql
#' @examples
#' to_sql(rsql(count(distinct(x))))
#' to_sql(rsql(count(distinct(x))))
#' to_sql(rsql(array_max(x_array) > 5))
#' to_sql(rsql(x > 5 && y < 5))
#' 
#' ## rsql functions
#' rsql:::SQL_FUNCTION_LIST
#' ## rsql prefix operators
#' rsql:::SQL_PREFIX_LIST
#' ## rsql postfix operators
#' rsql:::SQL_POSTFIX_LIST
#' ## rsql binary operators
#' rsql:::SQL_BINARY_LIST
rsql <- function(X,quote=TRUE,parse=FALSE){
  rsql.call = match.call()
  rsql.call[[1]] <- quote(rsql_expression)
  eval(rsql.call)
}

#' @export
rsql_expression <- function(X,quote=TRUE,parse=FALSE){
  if (quote) {
    X <- as.list(match.call())$X
  }

  if(parse) {
   X <- as.list(parse(text=X))
  }

  if (is(X,"rsql_expression")) {
    X
  }else {
    if (is.list(X)) {
      lapply(X,rsql_expression,quote=FALSE,parse=FALSE)
    }else {
#new(Class="rsql_expression",value=X)
      eval(substitute(sql_bquote(X,parent.frame()),list(X=X)))
    }
  }
}




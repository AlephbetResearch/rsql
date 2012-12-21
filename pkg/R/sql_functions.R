
rsql_to_sql_env <- new.env()

#' Format SQL SELECT statement to select the arguments passed
#'
#' Converts arguments to SQL using \code{to_sql} before formatting
#' them using \code{names} as aliases
#'
#' @param ... named list of expressions to select
#' 
#' @import stringr
#' @export
sql_select = function(...){
  #unlist to fix formatting for embedded lists of one element
  x=unlist(list(...))
  #get names from rsql expressions
  xnames = rsql_colnames(x)
  
  has_explicit_alias = sapply(x,
      function(xarg){
        if (length(xarg)>1 && xarg[[1]]==as.name('as')){
          TRUE
        }else {
          FALSE
        }})
  paste0("SELECT ",
      paste0(to_sql(x),
        ifelse(xnames == "" | has_explicit_alias,
          ""
          ,
          paste0(" AS ",xnames)),
        collapse=",\n\t"))
}
 

#' Format SQL JOIN statement to select the arguments passed
#'
#' Converts arguments to SQL using \code{to_sql} before formatting
#' them.
#'
#' @param x table reference to join
#' @param condition condition on which to join
#' @param method type of join to perform, e.g. \code{"LEFT OUTER"}
#'
#' @export
sql_join = function(x,on,
    method=NULL){
  paste0(if (is.null(method)) {
          ""
        }else {
# Re parse method here because string literals get deparsed
# on call to to_sql
          paste0(parse(text=method)[[1]]," ")
        },
        "JOIN ",to_sql(x),
        " ON ", to_sql(on))
}

#' Format SQL UNION statement to select the arguments passed
#'
#' Converts arguments to SQL using \code{to_sql} before formatting
#' them.
#'
#' @param e1 table reference to union
#' @param e2 table reference to union
#' @param all logical indication whether to union all
#'
#' @export
sql_union = function(e1,e2,...,all=TRUE){
  union_sql = paste0('\nUNION ',if (all){'ALL '},'\n')
  paste0(lapply(list(e1,e2,...),to_sql),collapse=union_sql)
}

sql_cast <- function(elem,type) {
  paste0("cast(",to_sql(elem)," AS ",to_sql(type))
}

`sql_[` <- function(x,i){
  paste0(to_sql(x),'[',to_sql(i),']')
}

sql_alias <- function(expr,...){
  paste0(to_sql(expr)," AS ",args_to_sql(...))
}

sql_switch <- function(...,as.logical=FALSE) {
  args = list(...)
  if (as.logical) {
    cases = args[[1]]
    values = args[[2]]
    default = args[[3]]
    paste0("CASE \n\t",
        paste("WHEN",to_sql(cases),
          "THEN",to_sql(values),sep=" ",collapse="\n\t"),
          "\n\tELSE ",to_sql(default),"\n\tEND")
  }else {
    expr = args[[1]]
    cases = args[[2]]
    values = args[[3]]
    default = args[[4]]
    paste0("CASE ",to_sql(expr),"\n\t",
        paste("WHEN",to_sql(cases),
          "THEN",to_sql(values),sep=" ",collapse="\n\t"),
          "\n\tELSE ",to_sql(default),"\n\tEND")
  }
}



args_to_sql <- function(...){
  paste(
      #unlist the list for embedded lists not printing as 'list()'
      sapply(to_sql(unlist(list(...))),
        paste,collapse=", "),
      collapse=", ")
}

#' Format a unary operator with name \code{x}
#'
#' Function created converts arguments \code{...} to SQL using
#' \code{to_sql} before formatting them either before or after
#' name \code{x}, depending on \code{prefix}.
#'
#' @param x operator name
#'
#' @export
unary_operator_to_sql <- function(x,prefix=TRUE) {
  sqlname = x
  if (prefix) {
    function(e1,...) {
      paste(sqlname,args_to_sql(e1,...))
    }
  }else {
    function(e1) {
      if (length(e1)!=1) {
        stop("Postfix unary SQL operators take 1 argument of length 1")
      }
      paste(to_sql(e1),sqlname)
    }
  }
}

#' Format a binary operator with name \code{x}
#'
#' Function created converts \code{e1} and \code{e2} to SQL using
#' \code{to_sql} before formatting them on either side of operator
#' name \code{x}.
#'
#' @param x operator name
#'
#' @export
binary_operator_to_sql <- function(x) {
  sqlname = x
  function(e1,e2) {
    paste(to_sql(e1),sqlname,to_sql(e2))
  }
}

#' Format a regular function with name \code{x}
#'
#' Function created converts arguments \code{...} to SQL using
#' \code{to_sql} before wrapping them in parenthesis and 
#' prepending function name \code{x}.
#'
#' @param x function name
#'
#' @export
function_to_sql <- function(x) {
  sqlname = x
  function(...) {
    str_c(sqlname,"(",args_to_sql(...),")")
  }
}

# regular functions
# Goofy thing here is including '('
# Turns out that logical groupings for order of operation
# are parsed as calls, so A && ( B || C || D ) creates calls
# where '(' is the function.  If this is processed normally,
# you get (( B || C || D ), because an extra '(' is printed 
# out as the name of the function.
SQL_FUNCTION_LIST <- c(count="COUNT",sum="SUM","("="")
# unary operators that are 'pre-fix'
SQL_PREFIX_LIST <- c(distinct="DISTINCT",not="NOT","!"="NOT",
  'from'='FROM',group_by="GROUP BY",having="HAVING",where='WHERE',
  order_by='ORDER BY',limit="LIMIT",lateral_view="LATERAL VIEW")
# unary operators that are 'post-fix'
SQL_POSTFIX_LIST <- c(is.null="IS NULL",asc="ASC",desc="DESC")
# binary operators that are 'in-fix'
SQL_BINARY_LIST <- c("&&"="AND","||"="OR",
    ">"=">","<"="<",">="=">=","<="="<=","=="="=",
    "-"="-","+"="+","/"="/","*"="*","%%"="%",
    'and'='AND','or'='OR','as'='AS')

#' (Re)Assign a function to construct a SQL function/operator
#' 
#' Functions and operators common to multiple implementations
#' should already be defined.  Users should only need to 
#' assign new functions when they wish to use non-standard syntax
#' for their DBMS.
#' 
#' @param ... named list of functions to assign 
#'
#' @export
rsql_assign <- function(...){
  args <- list(...)
  for (varname in names(args)) {
    assign(varname,args[[varname]],envir=rsql_to_sql_env)
  }
}

#' (Re)Set the rsql environment
#'
#' Users should not have to use this unless they are using 
#' \code{rsql_assign} and break things.
#'
#' @export
rsql_init <- function() {

# create regular functions
  for (name in names(SQL_FUNCTION_LIST)) {
    val = SQL_FUNCTION_LIST[[name]]
    rsql_to_sql_env[[name]] <- function_to_sql(val)
  }
# create pre-fix operators
  for (name in names(SQL_PREFIX_LIST)) {
    val = SQL_PREFIX_LIST[[name]]
    rsql_to_sql_env[[name]] <- unary_operator_to_sql(val,prefix=TRUE)
  }
# create post-fix operators
  for (name in names(SQL_POSTFIX_LIST)) {
    val = SQL_POSTFIX_LIST[[name]]
    rsql_to_sql_env[[name]] <- unary_operator_to_sql(val,prefix=FALSE)
  }
# create binary operators
  for (name in names(SQL_BINARY_LIST)) {
    val = SQL_BINARY_LIST[[name]]
    rsql_to_sql_env[[name]] <- binary_operator_to_sql(val)
  }

  rsql_assign(select=sql_select,
      join=sql_join,union=sql_union,cast=sql_cast,as=sql_alias,
      `[`=`sql_[`,switch=sql_switch)
}


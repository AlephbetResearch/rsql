setClass(
    Class="rsql_selectable",
    representation=representation(
      colrefs="list"),
    prototype=prototype(
      colrefs=list())
    )

#' Class containing an R representation of a compound SQL select statement
#'
#' @autoSlots
#' @export
setClass(
    Class="rsql_compound_select_statement",
    contains="rsql_selectable",
    representation=representation(
      rsql="list"),
    prototype=prototype(
      rsql=list()),
    )

#' Explicitly create a \code{rsql_compound_select_statement} object
#'
#' This should not need to be called by users. 
#'
#' @export
rsql_compound_statement <- function(colrefs,rsql) {
  new(Class="rsql_compound_select_statement",
      colrefs=colrefs,
      rsql=rsql)
}

#' @export
setMethod("to_sql",
    "rsql_compound_select_statement",
    function(x) {
      paste(to_sql(x@rsql),collapse="\n")
    })

#' @export
setMethod("show",
    "rsql_compound_select_statement",
    function(object) {
      cat("Columns:\n")
      show(object@colrefs)
      cat("RSQL:\n")
      show(object@rsql)
      })

#' @export
setMethod("print",
    "rsql_compound_select_statement",
    function(x,...)
      print(x$rsql,...)
   )

#' Class containing an R representation of a SQL statement
#'
#' @autoSlots
#' @export
setClass(
    Class="rsql_select_statement",
    contains="rsql_selectable",
    representation=representation(
      select="list",
      from="list",
      optional="list"),
    prototype=prototype(
      select=list(),
      colrefs=list(),
      from=list(),
      optional=list()),
    )

# This is the order for SQL commands in a SELECT statement
# This is used when adding commands to a SELECT statement
sql_order = c("where","group_by","having","order_by","limit")

#' Explicitly create a \code{rsql_select_statement} object
#'
#' This should not need to be called by users. Users should
#' instead use rsql_select.
#'
#' @export
rsql_select_statement <- function(select=list(),from=list(),
    optional=list()) {
  opts <- optional
  if (!is(select,"list")){
    select = list(select)
  }
  if (!is(from,"list")) {
    from = list(from)
  }
  optnames <- sapply(opts,function(x)deparse(x[[1]]))
  if (length(optnames) != length(unique(optnames))) {
    optcounts <- summary(as.factor(optnames))
    stop("Duplicated Query elements not allowed: ",
        names(optcounts)[optcounts>1])
  }
  opts <- opts[order(pmatch(optnames,sql_order))]
  opts <- opts[!is.null(opts)]
  
  cnames = rsql_colnames(select)
  colrefs <- rsql_colrefs(cnames)

  x <- new(Class="rsql_select_statement",
      colrefs=colrefs,
      select=select,
      from=from,
      optional=opts)
  x
}

#' Generic method for to_sql
#' @export
to_sql.rsql_select_statement <- function(x) {
      paste(unlist(c(
        to_sql(as.call(c(quote(select),list(x@select)))),
        lapply(x@from,to_sql),
        lapply(x@optional,to_sql))),
          collapse="\n")
      }

#' Generic method for to_sql
#' @export
setMethod("to_sql",
    "rsql_select_statement",
    to_sql.rsql_select_statement
   )

sql_from_clauses = c("from","join")

#' Function for merging SELECT statements with additional elements
#'
#' In most cases, this should not be used directly by users.
#' @export
`+.rsql_select_statement` <- function(e1,e2) {
  if (is(e2,"rsql_select_statement")) {
  rsql_select_statement(
      select=c(e1@select,e2@select),
      from=c(e1@from,e2@from),
      optional=c(e1@optional,e2@optional))
  }else {
    if (is(e2,"rsql_expression")) {
      e2 <- e2@value
    }else if (is.quoted(e2)) {
      e2 = unclass(e2)
    }
    if (is(e2,"language")) {
      if (deparse(e2[[1]]) %in% sql_from_clauses) {

        rsql_select_statement(
            select=e1@select,
            from=c(e1@from,e2),
            optional=e1@optional)
      }else { 
        rsql_select_statement(
            select=e1@select,
            from=e1@from,
            optional=c(e1@optional,e2))
      }
    }else {
      stop("Cannot add argument to rsql_select_statement: ",e2)
    }
  }
}

#' @export
setMethod("+",
    "rsql_select_statement",
    `+.rsql_select_statement`
   )

#' Generic method for show
#' @export
setMethod("show",
    "rsql_select_statement",
    function(object) {
      cat("COLUMNS:\n")
      show(object@colrefs)
      cat("SELECT:\n")
      show(object@select)
      cat("FROM:\n")
      show(object@from)
      cat("OPTIONAL:\n")
      show(object@optional)
      })

#' Generic method for print
#' @export
setMethod("print",
    "rsql_select_statement",
    function(x,...)
      print(as.character(x,...))
   )

#' Overloading the $ operator for access to column references
#' @export
setMethod('$',
    "rsql_select_statement",
    function(x,name){
      if (name %in% colnames(x)) {
        x@colrefs[[name]]
      }else if (exists(name,envir=rsql_function_env,inherits=FALSE)){
        fun = get(name,envir=rsql_function_env)
        function(...){
          args = rsql_unquote(list(...))
          x + do.call(fun,args)
        }
      }else if (exists(name,envir=rsql_to_sql_env,inherits=FALSE)) {
        function(...,quote=FALSE){
          expr <- eval(substitute(.(...)))
          if (!quote) {
            expr = list(...)
          }
          expr <- rsql_unquote(expr)
          expr <- as.call(c(list(as.name(name)),expr))
          x + rsql_expression(expr,quote=FALSE)
        }
      }else {
        stop("Unknown command: ",name,
          "\nRegister SQL functions with rsql_assign(name=function())")
      }
    }
  )


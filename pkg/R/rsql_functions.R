##### rsql functions
rsql_function_env <- new.env()

#' Construct a SQL query
#' 
#' Constructs a basic SQL query from arguments
#' 
#' @param data object to be selected
#' @param ... additional arguments to be selected
#'
#' If \code{data} is a \code{\link{rsql_table}} then it and \code{...}
#' are evaluated as \code{from(data,list(...))}.  In addition to adding the 
#' resulting elements to the list of expressions to be selected,
#' \code{from(data)} is added to the statement as a quoted 
#' \code{\link{rsql}} expression.
#'
#' Otherwise \code{data} and any subsequent arguments are treated as
#' quoted R expressions as created by \code{\link{rsql}} and added
#' to the list of things to be selected.  In this case, no from clause
#' is added to the statement.
#'
#' In addition to being called directly, \code{rsql_select} is used
#' to add select expressions using \code{$select} on \code{rsql_table}
#' and \code{rsql_select_statement} objects.  See examples below.
#'
#' @examples
#' library(rsql)
#' tab = rsql_table("tab",c('x','y'))
#' to_sql(rsql_select(tab))
#' to_sql(rsql_select(tab,x))
#' to_sql(rsql_select(from(tab,x)))
#' to_sql(tab$select())
#' to_sql(tab$select(x))
#'
#' @export
rsql_select <- function(data,expr){
  args = if (missing(expr)) {
    list()
  }else {
    expr
  }
  colrefs <- list()
  rsql.from <- list()
  if (!missing(data) && is(data,"rsql_table")) {
    colrefs <- if (length(args)>0) {
      from(data,args)
    }else {
      from(data)
    }
    if (!is.list(colrefs)) {
      colrefs = list(colrefs)
    }
    rsql.from = list(substitute(from(obj),
          list(obj=data)))
  }else {
    colrefs <- rsql_unquote(list(data,expr))
  }
  rsql_select_statement(select=colrefs,from=rsql.from)
}

assign('select',rsql_select,envir=rsql_function_env)

#' Create a SQL union of two select statements
#'
#' @param e1 first statement
#' @param e2 second statement
#' @param ... additional statements
#' @param all logical value, whether or not to UNION ALL
#'
#' @examples
#' library(rsql)
#' sel1 = tab.select(.(x))
#' sel2 = tab.select(.(y))
#' to_sql(rsql_union(sel1,sel2))
#'
#' @export
rsql_union <- function(e1,e2,...,all=TRUE) {
  rsql.from = as.call(eval(substitute(list(quote(union),e1,e2,...,all=all))))
  all_cols = lapply(list(e1,e2,...),colnames)
  cols = all_cols[[min(which(!sapply(all_cols,is.null)))]]
  rsql_compound_statement(colrefs=rsql_colrefs(cols),
      rsql=list(rsql.from))
}

assign('union',rsql_union,envir=rsql_function_env)

rsql_unquote <- function(expr) {
  if (is(expr,'list')) {
    lapply(expr,rsql_unquote)
  } else if (!is.quoted(expr)) {
    expr
  }else {
    envir = attr(expr,'env')
    # names should be preserved
    argnames = names(unclass(expr))
    args = lapply(expr,function(arg)
        do.call(sql_bquote,list(arg,envir)))
    names(args) = argnames
    unlist(args)
  }
}

#' Evaluate any bquoted expressions and substitute actual column references.
rsql_substitute <- function(args,colrefs=list()) {
  if (is(args,"list")) {
    lapply(args,rsql_substitute,colrefs=colrefs)
  }else {
     envir = parent.frame()
     if (is.quoted(args)) {
       args = unlist(rsql_unquote(args))
     }

     do_sub = function(arg){
       do.call(substitute,list(arg,env=c(colrefs,list(`*`=colrefs))))
     }
     args <- if (is(args,"list")){
        #if (length(args)>1) {  # don't add extra list wrappers
          unlist(lapply(args,do_sub))  # but still allow .() to produce lists
        #}else {
        #    do_sub(args[[1]])
        #}
     }else {
       do_sub(args)
     }
     args
  }
}

#' Generate the column names from a list of quoted rsql expressions
#' @param x a list a quoted rsql expressions
#' @export
rsql_colnames <- function(x) {
      if (is.quoted(x)) {
        x = unclass(x)
      }
      # check for explicit names in the list
      xnames = names(x)
      if (is.null(xnames)) {
        xnames <- rep("",length(x))
      }
      # check for SQL alias using as(...)
      explicit_alias = sapply(x,
        function(xarg){
          if (length(xarg)>1 && xarg[[1]]==as.name('as')){
            xarg[[3]]
          }else {
            NA
          }})
      has_alias = !is.na(explicit_alias)
      xnames[has_alias] = explicit_alias[has_alias]
      # if it's a name, just use the name if there's
      # no alias provided
      use_colname <- nchar(xnames)==0 & sapply(x,is.name)
      xnames[use_colname] = sub('^[^.]+\\.',"",
        sapply(x[use_colname],deparse))
      xnames
}

#' Create quoted column references from column names and a table name or alias
#'
#' @param colnames column names
#' @param alias table name or alias
#'
#' @examples
#'
#' library(rsql)
#' rsql_colrefs(c("x","y"),"tab")
#'
#' @export
rsql_colrefs <- function(colnames,alias=NULL) {
  if (!is.null(alias)) {
    alias <- paste0(alias,".")
  }
  if (any(nchar(colnames)==0)) {
    colnames = "*"
  }
# Add in the surrounding '`' to allow special characters, specifically *
  colrefs = rsql_expression(paste0('`',alias,colnames,'`'),
      quote=F,parse=T)
  names(colrefs) = colnames
  colrefs
}


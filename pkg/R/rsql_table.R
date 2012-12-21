#' Class containing an R representation of a SQL table
#' @autoSlots
#' @export
setClass(
    Class="rsql_table",
    contains="rsql_selectable",
    representation=representation(
      name="character"),
    )

#' Create an rsql table reference.
#'
#' @param X table name
#' @param colnames vector of column names
#' @export
rsql_table <- function(X,colnames=NULL) {
  table_name = if (is.character(X)) {
    X
  }else {
    as.character(X)
  }
  if (length(table_name)!=1) {
    stop("Table name must be length 1")
  }
  cols=list()
  if (!is.null(colnames)) {
    colrefs <- rsql_colrefs(colnames,alias=table_name)
  }
  new(Class="rsql_table",name=table_name,colrefs=colrefs)
}

setMethod('dim','rsql_selectable',
    function(x) {
      c(NA,length(x@colrefs))
      })

setMethod('dimnames','rsql_selectable',
    function(x) {
     list(NULL,names(x@colrefs))
      })

#' @export
from.rsql_selectable <- function(data,expr=TRUE,prefix=NULL,suffix=NULL,envir=parent.frame()) {
  colrefs = data@colrefs
  if (!missing(expr) && is.null(expr)) {
    list()
  }else if (missing(expr)) {
    names(colrefs) <- paste0(prefix,colnames(data),suffix)
    colrefs
  }else if (length(expr)==0) {
    list()
  }else if (is.logical(expr)) {
    names(colrefs) <- paste0(prefix,colnames(data),suffix)
    if (length(expr)>1 && length(expr)!=length(colnames(data))) {
      stop("Logical argument to rsql_select has length: ",
          length(expr),
          "\n\tbut table has ",length(colnames),"columns")
    }
    colrefs[expr]
  }else {
    expr.eval <- rsql_substitute(expr,colrefs=colrefs)
    expr.eval
  }
}

#' @export
setMethod("with","rsql_selectable",from.rsql_selectable)

#' @export
setMethod("from",list(data="rsql_selectable"),from.rsql_selectable)

#' @export
from. <- function(data,...){
  args = as.list(match.call())[-1]
  args$data = NULL
  from(data,do.call(.,args))
}

#' @export
setMethod('to_sql',
    "rsql_table",
    function(x){
        x@name
    }
  )

setMethod('as.character',
    "rsql_table",
    function(x){
        x@name
    }
  )


#' @export
setMethod('$',
    "rsql_table",
    function(x,name) {
      if (name %in% colnames(x)) {
        x@colrefs[[name]]
      }else {
        fun <- get(name,envir=rsql_function_env)
        function(...){
          args = rsql_unquote(list(...))
          eval(as.call(c(list(fun,data=x),args)))
        }
      }
    }
)

#' @export
setClass(
    Class="rsql_expression",
    representation=representation(
      value="language")
    )

#' Class containing a R reference to a SQL table reference with an alias
#'
#' @autoSlots
#' @export
setClass(
    Class="rsql_alias",
    representation=representation(
      alias="character",
      reference="ANY"),
    contains="rsql_table"
    )

#' Create a \code{rsql_alias} to \code{object}
#'
#' @param object what to reference
#' @param alias name to use as alias
#'
#' @export
rsql_alias <- function(object,alias) {
  cnames = colnames(object)

  colrefs <- rsql_colrefs(cnames,alias=alias)

  name = if ("name" %in% slotNames(object)) {
    object@name
  }else {
    as.character(NA)
  }
  new(Class="rsql_alias",
      colrefs=colrefs,
      reference=object,
      alias=alias,
      name=name)
}

#' @export
setMethod('to_sql','rsql_alias',
    function(x) {
      ref = x@reference
      refstring = if (is(ref,"rsql_alias")) {
        to_string(ref@reference)
      }else if (is(ref,"rsql_select_statement") || 
        is(ref,"rsql_compound_select_statement")) {
        paste0("( ",to_sql(ref)," )")
      }else if (is(ref,"rsql_table")) {
        to_sql(ref)
      }
      paste0(refstring,' ',x@alias)
    })


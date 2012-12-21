#' Class containing an R representation of a SQL table
#' @autoSlots
#' @export
setClass(
    Class="rsql_selectable",
    representation=representation(
      colrefs="list"),
    prototype=prototype(
      colrefs=list())
    )



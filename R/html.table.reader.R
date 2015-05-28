#' Read an Html Table from a url in an html.table file.
#'
#' This function will head an HTMl file from the url,
#' parse the file for tables, and extract all tables
#' into dataframes that are names using the table id
#'
#' @param data.file The name of the data file to be read.
#' @param filename The path to the data set to be loaded.
#' @param variable.name The name to be assigned to in the global environment.
#'
#' @return No value is returned; this function is called for its side effects.
#'
#' @importFrom ProjectTemplate translate.dcf
#'
#' @examples
#' library('ProjectTemplate')
#'
#' \dontrun{es.reader('example.es', 'data/example.es', 'example')}
html.table.reader <- function(data.file, filename, variable.name)
{
  url.info <- translate.dcf(filename)

  url <- url.info[["url"]]

  web_page <- rvest::html(url)
  tables <- rvest::html_nodes(web_page, "table")

  for(table in tables) {
    table.name <- rvest::html_attr(table, "id")
    if(!is.na(table.name)) {
    table.name <- gsub("-", ".", table.name)  
    assign(paste(variable.name, table.name, sep="."),
      rvest::html_table(table),
           envir = .GlobalEnv)     
    }
  }
}

.onLoad <- function(...)
{
	.add.extension('html.table', html.table.reader)
}




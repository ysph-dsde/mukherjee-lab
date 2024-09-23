library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)


make_doi_link <- function(citation) {
  
  citation <- gsub(
    pattern = "\\.$", 
    replacement = "", 
    citation
  )
  
  citation <- gsub(
    pattern = "(DOI: )([0-9\\.\\/a-zA-Z]+)", 
    replacement = "DOI: [\\2](https://doi.org/\\2)", 
    citation
  )
  
  citation
}

make_pub_list <- function(pubs) {
  pubs_list <- list()
  for (ii in 1:nrow(pubs)) {
    pubs_list[[ii]] <- make_pub(pubs[ii, ], index = ii)
  }
  return(htmltools::HTML(paste(unlist(pubs_list), collapse = "")))
}

make_pub <- function(pubs, index = NULL) {
  # Make DOI link
  pubs$citation <- make_doi_link(pubs$citation)
  
  header <- FALSE
  if (is.null(index)) {
    cite <- pubs$citation
  } else {
    cite <- glue::glue('{index}) {pubs$citation}')
    if (index == 1) { header <- TRUE }
  }
  return(htmltools::HTML(glue::glue(
    '<div class="pubs">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    </div>
    </div>'
  )))
}


markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}
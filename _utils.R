library(htmltools)
library(markdown)
library(glue)
library(rentrez)

get_citations <- function() {
  # Initial search for Bhramar Mukherjee
  initial_search <- entrez_search("pubmed",
                                  "Bhramar Mukherjee", 
                                  retmax = 1000,
                                  use_history = TRUE) # return web_history for use in later calls
  
  # Grab PMID
  pmids <- initial_search$ids
  # Find summary
  pmid_summary <- entrez_summary(db = "pubmed", 
                                 web_history = initial_search$web_history)
  
  # Final result
  citations <- vector("character", length(pmid_summary))
  for (ii in c(1:length(pmid_summary))) {
    # Single publication
    pub <- pmid_summary[[ii]]
    
    # Extract relevant info for each publication
    title <- pub$title
    epub_date <- paste0(pub$epubdate, ".")
    authors <- pub$authors$name
    authors <- paste0(authors, collapse = ", ")
    journal <- paste0(pub$source, ".")
    doi <- paste0(pub$elocationid, ".")
    uid <- pub$uid
    pmid <- paste("PMID:", pub$uid)
    title_hyperpink <- glue::glue('[{title}](https://pubmed.ncbi.nlm.nih.gov/{uid}/)')
    
    citation <- paste0(title_hyperpink, "<br> ", authors, "<br> ", journal, " ",
                       epub_date, " ", doi, "<br> ", pmid)
    
    citations[ii] <- citation
  }
  
  citations
}

make_pub_list <- function(citations) {
  pubs_list <- list()
  for (ii in 1:length(citations)) {
    pubs_list[[ii]] <- make_pub(citations[ii], index = ii)
  }
  return(htmltools::HTML(paste(unlist(pubs_list), collapse = "")))
}

make_pub <- function(citation, index = NULL) {
  
  header <- FALSE
  if (is.null(index)) {
    cite <- citation$citation
  } else {
    cite <- glue::glue('{index}) {citation}')
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
  return(htmltools::HTML(markdown::renderMarkdown(text = text)))
}

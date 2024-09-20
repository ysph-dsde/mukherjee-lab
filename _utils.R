library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

# TODO: Link the title of paper with PubMed link.

process_pubs <- function(pubs) {
  # NOTE: Citations are already included in our CSV
  # pubs <- make_citations(pubs) 
  if (!is.na(pubs$doi)) {
    pubs$doi <- make_doi(pub$doi)
  }
  
  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {

  
  pub$year <- glue::glue("({pub$year})")
  pub$title <- glue::glue('"{pub$title}"')
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$author, pub$year, pub$title, pub$journal, 
    pub$number, pub$doi
  ))
}

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_pub_list <- function(pubs) {
  pubs_list <- list()
  for (ii in 1:nrow(pubs)) {
    pubs_list[[i]] <- make_pub(pubs[ii, ], index = i)
  }
  return(htmltools::HTML(paste(unlist(pubs_list), collapse = "")))
}

make_pub <- function(pubs, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pubs)
  if (is.null(index)) {
    cite <- pubs$citation
    icons <- make_icons(pubs)
  } else {
    cite <- glue::glue('{index}) {pubs$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pubs)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="pubs">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    <div class="g-col-1"> {altmetric} </div>
    </div>
    {icons}
    </div>'
  )))
}

make_altmetric <- function(pubs) {
  altmetric <- glue::glue('<div data-badge-type="donut" data-doi="{pubs$doi}" data-hide-no-mentions="true" class="altmetric-embed"></div>')
  return(altmetric)
}


aside <- function(text) {
  return(tag("aside", list(text)))
}

center <- function(text) {
  return(tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(tag("b", text)))))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  if (pub$summary) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "Summary",
      url  = pub$url_summary, 
      class = "icon-link-summary", 
      target = "_self"
    )))      
  }
  if (!is.na(pub$url_pub)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url_pub
    )))
  }
  if (!is.na(pub$url_pdf)) {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf",
      text = "PDF",
      url  = pub$url_pdf
    )))
  }
  if (!is.na(pub$url_repo)) {
    html <- c(html, as.character(icon_link(
      icon = "fab fa-github",
      text = "Code & Data",
      url  = pub$url_repo
    )))
  }
  if (!is.na(pub$url_other)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = pub$other_label,
      url  = pub$url_other
    )))
  }
  if (!is.na(pub$url_rg)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-researchgate",
      # text = "&nbsp;",
      text = "RG",
      url  = pub$url_rg
    )))
  }
  if (!is.na(pub$url_scholar)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-google-scholar",
      # text = "&nbsp;",
      text = "Scholar",
      url  = pub$url_scholar
    )))
  }
  return(paste(html, collapse = ""))
}

# The icon_link() function is in {distilltools}, but I've modified this
# one to include  a custom class to be able to have more control over the
# CSS and an optional target argument

icon_link <- function(
    icon = NULL,
    text = NULL,
    url = NULL,
    class = "icon-link",
    target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}

make_media_list <- function() {
  media <- gsheet::gsheet2tbl(
    url = 'https://docs.google.com/spreadsheets/d/1xyzgW5h1rVkmtO1rduLsoNRF9vszwfFZPd72zrNmhmU/edit#gid=2088158801')
  temp <- media %>% 
    mutate(
      date = format(date, format = "%b %d, %Y"), 
      outlet = paste0("**", outlet, "**"),
      post = paste0("- ", date, " - ", outlet, ": ", post)
    )
  return(paste(temp$post, collapse = "\n"))
}

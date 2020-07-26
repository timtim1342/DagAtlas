# packages
library(tidyverse)
library(lingtypology)
library(DT)

extr_map <- function(fe) {
  vill <- read_tsv("./data/villages.csv") # villages dataset
  meta <- read_tsv("./data/meta.csv") # language metadata and colors
  
  # preparation of data
  vill <- vill[complete.cases(vill$lat),] # remove villages for which we do not have coordinates (yet)
  meta_core <- meta %>% # remove idioms not (yet) recognized as distinct
    filter(core == "yes")
  vill_meta <- merge(vill, meta_core, by = "lang") # merge villages and coordinates with language metadata
  fe_vill <- merge(vill_meta, fe, by = "lang") # merge villages, coordinates, and language metadata with feature information
  fe_vill$datapoint <- "extrapolated datapoint"
  
  mp <- map.feature(lang.gltc(fe_vill$glottocode),
                      latitude = fe_vill$lat,
                      longitude = fe_vill$lon,
                      features = fe_vill$lang, # color feature = language
                      color = fe_vill$lang_color,
                      stroke.features = fe_vill$value, # stroke.feature = your feature value
                      stroke.color = c("black", "white"),
                      label = fe_vill$lang,
                      zoom.control = T,
                      popup = paste("<b>Village:</b>", fe_vill$village, "<br>",
                                    "<b>Source:</b>", fe_vill$source, fe_vill$page, "<br>",
                                    "<b>Datapoint:</b>", fe_vill$datapoint),
                      width = 3, stroke.radius = 8,
                      legend = FALSE)
  return(mp)
}

gen_map <- function(fe) {
  vill <- read_tsv("./data/villages.csv") # villages dataset
  meta <- read_tsv("./data/meta.csv") # language metadata and colors
  
  # preparation of data
  vill <- vill[complete.cases(vill$lat),] # remove villages for which we do not have coordinates (yet)
  meta_core <- meta %>% # remove idioms not (yet) recognized as distinct
    filter(core == "yes")
  vill_meta <- merge(vill, meta_core, by = "lang") # merge villages and coordinates with language metadata
  fe_vill <- merge(vill_meta, fe, by = "lang") # merge villages, coordinates, and language metadata with feature information
  fe_vill$datapoint <- "extrapolated datapoint"
  
  # filter core languages
  core_meta <- meta %>%
    filter(core == "yes")
  core_data <- left_join(core_meta, fe, by = "lang")
  core_data$datapoint <- "general datapoint"
  
  # draw a map
 mp <- map.feature(lang.gltc(core_data$glottocode),
              latitude = core_data$gltc_lat,
              longitude = core_data$gltc_lon,
              features = core_data$lang, # color feature = language
              color = core_data$lang_color,
              stroke.features = core_data$value, # stroke.feature = your feature value
              stroke.color = c("black", "white"),
              label = core_data$lang,
              zoom.control = T,
              popup = paste("<b>Source:</b>", core_data$source, core_data$page, "<br>",
                            "<b>Datapoint:</b>", core_data$datapoint),
              width = 3, stroke.radius = 8,
              legend = FALSE)
 return(mp)
}

d_b <- function(fe) {
  # select which feature data you want to show in the datatable
  dtable <- fe %>%
    mutate(value = factor(value)) %>%
    select(c(id, lang, idiom, feature, value, source, page, contributor))
  cit <- vector("character", length(dtable$source))
  for (i in seq_along(dtable$source))
  {
    if (grepl("; ", dtable$source[[i]], fixed = TRUE) == TRUE)
    {
      cits = strsplit(dtable$source[[i]], "; ")
      for (j in seq_along(cits))
      {
        cits[[j]] <- RefManageR::Cite(bib = bib, cits[[j]], .opt = list(max.names = 2))
      }
      dtable$source[[i]] = paste(cits, sep = "; ")
    }
    else
    {
      dtable$source[[i]] <- RefManageR::Cite(bib = bib, dtable$source[[i]], .opt = list(max.names = 2))
    }
  }
  # generate searchable datatable
  db <- DT::datatable(dtable,
                      filter = "top",
                      escape = FALSE,
                      rownames = FALSE,
                      extensions = 'Buttons',  # buttons
                      options = list(
                        columnDefs = list(list(searchable = FALSE, targets = 0)),
                        pageLength = 5,
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        ordering = TRUE,
                        dom = 'fBltpi',  # add map-lang-link button, fff
                        buttons = list(list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = '<i class="fas fa-download"></i>'
                        ))))
  return(db)
}
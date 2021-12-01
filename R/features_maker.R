# packages
library(tidyverse)
library(lingtypology)
library(DT)
library(ymlthis)
library(RefManageR)

data_prep <- function(fe){
  # based on Samira's code
  villages <- read_tsv("../data/tald_villages.csv") # villages dataset
  
  # preparing data for ubiralka
  
  # remove data not for mapping
  
  fe <- fe[(fe$map == "yes"),]
  
  # split feature data into dialect levels
  # this returns a list of dataframes (ordered alphabetically :(), which you then split
  
  # проблемы для автоматизации, я думаю, возникнут здесь: не во всех таблицах одинаковое количество types, у кого-то может быть, например, еще dialect_nt2, и получается 5 тайпов. Или, наоборот, может быть у кого-то только language, village, dialect_toplevel.
  # Т: переделал grop_by в filter и добавил if в конце для nt1, которого может и не быть, а может быть
  
  fe_l <- fe %>% 
    filter(fe$type == "language")
  fe_tl <- fe %>% 
    filter(fe$type == "dialect_toplevel")
  fe_v <- fe %>% 
    filter(fe$type == "village")
  
  # assign granularity level for each set
  
  fe_tl$granularity <- "toplevel dialect"
  fe_v$granularity <- "village dialect"
  fe_l$granularity <- "language"
  
  # merge feature data with villages dataset
  
  # create matching columns
  
  colnames(fe_tl)[colnames(fe_tl) == "idiom"] <- "dialect_toplevel"
  colnames(fe_v)[colnames(fe_v) == "idiom"] <- "village_dialect"
  colnames(fe_l)[colnames(fe_l) == "idiom"] <- "standard"
  
  # merge villages and data per level
  
  tlevel_villages <- merge(villages, fe_tl, by = "dialect_toplevel")
  v_villages <- merge(villages, fe_v, by = "village_dialect")
  lang_villages <- merge(villages, fe_l, by = "standard")
  
  # all above for nt1
  if ('dialect_nt1' %in% fe$type) {
    fe_nt1 <- fe %>% 
      filter(fe$type == "dialect_nt1")
    fe_nt1$granularity <- "non-toplevel 1 dialect"
    colnames(fe_nt1)[colnames(fe_nt1) == "idiom"] <- "dialect_nt1"
    nt1level_villages <- merge(villages, fe_nt1, by = "dialect_nt1")
    
    # merge the different levels with villages (in order high granularity (village) < low granularity (dialect_toplevel))
    dialects_villages <- bind_rows(v_villages, nt1level_villages, tlevel_villages, lang_villages)
  } else {
    
    # merge the different levels with villages (in order high granularity (village) < low granularity (dialect_toplevel))
    dialects_villages <- bind_rows(v_villages, tlevel_villages, lang_villages)
  }
  
  dialects_villages_clean <- dialects_villages[!duplicated(dialects_villages$village),]
  
  # isolate general language data
  
  fe_gl <- fe %>%
    filter(genlang_point == "yes") %>%
    mutate(granularity = "language") %>%
    mutate(default_level = lang) %>%
    select(-idiom)
  
  # merge feature data and village set
  
  glang_villages <- merge(villages, fe_gl, by = "default_level")
  
  # merge everything
  
  alldata <- bind_rows(dialects_villages_clean, glang_villages)
  alldata_clean <- alldata[!duplicated(alldata$village),]
  
  # prepare data for simple maps
  
  # filter general language points
  
  fe_genlang <- fe %>%
    filter(genlang_point == "yes")
  
  # check if each language has max 1 genlang point
  
  #table(duplicated(fe_genlang$lang))
  
  # merge villages, coordinates, and language metadata with feature information
  
  fe_vill <- merge(villages, fe_genlang, by = "lang") 
  
  fe_simple <- fe_vill %>%
    filter(!duplicated(lang))
  
  
  # количесто значений в легенду (можно упростить?)
  alldata_clean %>% 
    group_by(value) %>% 
    tally() -> summm
  
  left_join(alldata_clean, summm, by="value") %>% 
    mutate(value = paste0(value, ' (', as.character(n), ')')) -> alldata_clean
  
  fe_vill %>% 
    group_by(value) %>% 
    tally() -> summm
  
  left_join(fe_vill, summm, by="value") %>% 
    mutate(value = paste0(value, ' (', as.character(n), ')')) -> fe_vill
  
  fe_simple %>% 
    group_by(value) %>% 
    tally() -> summm
  
  left_join(fe_simple, summm, by="value") %>% 
    mutate(value = paste0(value, ' (', as.character(n), ')')) -> fe_simple
  
  
  return_list <-list("alldata_clean"=alldata_clean, "fe_vill"=fe_vill, "fe_simple"=fe_simple)
  
  return(return_list)
}

ubiralka_map <- function(fe) {  # карта с убиралкой
  return_list <- data_prep(fe)
  alldata_clean <- return_list$alldata_clean
  
  mp<- map.feature(lang.gltc(alldata_clean$gltc_lang),
                   latitude = alldata_clean$lat,
                   longitude = alldata_clean$lon,
                   features = as.factor(alldata_clean$value), # title = unique(alldata_clean$value_name),
                   label = alldata_clean$lang.x,
                   color = "magma",
                   popup = paste(alldata_clean$village, "|", alldata_clean$rus_village, "<br>",
                                 "data:", alldata_clean$granularity, "<br>",
                                 "value:", alldata_clean$value),
                   control = alldata_clean$granularity,
                   zoom.control = T,
                   legend.position = 'bottomleft')
  
  return(mp)
}

extr_map <- function(fe) {
  return_list <- data_prep(fe)
  fe_vill <- return_list$fe_vill
  
  mp <- map.feature(lang.gltc(fe_vill$gltc_lang),
                    latitude = fe_vill$lat, 
                    longitude = fe_vill$lon,
                    features = fe_vill$default_level,
                    color = fe_vill$lang_col,
                    legend = F,
                    label = fe_vill$lang,
                    stroke.features = as.factor(fe_vill$value),
                    stroke.color = "magma", # stroke.title = unique(fe_vill$value_name),
                    popup = paste(fe_vill$village, "|", fe_vill$rus_village, "<br>",
                                  "data:", 'extrapolated datapoin', "<br>",
                                  "value:", fe_vill$value),
                    zoom.control = TRUE,
                    legend.position = 'bottomleft')
  
  return(mp)
}

gen_map <- function(fe) {
  return_list <- data_prep(fe)
  fe_simple <- return_list$fe_simple
  
  mp <- map.feature(lang.gltc(fe_simple$gltc_lang),
                    latitude = fe_simple$lat,
                    longitude = fe_simple$lon, 
                    features = fe_simple$default_level,
                    color = fe_simple$lang_col,
                    legend = F,
                    label = fe_simple$lang,
                    stroke.features = fe_simple$value, 
                    stroke.color = "magma", # stroke.title = unique(fe_simple$value_name),
                    popup = paste(fe_simple$village, "|", fe_simple$rus_village, "<br>",
                                  "data:", 'general datapoin', "<br>",
                                  "value:", fe_simple$value),
                    zoom.control = TRUE,
                    legend.position = 'bottomleft')
 return(mp)
}

gen_map_byfe <- function(fe) {
  return_list <- data_prep(fe)
  fe_simple <- return_list$fe_simple
  
  mp <- map.feature(lang.gltc(fe_simple$gltc_lang),
                    features = as.factor(fe_simple$value), # title = unique(fe_simple$value_name),
                    color = "magma",
                    label = fe_simple$lang,
                    popup = paste(fe_simple$village, "|", fe_simple$rus_village, "<br>",
                                  "data:", 'general datapoin', "<br>",
                                  "value:", fe_simple$value),
                    zoom.control = T,
                    legend.position = 'bottomleft')
  return(mp)
}

d_b <- function(fe, col_names) {
  
  keys_present <- c()
  keys_lost <- c()
  fe_name <- col_names[3]  # тип имя файла
  bib <- ReadBib(file = "../data/bibliography.bib")
  
  # select which feature data you want to show in the datatable
  dtable <- fe %>%
    select(col_names)
  cit <- vector("character", length(dtable$source))
  for (i in seq_along(dtable$source))
  {
    if (is.na(dtable$source[[i]]))
        {
      dtable$source[[i]] <- "NA"
      }
    else if (grepl("; ", dtable$source[[i]], fixed = TRUE) == TRUE)
      {
      cits = strsplit(dtable$source[[i]], "; ")
      for (j in seq_along(cits))
      {
        if (length(SearchBib(bib, key=cits[[j]])) == 0){
          append(keys_lost, cits[[j]])->keys_lost
          cits[[j]] <- ""
        }
        else
        {
          append(keys_present, cits[[j]])->keys_present
          cits[[j]] <- paste0('<a href="#ref-', cits[[j]], '">', cits[[j]], '</a>')
        }
      }
      dtable$source[[i]] = paste(cits, sep = "; ")
    }
    else
    {
      print(dtable$source[[i]])
      if (length(SearchBib(bib, key=dtable$source[[i]])) == 0){
        append(keys_lost, dtable$source[[i]])->keys_lost
        dtable$source[[i]] <- ""
      }
      else
      {
        append(keys_present, dtable$source[[i]])->keys_present
        dtable$source[[i]] <- paste0('<a href="#ref-', dtable$source[[i]], '">', dtable$source[[i]], '</a>')
      }
      
    }
  }
  #append(keys_lost)
  c(fe_name, keys_lost, '\n')->keys_lost
  cat(keys_lost, file = '../errors_log.txt', append = TRUE, sep='\n')
  
  # generate searchable datatable
  db <- DT::datatable(dtable,
                      filter = "top",
                      escape = FALSE,
                      rownames = FALSE,
                      extensions = c('Buttons', 'FixedColumns'),  # buttons
                      options = list(
                        pageLength = 5,  # columnDefs = list(list(searchable = FALSE, targets = 0)),
                        paging = TRUE,
                        searching = TRUE,
                        fixedColumns = TRUE,
                        ordering = TRUE,
                        dom = 'fBltpi',  # add map-lang-link button, fff
                        scrollX = TRUE,
                        buttons = list(list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = '<i class="fas fa-download"></i>'
                        ))))
  return(db)
}

yml_header_params <- function(feature_id) {
  
  info <- read_tsv('../data/features.csv')
  info %>% 
    filter(id == feature_id) -> feature_info
  
  names <- c()
  surnames <- c()
  authors <- feature_info$author
  unlist(strsplit(authors, ", ")) -> sur_names
  
  for (i in 1:length(sur_names)){
    unlist(strsplit(sur_names[i], " ")) -> n
    names <- append(names, n[1])
    surnames <- append(surnames, n[2])
    
  }
  
  author_bib = paste(names[1], surnames[1])  # поле в биб ссылку и yml
  if (length(names) >1){
    for (i in 2:length(names)){
      author_bib = paste0(author_bib, ' and ', names[i], ' ', surnames[i])
    } 
  }
  
  author = paste0(surnames[1], ', ', names[1])
  if (length(names) >1){
    for (i in 2:length(names)){
      author = paste0(author, ' and ', surnames[i], ', ', names[i])
    } 
  }
  
  
  title = feature_info$title
  cdate = feature_info$cdate
  udate = paste('Last update:', feature_info$udate)
  bibliography = paste0('./data/features/ref_bibs/', feature_info$filename, '.bib')  # с путями осторожно, тут . т.к. это пишется в файл, который в другой дир
  
  yml_header_params_list <-list("title"=title, "cdate"=cdate, "udate"=udate, "author"=author, "author_bib"=author_bib, "bibliography"=bibliography)
  
  return(yml_header_params_list)
}

yml_header_map <- function(feature_id) {
  yml_header_params_list <- yml_header_params(feature_id)
  
  yml() %>% 
    yml_title(paste(yml_header_params_list$title, '(Maps&Data)')) %>% 
    yml_author(yml_header_params_list$author_bib) %>% 
    yml_date(yml_header_params_list$udate) %>% 
    yml_citations(bibliography = "./data/bibliography.bib", csl = './data/apa.csl', nocite = "@*")
    
}

yml_header <- function(feature_id) {
  yml_header_params_list <- yml_header_params(feature_id)
  
  yml() %>% 
    yml_title(yml_header_params_list$title) %>% 
    yml_author(yml_header_params_list$author_bib) %>% 
    yml_date(yml_header_params_list$udate) %>% 
    yml_citations(bibliography = yml_header_params_list$bibliography, link_citations = TRUE, csl = './data/apa.csl')
}
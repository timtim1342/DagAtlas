library(stringr)

html_example <- function(morphs, glosses, translation){
  s_glossed <- ''
  colspan <- length(morphs)
  for (word in glosses)
    {
    w_glossed <- ''
    characters <- strsplit(word, '')
    case <- 'l'
    for (i in seq_along(characters[[1]]) )
      {
      if (str_detect(characters[[1]][i], '[[:upper:]]'))
        {
        if (case == 'l')
          {
          case <- 'u'
          w_glossed <- paste(w_glossed, 'tag', characters[[1]][i], sep = ' ')
          } else {
            w_glossed <- paste(w_glossed, characters[[1]][i], sep = ' ')
          }
      } else {
        if (case == 'u'|| i == length(characters[[1]]))
        {
          case <- 'l'
          w_glossed <- paste(w_glossed, 'tag', characters[[1]][i], sep = ' ')
        } else {
        w_glossed <- paste(w_glossed, characters[[1]][i], sep = ' ')
        }
      }
    }
    s_glossed <- paste(s_glossed, w_glossed, sep = ' ') 
    }
  return(s_glossed)
  }

html_example(c('di-da-sa', 'ɬik’-a-w'), c('1SG-SUP-EL', 'good-ADJZ-M'), 'You...')

characters = strsplit('good-ADJZ-M', '')
for (i in seq_along(characters))
  {
  case <- 'l'
  print(i)
  if (5 == 5)
    {
    print(paste(characters[i], '   sd'))
    }
}
  
characters[[1]][1]
library(DeclareDesign)
library(knitr)
library(tidyverse)
library(kableExtra)
library(CausalQueries)
library(DesignLibrary)
library(xtable)
library(knitr)
library(sandwich)
library(lmtest)
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggtext)
library(latex2exp)
library(cowplot)
library(pwrss)

options(mc.cores = parallel::detectCores())

knitr::opts_chunk$set(echo = TRUE)
## library(rdddr)
source("assets/ggplot_dd_theme.R")
set.seed(343)
run <- FALSE

theme_set(theme_light())

bs_style <- c("striped", "hover", "condensed", "responsive")
options(kable_styling_bootstrap_options = bs_style)

knitr::opts_chunk$set(
  echo = TRUE, 
  warning = FALSE, 
  error = TRUE, 
  message = FALSE,
  eval = TRUE #, 
  #tidy = TRUE#, 
  #results = 'asis'
  )


hj_ggdag <- function(x = NULL,
                     y = NULL,
                     names = NULL,
                     arcs = NULL,
                     statement = NULL,
                     model = NULL, #accepts causal model objects and returns ggdag
                     title = "",
                     padding = .1, # padding around box if labels = T
                     labels = FALSE,
                     textcol = 'black', # text colour (not label)
                     textsize = 3.88, #text size (not label)
                     force = 0, #repelling force between labels
                     obscure=NULL, # obscure arrows of the form X->Y
                     shape = 16,
                     nodecol = 'lightgrey',
                     nodesize = 16,
                     labelsize = 3.88,
                     labelparse = TRUE,
                     ...) { # other arguments passed to ggdag and geom_dag_label_repel, e.g. force_pull, node = T/F
  
  # Checks
  if(is.null(model) & is.null(names))
    stop("Names should be provided directly or via model argument")
  if(is.null(statement) & is.null(model) & is.null(arcs))
    stop("Model statement should be provided directly or via model or arcs argument")
  
  # Get names
  nodes <- if (is.null(names)) model$nodes else LETTERS[1:length(names)]
  
  # Get statement
  if(!is.null(model)) statement <- model$statement
  if(!is.null(arcs))
    statement <-  paste(nodes[arcs[,1]], " -> ", nodes[arcs[,2]], collapse = "; ")
  dagitty_statement <-  paste("dag{", statement, "}") %>% dagitty
  
  
  # Add coordinates if provided (otherwise generated)
  
  if(!is.null(x)){
    names(x) <- nodes
    names(y) <- nodes
    
    coordinates(dagitty_statement) <-
      list(x = x , y = y) %>%
      coords2df() %>% coords2list()
  }
  
  # Make the df
  df <- dagitty_statement %>% tidy_dagitty()
  df$data <- df$data %>% mutate(
    label = if(is.null(names)) name else
      names %>% as.character %>% .[match(df$data$name,LETTERS)],
    end = if(is.null(names)) to else
      names %>% as.character %>% .[match(df$data$to,LETTERS)],
    update=paste0(label,end),
    pos=match(label,names)) %>%
    arrange(-desc(pos))
  
  #matching bit is necessary because the dataframe doesn't always list all names in the order you first specify
  
  # remove any arrows to be obscured
  if (!is.null(obscure)) {obscoords<-data.frame(update = lapply(obscure %>%
                                                                  str_split('->'),paste,collapse='') %>%
                                                  unlist())
  df$data$direction[match(obscoords$update,df$data$update)]<-NA}
  
  
  # Step 2: Format and export
  p <- df %>%
    ggplot(aes(x=x,y=y,xend=xend,yend=yend)) +
    geom_dag_point(colour=nodecol, shape=shape, size=nodesize) +
    theme_dag() +
    labs(title = TeX(title %>% str_remove_all('\\"')))
  
  if (labels==TRUE){
    parse <- ifelse(class(names)=='expression',TRUE,FALSE)
    
    p +
      geom_dag_label_repel(aes(label = label), #, fill = 'label'),
                           show.legend = TRUE,
                           parse = labelparse,
                           box.padding = padding,
                           hjust = 0,
                           segment.color = 'grey',
                           segment.size = 0.5,
                           min.segment.length=0.5,
                           size = labelsize,
                           force = force+2,
                           ...) +
      geom_dag_edges()
    
  } else {
    # Labels centered on nodes
    p +
      geom_dag_text_repel(aes(label = label),
                          show.legend = FALSE,
                          parse = labelparse,
                          color=textcol,
                          size=textsize,
                          box.padding = 0,
                          force = force
      ) + geom_dag_edges()
  }
}


perm <- function(v) {
  sapply(1:length(v), function(x) {
    rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
         length.out=prod(v))
  } ) - 1
}

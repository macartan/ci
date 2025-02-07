if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(DeclareDesign, knitr, tidyverse, kableExtra, CausalQueries, DesignLibrary, xtable, 
               sandwich, lmtest, dagitty, ggdag, ggtext, latex2exp, cowplot, pwrss, ri2, future, SuperLearner, drtmle, ggpubr)


options(mc.cores = parallel::detectCores())

knitr::opts_chunk$set(echo = TRUE)
## library(rdddr)
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


theme_dd <-
  function() {
    theme_minimal() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = "none"
      )
  }

hex_add_alpha <- function(col, alpha) {
  sprintf("%s%02X", col, floor(alpha * 256))
}

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_orange <- "#F38672"
dd_purple <- "#7E43B6"
dd_gray <- gray(0.2)
dd_pink <- "#C6227F"
dd_light_gray <- gray(0.8)


dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"

one_color_palette <- dd_light_blue
two_color_palette <- c(dd_dark_blue, dd_pink)
three_color_palette <- c(dd_light_blue, dd_orange, dd_pink)

one_color_palette_gray <- c(dd_dark_blue, dd_light_gray)
two_color_palette_gray <- c(two_color_palette, dd_light_gray)
three_color_palette_gray <- c(three_color_palette, dd_light_gray)


# plus gray
# 
# one_color_palette <- dd_light_blue
# two_color
# three_color

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

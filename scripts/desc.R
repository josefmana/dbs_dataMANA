# This script extracts descriptions (numerical & visual). It ought to be run only after 00_import.R!

# clear environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","tidyverse","ggplot2")

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a folder for tables and figures
sapply( c("tabs","figs") , function(i) if( !dir.exists(i) ) dir.create(i) )

# set theme for plotting in ggplot2
theme_set( theme_minimal(base_size = 10) )

# read the original data
d0 <- read.csv( here("_raw","data","ITEMPO-ManaExportNeuropsych_DATA_2024-02-18_1157.csv"), sep = "," )
scl <- read.csv( here("helpers","test_scoring.csv"), sep = ";" ) # scales to be described

# drop row with IAPI for now until we pre-process it
scl <- scl[ scl$scale != "iapi", ]


# extract IDs in order of surgery 
ord <-
  
  d0[ d0$redcap_event_name == "operace_arm_1", c("study_id","surgery_date") ] %>%
  mutate( surgery_date = as.Date(surgery_date) ) %>%
  arrange( surgery_date ) %>%
  select( study_id ) %>%
  filter( grepl("IPN",study_id) ) %>%
  unlist()


# read motor and psychological sum scores
df <-
  
  lapply(
    setNames( c("motor","psych"), c("motor","psych") ),
    function(i)
      
      read.csv( here( "_data", paste0(i,"_long_df.csv") ), sep = "," ) %>%
      filter( id %in% ord ) # keep only patients with surgery

  )



# IN-HOUSE FUNCTIONS ---

# prepare and save a plot representing values present in the data
figprint <-
  
  function( dat, var, tit, fil, ord, w = NA, h = NA, col = "#0072B2" ) {
    
    dat %>%
      
      # pre-process the data
      mutate(
        present = ifelse( is.na( get(var) ), F, T ),
        time = ifelse( present, round(stimtime_years,2), NA ),
        event = factor( event, levels = c( "screening", paste0( "y", seq(1,21,2) ) ), ordered = T ),
        id = factor( id, levels = ord, ordered = T )
      ) %>%
      select( id, event, time, present ) %>%
      
      # plotting proper
      ggplot() +
      aes( y = event, x = id, fill = present, label = time ) +
      geom_tile( color = "white" ) +
      geom_text( color = "white", fontface = "bold", size = 2.2 ) +
      scale_fill_manual( values = c("grey95", col ) ) +
      labs(
        title = tit,
        subtitle = "coloured fields indicate cases present in REDCap (with years from surgery indicated by text)",
        y = NULL,
        x = NULL
      ) +
      coord_flip() +
      theme(
        legend.position = "none",
        plot.title = element_text( size = 24, face = "bold", hjust = .5 ),
        plot.subtitle = element_text( size = 14, hjust = .5 )
      )
    
    # save it
    ggsave( filename = fil, dpi = 300, width = w, height = h )
    
  }


# VISUALISATION OF ASSESSMENTS PRESENT ----

# loop through all outcomes and draw figures with included mesures
for ( i in 1:nrow(scl) ) with(
  
  scl,
  figprint(
    dat = df[[substr( type[i], 1, 5 )]],
    var = scale[i],
    tit = long[i],
    col = col[i],
    ord = ord,
    fil = here( "figs", paste0( scale[i], "_dist.jpg" ) ),
    w = 10, h = 42
  )
  
)
  
# SESSION INFO -----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = here("scripts","desc_envir.txt") )

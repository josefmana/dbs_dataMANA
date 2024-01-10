# This is a script used to prepare clinical data of STN DBS treated PD patients from iTEMPO REDCap database.

# The script extracrs MDS-UPDRS-III, DRS-2, BDI-II, STAI-X1, STAI-X2, FAQ and PDAQ longitudinal data sets
# as well as pre-surgery neuropsychological battery data.

# list packages to be used
pkgs <- c("here","tidyverse","purrr","gt")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare a data folder for the outcomes
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ---

# prepare an array of observed data
obs_array <- function( d, vars, nms, resp ) {
  
  # prepare dimension sizes and names
  dims <- sapply( setNames(vars,nms), function(i) length( unique( d[[i]] ) ) )
  dimnms <- lapply( setNames(vars,nms), function(i) unique( d[[i]] ) )
  
  # prepare a data set with all combinations of vars levels included
  d <- expand.grid(dimnms) %>% rev() %>% left_join( d, by = vars )
  
  # set-up an array for the data
  out <- array( data = d[[resp]], dim = dims, dimnames = dimnms )
  return(out)
  
}


# DATA READ ----

# read outcome data
d0 <- read.csv( here("_raw","ITEMPO-ManaExportNeuropsych_DATA_2024-01-09_1537.csv"), sep = "," ) # outcome data
its <- read.csv( here("_raw","mds_updrs_iii_redcap_names.csv"), sep = "," ) # MDS UPDRS-III RedCap names
mot <- read.csv( here("_raw","mds_updrs_iii_scoring.csv"), sep = ";" ) # scoring of MDS UPDRS-III
psy <- read.csv( here("_raw","psycho_scoring.csv"), sep = ";" ) # scoring of psychological variables


# RESPONSE DATA EXTRACTION ----

# do some housekeeping of the data
d0 <-
  
  d0 %>%

  # rename identificators and LEDD
  rename( "id" = "study_id" ) %>%
  rename( "event" = "redcap_event_name") %>%
  rename( "ledd" = "levodopa_equivalent" ) %>%
  
  # drop testing data points and non-PD patients
  filter( !(id %in% c("pokusný Řehoř Dobromyslný","TestPAc") ) ) %>%
  filter( grepl( "arm_1", event ) ) %>%
  
  # re-format some
  mutate(
    
    # re-code and re-format event names
    event =
      
      sub( "_arm_1", "", event ) %>% # no need for "_arm_1" anymore
      sub( "nvtva_", "", . ) %>% # no need for "nvtva_" prefix either
      sub( "operace", "surgery", . ) %>% # re-code surgery from Czech to English
      ifelse( grepl("screening|refresh|surgery", . ), ., sub("r","y",.) ) %>% # re-code from Czech r ('rok') to English y ('year')
      factor( levels = c( "screening", "refresh", "surgery", paste0( "y", seq(1,21,2) ) ), ordered = T ), # re-format to ordered factor
    
    # re-format surgery date
    surgery_date = as.Date( surgery_date, format = "%Y-%m-%d" ),
    
    # add variable denoting whether the patient was operated before (retrospective) or after (prospective) 1.1.2018
    data_set = sapply(
      
      1:nrow(.),
      function(i)
        
        case_when(
          id[i] %in% id[ surgery_date < "2018-01-01"] ~ "retrospective",
          id[i] %in% id[ surgery_date >= "2018-01-01"] ~ "prospective"
        )

    )
  
  ) %>%
  
  # keep only patients with information about when the surgery happened
  filter( complete.cases(data_set) )

# extract patients' IDs conditional on data set type
pats <-
  
  sapply(
    c("retrospective","prospective"),
    function(i) {
      
      p <- unique( d0[ d0$data_set == i, "id" ] ) # extract the data
      print( paste0( i, ": N = ", length(p) ) ) # print number of patients
      return(p) # save the IDs
      
    }
  )


# ---- psychological variables pre-processing ----

# extract FAQ item scores
for( i in unlist( strsplit( with( psy, item[scale=="faq"] ), "," ) ) ) {
  
  d0[ , paste0("faq_",i) ] <-
    
    case_when(
      d0[ , paste0("faq_uvod_",i) ] == 1 ~ d0[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
      d0[ , paste0("faq_uvod_",i) ] == 2 ~ d0[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
    )

}

# remove all FAQ scores apart from item responses
d0 <- d0[ , -which( grepl( "faq_fill|faq_uvod|faq_vykon|faq_nikdy|faq_score", names(d0) ) ) ]

# remove DRS-2 total score from the data set (we want single subscores only)
d0 <- d0[ , -which( names(d0) == "drsii_total" ) ]

# tidy-up column names for BDI-II
names(d0)[ grepl("bdi",names(d0)) ] <- names(d0)[ grepl("bdi",names(d0)) ] %>% sub( "_[^_]*$", "", . )


# ---- extract item-level data ----

# list variables of interest
voi <- c( "mds_updrs_iii", psy$scale )

# extracting proper
d1 <-
  
  lapply(
    
    setNames(voi,voi), # loop through variables of interest
    function(i) {
      
      # MDS UPDRS-III needs special treatment because it was measured in medication/stimulation on/off states
      if ( i == "mds_updrs_iii") {
        
        # prepare data set containing MDS UPDRS-III item responses
        d <-
          
          lapply(
            
            setNames( names(its), names(its) ), # loop through conditions/combinations from above
            function(j) {
              
              # list rows to include
              incl <- with( d0,
                            if( strsplit(j,"_")[[1]][1] == "pre" ) event == "screening"
                            else event %in% paste0("y",seq(1,21,2) )
                            )
            
              # and select only data from included patients and measures from selected combination
              d0[ incl, c( "id", "event", t(its[,j]) ) ] %>%
                
                # rename the columns such that they are identical across combinations
                `colnames<-`(
                  
                  c(# patient id first
                    colnames(.)[1],
                    
                    # way too sophisticated re-coding for items
                    # (because they differ across conditions in RedCap quite enough to make it messy)
                    sub( "_[^_]*$", "" , colnames(.)[2:ncol(.)] ) %>%
                      sub( "mdsupdrs_3", "item", . ) %>%
                      sub( "_ldopatest", "", . ) %>%
                      sub( "_ldopateston", "", . )
                    
                  )
                  
                ) %>%
                
                # add variables for event (pre vs post), medication (on vs off) and stimulation (on vs off vs no)
                mutate(
                  medic = strsplit(j,"_")[[1]][2],
                  stim = strsplit(j,"_")[[1]][3],
                  .after = event
                )
              
            }
            
          ) %>%
          
          # collapse all the data to a single sexy data file
          do.call( rbind.data.frame, . ) %>%
          arrange( by = id ) %>%
          pivot_longer(
            cols = contains("item"),
            values_to = "score",
            names_to = "item",
            names_transform = function(x) sub( "_", "", sub("item_","",x) )
          )
        
        # extract data array
        # VARIABLES ORDER IS CRITICAL FOR CORRECT EXTRACTION
        return( obs_array( d = d, vars = c("item","stim","medic","event","id"), nms = c("item","stim","medic","event","id"), resp = "score") )
        
      } else {
        
        # prepare the data to suitable format
        d <-
          
          pivot_longer(
            data = d0,
            cols = contains( paste0(i,"_") ),
            values_to = "score",
            names_to = "item",
            names_transform = function(x) sub( paste0(i,"_"), "", x )
          ) %>%
          
          select( id, event, item, score ) %>%
          filter( !(event %in% c("refresh","surgery") ) ) # keep only screening and post-surgery
        
        # extract data array
        # VARIABLES ORDER IS CRITICAL FOR CORRECT EXTRACTION
        return( obs_array( d = d, vars = c("item","event","id"), nms = c("item","event","id"), resp = "score") )
        
      }
      
    }
    
  )

# save the resulting array as .rds file
saveRDS( object = d1, file = "_data/response_data.rds" )


# RESPONSE DATA TRANSFORMATION ----

# in this chunk of code we pre-process (i.e., sum items for the most part) response data
# start by reversing item scores where applicable (psychological variables only, no reverse items in MDS UPDRS-III)
with(
  psy,
  for ( i in scale[complete.cases(rev)] ) for ( j in unlist( strsplit(rev[scale==i],",") ) ) {
    
    # reverse item scores by subtracting raw score from scale's (min + max)
    d1[[i]][j, , ] <<- # double arrow to ensure the results will go beyond with()
      ( max[scale==i] + min[scale==i] ) - d1[[i]][j, , ]
    
  }
)

# prepare an array for MDS UPDRS-III subscales
for ( i in mot$scale[-1] ) d1[[i]] <- d1$mds_updrs_iii[ with( mot, unlist( strsplit( item[scale==i], "," ) ) ), , , , ]


# ---- prepare a wide dataframes with outcomes ----

# prepare a dataframe with sum scores of each psychological variable of interest
df <-
  
  lapply(
    
    with( psy, setNames(scale,scale) ),
    function(i)
      sapply( dimnames(d1$faq)$event, function(j) colSums( d1[[i]][ ,j, ], na.rm = F ) ) %>% as.data.frame()
    
  )

# add to df MDS UPDRS-III (sub)scales
for ( i in mot$scale ) {
  
  df[[i]] <-
    
    lapply(
      
      with( dimnames(d1[[i]]), setNames(stim,stim) ), # loop through stimulation conditions
      function(j)
        
        lapply(
          
          with( dimnames(d1[[i]]), setNames(medic,medic) ), # loop through conditions conditions
          function(k)
            
            # loop through events last in order for them to be on the tail of final variable names
            sapply( dimnames(d1[[i]])$event, function(l) colSums( d1[[i]][ ,j,k,l, ], na.rm = F )  )

        )

    ) %>%
    
    # pull all the conditions together
    do.call( cbind.data.frame, . ) %>%
    
    # in column names use underscores "_" instead of dots "."
    `colnames<-`( gsub( ".", "_", colnames(.), fixed = T ) ) %>%
    
    # drop columns of conditions that were not measured (such as preop on stimulation conditions)
    # NOTE THAT THESE COLUMNS CAN BE IDENTIFIED VIA COMPUTING COLUMN MEANS BECAUSE THEIR MEAN DOES NOT EXIST (RESULTING IN NaN)
    select( all_of( colnames(.)[ !( apply( ., 2, mean, na.rm = T ) %>% cbind() %>% is.na() ) ] ) )
  
}


# FREQUENCY TABLES ----

# prepare a folder for tables
if( !dir.exists("tabs") ) dir.create("tabs")

# extract frequency tables for all neuropsychology as well as MDS-UPDRS III
t <-
  
  lapply(
    
    setNames(voi,voi), # loop through variables of interest
    function(i)
      
      df[[i]] %>%
      
      # add data set index
      mutate(
        data_set = sapply(
          rownames(.),
          function(i)
            case_when(
              i %in% pats$retrospective ~ "retrospective",
              i %in% pats$prospective ~ "prospective"
            )
        )
      ) %>%
      
      # make it longer
      pivot_longer(
        cols = !data_set,
        names_to = if ( i == "mds_updrs_iii") c("dbs","med","event") else "event", 
        names_sep = if ( i == "mds_updrs_iii") "_" else NULL,
        values_to = i
      ) %>%
      
      # keep only complete cases and create table out of it
      na.omit() %>%
      select( if ( i == "mds_updrs_iii") c("data_set","event","med","dbs") else c("data_set", "event") ) %>%
      table() %>%
      as.data.frame() %>%
      
      # 
      pivot_wider( names_from = data_set, values_from = Freq ) %>%
      relocate( retrospective, .before = prospective ) %>%
      
      # order it by event
      mutate( event = factor( event, levels = c( "screening", paste0("y",seq(1,21,2)) ), ordered = T ) ) %>%
      arrange( event )
    
  )

# ---- neuropsychology frequency table ----

# prepare a gt object
t1 <-
  
  t[ c("drsii","faq","pdaq","bdi","staix1","staix2") ] %>%
  
  # glue all different neuropsychology measures to a single wide file
  reduce( left_join, by = "event" ) %>%
  relocate( starts_with("retro"), .after = event ) %>%
  mutate( across( where(is.numeric), ~ ifelse( is.na(.x), 0, .x ) ) ) %>%
  
  # prepare a gt object with separate columns for prospective and retrospective data
  gt( caption = "Neuropsychological data" ) %>%
  tab_spanner( label = "Retrospective", columns = starts_with("retrospective") ) %>%
  tab_spanner( label = "Prospective", columns = starts_with("prospective") ) %>%
  
  # re-label columns
  cols_label( ends_with("ve.x.x.x") ~ "STAIX1", ends_with("ve.y.y.y") ~ "STAIX2" ) %>%
  cols_label(  ends_with("ve.x.x") ~ "PDAQ", ends_with("ve.y.y") ~ "BDI-II" ) %>%
  cols_label( ends_with("ve.x") ~ "DRS-2", ends_with("ve.y") ~ "FAQ" ) %>%
  
  # align the text to left for event and center for frequencies
  cols_align( align = "left", columns = "event" ) %>%
  cols_align( align = "center", columns = -1 )
  

# ---- motor signs frequency table ----

# prepare a gt object
t2 <-
  
  t$mds_updrs_iii %>%
  
  # re-code zero for "-" at place where no value could have been produced
  mutate(
    across(
      all_of( c("retrospective","prospective") ),
      ~ case_when(
        event == "screening" & dbs != "none" ~ "-", # no Stim. ON or OFF measures before surgery
        event != "screening" & dbs == "none" ~ "-", # no non-Stim measures after surgery
        .default = as.character(.x)
      )
    )

  ) %>%
  
  # spread the table
  pivot_wider(
    names_from = c("med","dbs"), names_sep = "_",
    values_from = c("retrospective","prospective"),
    id_cols = "event"
  ) %>%
  
  # order the columns appropriately
  relocate( starts_with("retrospective_off"), .after = event ) %>%
  relocate( starts_with("prospective_off"), .after = retrospective_on_on ) %>%
  
  # prepare a gt object with separate columns for prospective and retrospective data
  gt( caption = "MDS-UPDRS III data" ) %>%
  
  # lowermost spanner for medication condition (gather = F for retaining column order)
  tab_spanner( label = "Medication OFF", columns = contains("_off_"), gather = F ) %>%
  tab_spanner( label = "Medication ON", columns = contains("_on_"), gather = F ) %>%
  
  # uppermost spanner for data set type
  tab_spanner( label = "Retrospective", columns = starts_with("retrospective"), gather = F ) %>%
  tab_spanner( label = "Prospective", columns = starts_with("prospective"), gather = F ) %>%
  
  # label columns according to stimulation condition
  cols_label(
    ends_with("_none") ~ "No Stim.",
    ends_with("_off") ~ "Stim. OFF",
    ends_with("_on") ~ "Stim. ON"
  )


# ---- save all frequency tables ----

# NEED TO FIND A WAY TO SAVE IT AS AN IMAGE
gtsave( data = t1, filename = here("tabs","neuropsychology_freqtab.html") )
gtsave( data = t2, filename = here("tabs","mds-updrs_iii_freqtab.html") )


# CHECK THE IDENTITY OF MRI SUBJECTS ----

# read the list of 113 MRI subjects
mrs <- read.csv( here("_raw","MRIsubjects_key.csv"), sep = ";" )

# extract numbers of retrospective vs prospective patients
sapply( c("retrospective","prospective"), function(i) sum( mrs$IPN %in% pats[[i]] ) ) # 57/57

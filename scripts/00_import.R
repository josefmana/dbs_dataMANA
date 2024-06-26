# This is a script used to prepare clinical data of STN DBS treated PD patients from iTEMPO REDCap database.

# The script extracts MDS-UPDRS-III, DRS-2, BDI-II, STAI-X1, STAI-X2, FAQ, PDAQ, IAPI, MADRS and SAS longitudinal data sets
# as well as pre-surgery neuropsychological battery data.

rm( list = ls() ) # clear environment

# load packages
library(here)
library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

# prepare a data folder for the outcomes
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ---

# swap NAs for zeros
naswap <- function(x) ifelse( is.na(x), 0, x )

# extract single items from scale descriptions
itextr <- function(input,x) with( input, strsplit( item[scale==x], ",") )[[1]]

# prepare an array of observed data
obs_array <-
  
  function( d, vars, nms, resp ) {
    
    # prepare dimension sizes and names
    dims <- sapply( setNames(vars,nms), function(i) length( unique( d[[i]] ) ) )
    dimnms <- lapply( setNames(vars,nms), function(i) unique( d[[i]] ) )
    
    # prepare a data set with all combinations of vars levels included
    d <- expand.grid(dimnms) %>% rev() %>% left_join( d, by = vars )
    
    # set-up an array for the data
    out <- array( data = d[[resp]], dim = dims, dimnames = dimnms )
    return(out)
    
  }

# fill-in data from the original raw data frame
fillin <-
  
  # r = number of rows, v = variable, i = patient id, e = event
  function( d = d0, r, v, i, e ) {
    
    sapply(
      
      1:r, # loop through all rows
      function(j)

        # put in place a safe break for rows missing from the original data set
        if ( length( d[ d$id == i[j] & d$event == e[j], v ] ) > 0 ) d[ d$id == i[j] & d$event == e[j], v ] else NA

    )
  }


# DATA READ ----

d0 <- read.csv( here("_raw","data","ITEMPO-ManaExportNeuropsych_DATA_2024-05-31_2055.csv"), sep = "," ) # outcome data
its <- read.csv( here("helpers","mds_updrs_iii_redcap_names.csv"), sep = "," ) # MDS UPDRS-III REDCap names
scl <- read.csv( here("helpers","test_scoring.csv"), sep = ";" ) # scales to be imported
bat <- read.csv( here("helpers","neurpsy_bat.csv"), sep = "," ) # pre-surgery neuropsychological battery

# extract all variables of different types
vars <- sapply( unique(scl$type), function(i) with( scl, scale[type==i] ) )

# do some housekeeping of the original data
d0 <-
  
  d0 %>%

  # rename identificators
  rename( "id" = "study_id" ) %>%
  rename( "event" = "redcap_event_name") %>%
  
  # rename items which are not in an appropriate format
  rename_with( ~ sub("_[^_]*$","",.x), starts_with("bdi") ) %>%
  rename_with( ~ sub("iapi","iapi_",.x), starts_with("iapi") ) %>%
  rename_with( ~ sub("madrs","madrs_",.x), starts_with("madrs") ) %>%
  rename_with( ~ sub("_q","_",.x), starts_with("apatie") ) %>%
  
  # drop testing data points and non-PD patients
  filter( !(id %in% c("pokusný Řehoř Dobromyslný","TestPAc") ) ) %>%
  filter( grepl( "arm_1", event ) ) %>%
  
  # re-format some
  mutate(
    
    # get rid of any underscores in ID
    id = gsub("_","",id),
    
    # re-code and re-format event names
    event =
      
      sub( "_arm_1", "", event ) %>% # no need for "_arm_1" anymore
      sub( "nvtva_", "", . ) %>% # no need for "nvtva_" prefix either
      sub( "operace", "surgery", . ) %>% # re-code surgery from Czech to English
      ifelse( grepl("screening|refresh|surgery", . ), ., sub("r","y",. ) ) %>% # re-code from Czech r ('rok') to English y ('year')
      factor( levels = c( "screening", "refresh", "surgery", paste0( "y", seq(1,21,2) ) ), ordered = T ), # re-format to ordered factor

    # extract common motor assessment date
    motor_date =  ifelse( event == "screening", datum_vysetreni_ldopatest, datum_vysetreni_dbson ),
    
    # levodopa equivalent daily dose (calculated manually)
    # begin by swapping NAs for 0s in the LEDD subcomponents (but keep classical levodopa NA to prevent ended up with bunch of zeros)
    across( starts_with("daily_dose") & !contains("levodopa"), naswap ),
    
    # calculate LEDD by applying the LEDD equation 
    ledd =
      
      daily_dose_levodopa +
      ifelse( daily_dose_entacapone > 0, 0.33 * daily_dose_levodopa, 0 ) +
      1.7 * daily_dose_opicapone +
      1.5 * daily_dose_tolcapone +
      0.75 * daily_dose_levodopa_extend +
      100 * daily_dose_pramipexol +
      20 * daily_dose_ropinirol +
      30 * daily_dose_rotigotine +
      10 * daily_dose_selegiline +
      1 * daily_dose_amantadine,
    
    # re-code nominal variables for later
    sex = case_when( sex == 0 ~ "female", sex == 1 ~ "male" ),
    type_pd = case_when( type_pd == 1 ~ "tremor-dominant", type_pd == 2 ~ "akinetic-rigid" ),
    asym_park = case_when( asym_park == 1 ~ "right", asym_park == 2 ~ "left" ),
    neurpsy_concl = case_when( neurpsy_concl == 1 ~ "PD-NC", neurpsy_concl == 2 ~ "PD-MCI", neurpsy_concl == 3 ~ "PD-D" ),
    stn_dbs = ifelse( target_final == 1, 1, 0 )

  )


# RAW LONGITUDINAL DATA ----

# extract FAQ item scores
for( i in itextr(scl,"faq") ) {
  
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


# extract item-level data ----

# list variables of interest
voi <- c( "mds_updrs_iii", with( vars, c( psychology, psychiatry ) ) )

# add UPDRS III item score for pre-surgery ldopatest in patients who were missing it in original
for( i in itextr(scl,"updrs_iii") ) {
  
  with(
    its, {
      
      d0[ , pre_off_none[item==i] ] <<- ifelse( is.na(d0[ , pre_off_none[item==i] ]), d0[ , ldopatest_off[item==i]], d0[ , pre_off_none[item==i] ] )
      d0[ , pre_on_none[item==i] ] <<- ifelse( is.na(d0[ , pre_on_none[item==i] ]), d0[ , ldopatest_on[item==i]], d0[ , pre_on_none[item==i] ] )

    }
    
  )
  
}

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
            
            setNames( names(its)[2:5], names(its)[2:5] ), # loop through conditions/combinations from above
            function(j) {
              
              # list rows to include
              incl <-
                
                with(
                  d0,
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
            cols = starts_with( paste0(i,"_") ),
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
saveRDS( object = d1, file = here("_data","resp_data.rds") )


# LONGITUDINAL SUM SCORES ----

# in this chunk of code we pre-process (i.e., sum items for the most part) response data
# start by reversing item scores where applicable
with(
  
  scl,
  for ( i in scale[complete.cases(rev)] ) for ( j in unlist( strsplit(rev[scale==i],",") ) ) {
    
    # reverse item scores by subtracting raw score from scale's (min + max)
    d1[[i]][j, , ] <<- # double arrow to ensure results will go beyond with()
      ( max[scale==i] + min[scale==i] ) - d1[[i]][j, , ]
    
  }

)

# prepare an array for MDS UPDRS-III subscales
for ( i in vars$motor[-1] ) d1[[i]] <- d1$mds_updrs_iii[ itextr(scl,i), , , , ]


# wide format data sets ----

# not using IAPI so far
vars$psychology <- vars$psychology[ vars$psychology != "iapi" ]

# prepare a dataframe with sum scores of each psycho variable of interest
df <-
  
  lapply(
    
    with( vars, setNames( c(psychology,psychiatry), c(psychology,psychiatry) ) ),
    function(i)
      sapply( dimnames(d1$faq)$event, function(j) colSums( d1[[i]][ ,j, ], na.rm = F ) ) %>%
      as.data.frame()
    
  )

# add to df MDS UPDRS-III (sub)scales
for ( i in vars$motor ) {
  
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


# long format data sets ----

# prepare a data set separately for neuropsychological and motor outcomes
for( i in c("psych","motor") ) assign(

  paste0( "df.", substr(i,1,3) ),

  lapply(

    setNames( unlist( vars[which(grepl(i,names(vars)))] ), unlist( vars[which(grepl(i,names(vars)))] ) ), # loop through all relevenat scales
    function(j)
      
      df[[j]] %>%
      rownames_to_column("id") %>% # save names from pivoting by extracting them to a column
      pivot_longer( -id, names_to = "event", values_to = j ) %>% # pivoting proper
      mutate( row = paste0(id,"_",event) ) %>% # prepare rownames for binding the data frames
      column_to_rownames("row") %>%
      select( all_of(j) ) # keep responses only (the rest is in the rownames)

  ) %>%
  
    do.call( cbind.data.frame, . ) %>% # binding by do.call(cbind) combo instead of left_join() to spare memory

    # add some other variables of interest
    mutate(

      # extract patients' and events' ids
      id = sapply( 1:nrow(.), function(j) strsplit( x = rownames(.)[j], split = "_" )[[1]][1] ), # patient id
      event = sapply( 1:nrow(.), function(j) tail( strsplit( x = rownames(.)[j], split = "_" )[[1]], n = 1) ), # event
      
      # extract variables fixed at screening
      sex = fillin( d0, nrow(.), "sex", id, rep("screening",nrow(.) ) ),
      hy = fillin( d0, nrow(.), "hy_stage", id, rep("screening",nrow(.) ) ),
      type_pd = fillin( d0, nrow(.), "type_pd", id, rep("screening",nrow(.) ) ),
      asym_park = fillin( d0, nrow(.), "asym_park", id, rep("screening",nrow(.) ) ),
      edu_years = fillin( d0, nrow(.), "years_edu", id, rep("screening",nrow(.) ) ),
      
      # extract time-varying data
      ledd_mg = fillin( d0, nrow(.), "ledd", id, event ),
      
      # dummy variables for time calculations
      ass_date = as.Date( fillin( d0, nrow(.), case_when( i == "psych" ~ "datum_drs", i == "motor" ~ "motor_date" ), id, event ) ),
      birth_date = as.Date( fillin( d0, nrow(.), "dob", id, rep( "screening", nrow(.) ) ) ),
      surg_date = as.Date( fillin( d0, nrow(.), "surgery_date", id, rep( "surgery", nrow(.) ) ) ),
      vat_date = as.Date( fillin( d0, nrow(.), "datum", id, event ) ),
      pd_start_date = fillin( d0, nrow(.), "rok_vzniku_pn", id, rep("screening",nrow(.) ) ),
      
      # time-lengths
      pd_dur = year(ass_date) - pd_start_date,
      age_years = time_length( difftime(ass_date,birth_date), "years" ),
      stimtime_years = time_length( difftime(ass_date,surg_date), "years" ),
      vattime_months = time_length( difftime(ass_date,vat_date), "months"),
      
      # put it all before response variables
      .before = 1
      
    ) %>%
    
    # drop dummy columns for time calculations
    select( !ends_with("_date") ) %>%
    
    # make event ordered factor
    mutate( event = factor( event, levels = c( "screening", paste0( "y", seq(1,21,2) ) ), ordered = T ) ) %>%
    
    # sort by IDs and events
    arrange( id, event ) %>%
    
    # add STN DBS indicator
    left_join( na.omit( d0[ , c("id","stn_dbs") ] ), by = "id" )

)

# for motor symptoms add medication and stimulation condition
df.mot <-
  
  df.mot %>%
  mutate(
    stim = sapply( 1:nrow(.), function(i) strsplit( x = rownames(.)[i], split = "_" )[[1]][2] ), # stimulation condition
    med = sapply( 1:nrow(.), function(i) strsplit( x = rownames(.)[i], split = "_" )[[1]][3] ), # medication condition
    .after = event
  )

# save them
write.table( df.psy, file = here("_data","psych_long_df.csv"), sep = ",", row.names = F, quote = F )
write.table( df.mot, file = here("_data","motor_long_df.csv"), sep = ",", row.names = F, quote = F )


# PREOP NEUROPSYCHOLOGICAL BATTERY ----

# extract pre-surgery neuropsychology data set
d2 <-
  
  # extract variables and rows of interest
  d0 %>%
  filter( event == "screening" ) %>%
  mutate( age_years = time_length( difftime( as.Date(datum_neuropsy_23afdc), as.Date(dob) ), "years" ) ) %>%
  mutate( pd_dur = year( as.Date(datum_neuropsy_23afdc) ) - rok_vzniku_pn ) %>%
  select( id, sex, hy_stage, type_pd, asym_park, pd_dur, years_edu, age_years, all_of(bat$test) ) %>%

  # pivot it
  pivot_longer(
    cols = all_of(bat$test),
    names_to = "test",
    values_to = "score"
  ) %>%
  
  # add test info
  left_join( bat, by = "test" ) %>%
  
  # tidy-up
  rename( "edu_years" = "years_edu" ) %>%
  mutate( test = case_when( grepl("nart",test) ~ "nart", grepl("mmse",test) ~ "mmse", .default = test ) )

# save it
write.table( d2, file = here("_data","preop_lvlII.csv"), sep = ",", row.names = F, quote = F )


# DATA MOVING ----

# read the file with directions for data moving
#mov <- read.csv( here("helpers","movers.csv"), sep ="," )

# loop through all final destinations
#for ( i in 1:nrow(mov) ) with(
#
#  mov, {
#    
#    print( paste0( "moving from ", from[i], " to ", to[i] ) ) # print info
#    file.copy( from = from[i], to = to[i], overwrite = T ) # move it
#    
#  }
#)


# Function generate.plate.map()
#
# 2019-09-04
# Marine Cambon

#### Description: 
# Generate plates map for extraction/PCR/cell cultures from a list of samples.

#### Usage:
# generate.plate.map(s, pos_ctrl_PCR = T, neg_ctrl_PCR = T, pos_ctrl_extract = T, neg_ctrl_extract = T, 
                  # random_samples = T)

#### Arguments:
# s                  a vector containing the samples names
# n_wells            a numerical value indicating the number of wells in the plate
#                       The only supported values for now are 12 and 96.
# pos_ctrl_PCR       a logical value indicating whether to add a PCR positive control in each plate,
#                       or a numerical value indicating the number of controls to put in each plate
# neg_ctrl_PCR       a logical value indicating whether to add a PCR negative control in each plate
#                       or a numerical value indicating the number of controls to put in each plate
# pos_ctrl_extract   a logical value indicating whether to add an extraction positive control in each plate
#                       or a numerical value indicating the number of controls to put in each plate
# neg_ctrl_extract   a logical value indicating whether to add an extraction negative control in each plate
#                       or a numerical value indicating the number of controls to put in each plate
# random_samples     a logical value indicating whether to randomize sample positions in the plate 
#                       Note that PCR and extraction controls will always be located at the end of the plate
# set_plate_counter  a numerical value indicating were plate numbering should start
# set_seed           a single value, interpreted as an integer, to specify the seed for random number
#                       generation and allow reproducibility of the sampling for samples randomization

#### Details:
# Positive and negative extraction and/or PCR controls can be added in each plate. 
# If the total number of samples and controls is not sufficient to completely fill plates, the wells which will
# remain empty are filled with "empty"

#### Value:
# Returns a list. The first element of the list is a dataframe with one row per sample, indicating the plate number
# and the well in the plate for each sample. The second component of the list is a list of the plates maps (each 
# element of the list is one plate)

generate.plate.map <- function(s, n_wells=96,
                               pos_ctrl_PCR = T,
                               neg_ctrl_PCR = T,
                               pos_ctrl_extract = T,
                               neg_ctrl_extract = T,
                               random_samples = T,
                               set_plate_counter = 1,
                               set_seed=NULL) {
  if(!n_wells %in% c(12,96)) stop("The function only works for 12 or 96 wells for now")
  
  # Randomize sample order if needed
  if(random_samples) {
    set.seed(set_seed)
    s <- sample(s, length(s), replace=F)
  }
  
  # Names of the wells
  if(n_wells==96) {
    list_wells <- paste(c('A','B','C','D','E','F','G','H'),sort(rep(1:12,8)),sep="")
    # Number of controls per plate
    nb_ctrl_per_plate <- sum(pos_ctrl_PCR, neg_ctrl_PCR, pos_ctrl_extract, neg_ctrl_extract)
    if(nb_ctrl_per_plate > 0) {
      # Names of the controls per plate
      ctrl_names <- rep(c("pos_ctrl_PCR", "neg_ctrl_PCR", "pos_ctrl_extract", "neg_ctrl_extract"), 
                        c(pos_ctrl_PCR, neg_ctrl_PCR, pos_ctrl_extract, neg_ctrl_extract))
      # Location of the controls in the plate
      wells_ctrl <- list_wells[(97-nb_ctrl_per_plate):96]
      
      nb_samples_per_plate <- 96-nb_ctrl_per_plate
    } else {
      nb_samples_per_plate <- n_wells
    }
  } else {
    list_wells <- paste(c('A','B','C'),sort(rep(1:4,3)),sep="")
    nb_samples_per_plate <- n_wells
    nb_ctrl_per_plate <- 0
  }

  # Number of plates needed
  nb_plates <- ceiling(length(s)/nb_samples_per_plate)
  
  
  # Create unique well ids among plates
  map <- expand.grid(list_wells, seq(set_plate_counter, set_plate_counter-1+nb_plates, 1))
  names(map) <- c("well", "plate")
  # Assign a well id controls 
  map$sample_id <- NA
  if(nb_ctrl_per_plate > 0) map$sample_id[map$well %in% wells_ctrl] <- rep(ctrl_names, nb_plates)
  # Assign a well id to samples and set to "empty" remaining wells
  map$sample_id[is.na(map$sample_id)] <- c(s, rep("empty", (n_wells*nb_plates)-(length(s)+nb_ctrl_per_plate*nb_plates)))
  
  # Generate a more readable map for each plate
  reshaped_map <- lapply(split(map, f = map$plate), function(x) {
    x$col <- gsub("[A-H]([0-9]+)", "\\1", x$well)
    x$row <- gsub("([A-H])[0-9]+", "\\1", x$well)
    res <- reshape(x, timevar = "col", idvar="row", direction="wide", drop=c("well", "plate"))
    names(res) <- gsub("sample.", "col.", names(res))
    return(res)
  })
  
  return(list(map, reshaped_map))
}

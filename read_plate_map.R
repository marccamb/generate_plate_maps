# Function read.plate.map()
#
# 2019-09-04
# Marine Cambon

#### Description: 
# Read a set of plate maps in wide format from multiple csv files and turns them into a tidy dataframe.

#### Usage:
# read.plate.map(file_names, plate_id=NULL, n_wells=96, csv_sep=",", path=".") 
# random_samples = T)

#### Arguments:
# file_names         a vector of the plate map files names
# n_wells            a numerical value indicating the number of wells in the plate
#                       The only supported values for now are 12 and 96.
# plate_id           a vector of plate id corresponding to each plate file. If NULL, 
#                       the plate ids will be number starting at 1.
# csv_sep            a character corresponding to the text file separator 
# path               a character with the path to the directory containing all files
# verbose            a boolean for printing additional messages

#### Details:
# 

#### Value:
# Returns a signle dataframe containing the samples from all plates.

read.plate.map <- function(file_names, plate_id=NULL, n_wells=96, csv_sep=",", path=".", verbose=T) {
  if(is.null(plate_id)) plate_id <- seq(1:length(file_names))
  if (length(file_names)==1) {
    f <- read.table(file.path(path, file_names), sep=csv_sep, h=T)
    if(dim(f)[1]*(dim(f)[2]-1)!= n_wells) stop(paste("The file does not contain the appropriate culumn number for", n_wells, "well plates. Please check the field separator csv_sep, or the number of wells n_wells"))
    res <- reshape(f, idvar = "row", varying = names(f)[-1], direction = "long")
    res$plate <- plate_id
  } else {
    if (verbose) message('Reading plates... Please check that file names and plates IDs are in the same order.')
    res <- NULL
    for (i in 1:length(file_names)) {
      f <- read.table(file.path(path, file_names[i]), sep=csv_sep, h=T)
      if(dim(f)[1]*(dim(f)[2]-1)!= n_wells) stop(paste("The file does not contain the appropriate culumn number for", n_wells, "well plates. Please check the field separator csv_sep, or the number of wells n_wells"))
      if(colnames(f)[1] != "row") stop("The name of the first colum should be \"row\"")
      r <- reshape(f, idvar = "row", varying = names(f)[-1], direction = "long")
      r$plate <- plate_id[i]
      res <- rbind(res, r)
    }
  }
  final_tab <- with(res, data.frame("plate"=plate, "well"=paste(row, time, sep=""), "sample_id"=res$col))
  #message("Done!")
  return(final_tab)
}

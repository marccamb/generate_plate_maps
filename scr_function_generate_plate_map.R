# Function generate.plate.map()
#
# 2019-09-04
# Marine Cambon

#### Description: 
# Generate plates map for extraction/PCR from a list of samples.

#### Usage:
# generate.plate.map(s, pos_ctrl_PCR = T, neg_ctrl_PCR = T, pos_ctrl_extract = T, neg_ctrl_extract = T, 
                  # random_samples = T)

#### Arguments:
# s                  a vector containing the samples names
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
# remain empty are filled with "empty" (euh bon c'est pas clair ce que je dis mais j'arrive pas à le dire)

#### Value:
# Returns a list. The firt component of the list is a dataframe with one row per sample, indicating the plate number
# and the well in the plate for each sample. The second component of the list is a list of the plates maps (each 
# component of the list is one plate)

generate.plate.map <- function(s, pos_ctrl_PCR = T,
                               neg_ctrl_PCR = T,
                               pos_ctrl_extract = T,
                               neg_ctrl_extract = T,
                               random_samples = T,
                               set_plate_counter = 1,
			       set_seed=NULL) {
  # Names of the 96 wells
  list_wells <- paste(c('A','B','C','D','E','F','G','H'),sort(rep(1:12,8)),sep="")
  # Number of controls per plate
  nb_ctrl_per_plate <- sum(pos_ctrl_PCR, neg_ctrl_PCR, pos_ctrl_extract, neg_ctrl_extract)
  if(nb_ctrl_per_plate > 0) {
  # Names of the controls per plate
    ctrl_names <- rep(c("pos_ctrl_PCR", "neg_ctrl_PCR", "pos_ctrl_extract", "neg_ctrl_extract"), 
                      c(pos_ctrl_PCR, neg_ctrl_PCR, pos_ctrl_extract, neg_ctrl_extract))
    # Location of the controls in the plate
    wells_ctrl <- list_wells[(97-nb_ctrl_per_plate):96]
  }
  # Randomize sample order if needed
  if(random_samples) {
	set.seed(set_seed)
	s <- sample(s, length(s), replace=F)
  }
  
  # Number of plates needed
  nb_samples_per_plate <- 96-nb_ctrl_per_plate
  nb_plates <- ceiling(length(s)/nb_samples_per_plate)
  
  
  # Create unique well ids among plates
  map <- expand.grid(list_wells, paste("plate", seq(set_plate_counter, set_plate_counter-1+nb_plates, 1), sep="_"))
  names(map) <- c("well", "plate")
  # Assign a well id controls 
  map$sample <- NA
  if(nb_ctrl_per_plate > 0) map$sample[map$well %in% wells_ctrl] <- rep(ctrl_names, nb_plates)
  # Assign a well id to samples an set to "empty" remaining wells
  map$sample[is.na(map$sample)] <- c(s, rep("empty", (96*nb_plates)-(length(s)+nb_ctrl_per_plate*nb_plates)))
  
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


read.plate.map <- function(file_names, plate_id="1", csv_sep=",", path=".") {
  if (length(file_names)==1) {
    f <- read.table(file.path(path, file_names), sep=csv_sep, h=T)
    if(ncol(f)!=13) stop("The plate does not have 12 columns. Please check the filed separator csv_sep")
    res <- reshape(f, idvar = "row", varying = names(f)[2:13], direction = "long")
    res$plate <- plate_id
  } else {
    message('Reading plates... Please check that file names and plates IDs are in the same order.')
    res <- NULL
    for (i in 1:length(file_names)) {
      f <- read.table(file.path(path, file_names[i]), sep=csv_sep, h=T)
      r <- reshape(f, idvar = "row", varying = names(f)[2:13], direction = "long")
      r$plate <- plate_id[i]
      res <- rbind(res, r)
    }
  }
  final_tab <- with(res, data.frame("plate"=plate, "well"=paste(row, time, sep=""), "sample_id"=res$col))
  message("Done!")
  return(final_tab)
}


print.plate.map <- function(d, col, txt = NULL, pdf=F, png=F, file.name=NULL) {
  if(unique(d$plate) > 1) stop("Only one plate can be printed at a time")
  if(any(pdf,png) & is.null(file.name)) stop("Please provide a file name.")
  
  l <- c("A", "B", "C", "D", "E", "F", "G", "H")
  i <- ifelse(pdf & png, 1, 2)
  while (i < 3) {
    if(i==1 & pdf) pdf(paste0(file.name, ".pdf"), 7,5)
    if(i==2 & png) png(paste0(file.name, ".png"), 7,5, units = "in", res = 300)
    par(bty="n", xpd=T, las=1, mar=c(2,2,2,8),fg="gray30", col.axis="gray30")
    plot(rep(1:12, each=8), rep(8:1, 12), axes=F,
         xlab="", ylab="",
         pch=21, cex=5, col="darkgray", bg=col)
    mtext(rev(l), side = 2, at = 1:8, line = 1)
    mtext(1:12, side = 3, at = 1:12, line = 1)
    mtext(paste("Plate", d$plate[1]), side=1, line=1)
    if(!is.null(txt)) text(rep(1:12, each=8), rep(rev(1:8), 12), txt)
    legend(13,8, pch=19, bg = "white",
           col=unique(col),
           unique(names(col))
           )
    if(i==1 & pdf) dev.off()
    if(i==2 & png) dev.off()
    i <- i + 1
  }
}

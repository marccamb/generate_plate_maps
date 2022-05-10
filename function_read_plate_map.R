read.plate.map <- function(file_names, plate_id=NULL, n_wells=96, csv_sep=",", path=".") {
  if(is.null(plate_id)) plate_id <- seq(1:length(file_names))
  if (length(file_names)==1) {
    f <- read.table(file.path(path, file_names), sep=csv_sep, h=T)
    if(dim(f)[1]*(dim(f)[2]-1)!= n_wells) stop(paste("The plate does not have", n_wells, "columns. Please check the field separator csv_sep, or the number of wells n_wells"))
    res <- reshape(f, idvar = "row", varying = names(f)[-1], direction = "long")
    res$plate <- plate_id
  } else {
    message('Reading plates... Please check that file names and plates IDs are in the same order.')
    res <- NULL
    for (i in 1:length(file_names)) {
      f <- read.table(file.path(path, file_names[i]), sep=csv_sep, h=T)
      r <- reshape(f, idvar = "row", varying = names(f)[-1], direction = "long")
      r$plate <- plate_id[i]
      res <- rbind(res, r)
    }
  }
  final_tab <- with(res, data.frame("plate"=plate, "well"=paste(row, time, sep=""), "sample_id"=res$col))
  message("Done!")
  return(final_tab)
}

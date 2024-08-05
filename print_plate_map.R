# Function print.plate.map()
#
# 2021-03-04
# Marine Cambon

#### Description: 
# Print a plate map for visual check and lab notbooks

#### Usage:
# print.plate.map(d, col, txt = NULL, legend.title=NULL, pdf=F, png=F, file.name=NULL)

#### Arguments:
# d                  a data frame containing a variable named `well` with the well identity.
# col                a vector of colors for wells. If provided as a named vector, an automatic legend will be plotted
# txt                a vector of text to be printed in each well (should be short)
# legend.title       a character string for the legend title. If NULL, no title is added
# pdf                a boolean indicating whether to produce a pdf file of the plate map
# pdf                a boolean indicating whether to produce a png file of the plate map
# file.name          a the name of the file produced when pdf and/or png are TRUE

#### Details:
# For a good aspect ratio in Rmarkdown/Quarto document, set the figure dimentions to fig-width: 5 and fig-height: 3
# for 12-well plates, or fig-width: 10 and fig-height: 6 for 96-well plates

#### Example:

# well_id <- expand.grid(LETTERS[1:8], 1:12)
# well_id <- paste0(well_id[,1], well_id[,2])
# d <- data.frame("well"=well_id,
#                 "treat"=c("treatment", "control"))
# colors <- c("treatment"="lightblue", "control"="pink")[d$treat]
# print.plate.map(d, col = colors, txt=d$well)


print.plate.map <- function(d, col, txt = NULL, legend.title=NULL, pdf=F, png=F, file.name=NULL, warnings=T) {
  if(class(d)=="list") stop("Only one plate can be printed at a time, and d is a list. Please provide a data.frame")
  if(any(pdf,png) & is.null(file.name)) stop("Please provide a file name.")
  
  l <- unique(substr(d$well, 1, 1))
  c <- unique(gsub("[A-Z]", "", d$well))
  if(length(d$well)==96) {
    wi <- 10
    he <- 6
  } else {
    wi <- 5
    he <- 3
  }
  
  id <- 0 
  if(pdf) id <- c(id,1)
  if(png) id <- c(id,2)
  for (i in id) {
    if(i==1) pdf(paste0(file.name, ".pdf"), wi,he)
    if(i==2) png(paste0(file.name, ".png"), wi,he, units = "in", res = 300)
    par(bty="n", xpd=T, las=1, mar=c(4,2,2,8),fg="gray30", col.axis="gray30")
    plot(rep(1:length(c), each=length(l)), rep(length(l):1, length(c)), 
         axes=F, xlab="", ylab="",
         pch=21, cex=ifelse(length(l)==96,5,7), col="darkgray", bg=col)
    mtext(rev(l), side = 2, at = 1:8, line = 1, font = 2)
    mtext(1:length(c), side = 3, at = 1:length(c), line = 1, font = 2)
    if(any(grepl("plate", names(d)))) mtext(paste("Plate", d$plate[1]), side=1, line=1)
    
    if(!is.null(txt)) text(rep(1:length(c), each=length(l)), rep(rev(1:length(l)), length(c)), txt)
    if(is.null(names(col))) {
      if(warnings) warning("coul is not a named vector. Automatic legend cannot be plotted")
    } else {
      legend(length(c)+0.5,length(l), pch=21, bg = "white",
             col="gray30",title = legend.title,
             pt.bg=unique(col),
             unique(names(col))
      )
    }
    if(i!=0) dev.off()
  }
}

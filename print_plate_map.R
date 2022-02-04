print.plate.map <- function(d, col, txt = NULL, pdf=F, png=F, file.name=NULL) {
  if(length(unique(d$plate)) > 1) stop("Only one plate can be printed at a time")
  if(any(pdf,png) & is.null(file.name)) stop("Please provide a file name.")
  
  l <- unique(substr(d$well, 1, 1))
  c <- unique(gsub("[A-Z]", "", d$well))
  i <- ifelse(pdf & png, 1, 2)
  while (i < 3) {
    if(i==1 & pdf) pdf(paste0(file.name, ".pdf"), 7,5)
    if(i==2 & png) png(paste0(file.name, ".png"), 7,5, units = "in", res = 300)
    par(bty="n", xpd=T, las=1, mar=c(2,2,2,8),fg="gray30", col.axis="gray30")
    plot(rep(1:length(c), each=length(l)), rep(length(l):1, length(c)), 
         axes=F, xlab="", ylab="",
         pch=21, cex=ifelse(length(l)==96,5,7), col="darkgray", bg=col)
    mtext(rev(l), side = 2, at = 1:8, line = 1)
    mtext(1:length(c), side = 3, at = 1:length(c), line = 1)
    mtext(paste("Plate", d$plate[1]), side=1, line=1)
    if(!is.null(txt)) text(rep(1:length(c), each=length(l)), rep(rev(1:length(l)), length(c)), txt)
    if(is.null(names(col))) {
      warning("coul is not a named vector. Automatic legend cannot be plotted")
    } else {
      legend(length(c)+0.5,length(l), pch=21, bg = "white",
             col="gray30",
             pt.bg=unique(col),
             unique(names(col))
      )
    }
    if(i==1 & pdf) dev.off()
    if(i==2 & png) dev.off()
    i <- i + 1
  }
}

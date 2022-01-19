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
    legend(13,8, pch=21, bg = "white",
           col="gray30",
           pt.bg=unique(col),
           unique(names(col))
           )
    if(i==1 & pdf) dev.off()
    if(i==2 & png) dev.off()
    i <- i + 1
  }
}

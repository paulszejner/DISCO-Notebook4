color.bar_h <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut))/(max-min)
  
  #dev.new(width=1.75, height=5)
  plot(y=c(0,10), x=c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  for (i in 1:(length(lut))) {
    y = (i-1)/scale + min
    rect(y,0,y+1/scale,10, col=lut[i], border=NA)
  }
  #box()
  axis(3, labels =F,ticks, las=1,padj = 1, lwd=0.5)
  axis(1, labels =T ,ticks, las=1,padj = -1, lwd=0.5)
  
}
#function to print sample image

example <- imageinfo4ssd[50,]

category <- (example$categories %>% str_split(pattern = ", "))[[1]]
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

cex <- 4
## Background rectangle: 

pad <- 0.1 #proportion to pad box by
 
img <- image_read(file.path(img_dir, example$file_name))
img <- image_draw(img)
textHeight <- graphics::strheight(category, cex = cex)
textWidth <- graphics::strwidth(category, cex = cex)
for (i in 1:example$cnt) {
  rect(xleft = x_left[i], ybottom = y_bottom[i],
       xright = x_right[i], ytop = y_top[i],
       border = "white", lwd = 5)
 rect(xleft = as.integer(x_right[i]) - (textWidth[i] * (1 + pad)), 
     ybottom = as.integer(y_top[i]) - (textHeight[i] * (1 + pad)), 
     xright = as.integer(x_right[i]) + (textWidth[i] * pad), 
     ytop = as.integer(y_top[i]) + (textHeight[i] * pad),
     col = "white", border = NA) 
 text(x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = category[i], 
    cex = cex, 
    adj = c(1,1), 
    col = "black", vfont = c("sans serif", "bold"))
}

dev.off()
print(img)




#function to print sample image


example <- imageinfo4ssd[50,]

category <- (example$categories %>% str_split(pattern = ", "))[[1]]
name <- (example$name %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

img <- image_read(file.path(img_dir, example$file_name))
img <- image_draw(img)
for (i in 1:example$cnt) {
  rect(xleft = x_left[i], ybottom = y_bottom[i],
       xright = x_right[i], ytop = y_top[i],
       border = "white", lwd = 5)
  text(
    x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = category[i], 
    cex = 4, adj = c(1,0), col = "white")
}
dev.off()
print(img)



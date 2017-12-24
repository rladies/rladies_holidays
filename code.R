# save all images of cakes&co, selected by hand
links <- c("https://pbs.twimg.com/media/DRK_e7iWkAAjdyB.jpg",
            "https://pbs.twimg.com/media/DMvXXopWkAAHAHp.jpg",
            "https://pbs.twimg.com/media/DMvXX4lWAAAxceh.jpg",
            "https://pbs.twimg.com/media/DMvXYvvW0AAHNc9.jpg",
            "https://pbs.twimg.com/media/DMvXa7TW4AE4-56.jpg",
            "https://pbs.twimg.com/media/DQ-MOeiW0AEnDSt.jpg",
            "https://pbs.twimg.com/media/DM98w3-WkAEjsKT.jpg",
            "https://pbs.twimg.com/media/DOjT41eX4AAUQrv.jpg",
            "https://pbs.twimg.com/media/DQUM5OlW0AA-Q_n.jpg",
            "https://pbs.twimg.com/media/DM1xeGXW4AA8hLE.jpg",
            "https://pbs.twimg.com/media/DM1R-tIW0AAWoaw.jpg",
            "https://pbs.twimg.com/media/DQPotJKUEAEzlKS.jpg")

library("magrittr")

# to get squared image with a purple border
format_image <- function(image){
  info <- magick::image_info(image)
  
  
  
  direction <- ifelse(info$height > info$width,
                      "height", "width")
  scale_number <- as.numeric(ceiling(info[direction]/500))
  image <- magick::image_scale(image, paste0(info["width"]/scale_number,
                                             "x", 
                                             info["height"]/scale_number))
  newinfo <- magick::image_info(image)
  image <- magick::image_border(image, "#88398A", paste0((500-newinfo$width)/2, "x",
                                            (500-newinfo$height)/2))
  info <- magick::image_info(image)
  # odd numbers out!
  if(info$height/2 != floor(info$height/2)){
    image <- magick::image_crop(image, "0x500+0")
  }
  if(info$width/2 != floor(info$width/2)){
    image <- magick::image_crop(image, "500x0+0")
  }
  image
}

# to save images
save_image <- function(link, name){
  magick::image_read(link) %>%
    format_image() %>%
    magick::image_write(paste0("pics/image", name, ".jpg"))
}

purrr::walk2(links,
           seq_along(links),
           save_image)

# appending cols and then rows
no_rows <- 3
no_cols <- 4

files <- paste0("pics/image", 1:length(links), ".jpg")

make_column <- function(i, files, no_rows){
  magick::image_read(files[(i*no_rows+1):((i+1)*no_rows)]) %>%
    magick::image_append(stack = TRUE) %>%
    magick::image_write(paste0("cols/", i, ".jpg"))
}

purrr::walk(0:(no_cols-1), make_column, files = files,
     no_rows = no_rows)

magick::image_read(dir("cols/", full.names = TRUE)) %>%
  magick::image_append(stack = FALSE) %>%
  magick::image_write("pastriRchy.jpg")
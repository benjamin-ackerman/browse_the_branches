rating_modified <- function (gt_object, column, max_rating = 5, ..., color = "orange", 
          icon = "star") {
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% 
              class(gt_object))
  text_transform(gt_object, locations = cells_body(columns = {
    {
      column
    }
  }), fn = function(x) {
    num_x <- suppressWarnings(as.numeric(x))
    lapply(X = num_x, FUN = function(rating) {
      if (gtExtras:::is_blank(rating) || rating %in% c(NA, "NA", 
                                            "")) {
        return(gt::html("&nbsp;"))
      }
      full_fill_rating <- floor(rating)
      partial_fill_rating <- ceiling(rating)
      # rounded_rating <- floor(rating + 0.5)
      stars <- lapply(seq_len(partial_fill_rating), function(i) {
        if (i <= full_fill_rating) {
          fontawesome::fa(icon, fill = color, height = "20px", 
                          a11y = "sem", prefer_type = "solid")
        } else if(i == partial_fill_rating){
          fontawesome::fa(paste0(icon,"-half-stroke"), fill = color, height = "20px", 
                          a11y = "sem", prefer_type = "solid")
        } else{           
          fontawesome::fa(icon, fill = "grey", height = "20px", 
                          a11y = "sem")
        }
      })
      label <- sprintf("%s out of %s", rating, max_rating)
      div_out <- htmltools::div(title = label, `aria-label` = label, 
                                role = "img", c(stars, glue::glue("({rating})")), style = "padding:0px")
      as.character(div_out) %>% gt::html()
    })
  }) %>% cols_align(align = "left", columns = {
    {
      column
    }
  })
}

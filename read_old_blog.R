posts <- list.files("../sites/eringrand.github.io.raw/_posts/", full.names = TRUE)

library(tidyverse)
posts_raw <- data.frame(post_loc = posts,
                post_name = basename(posts)
                ) |> 
  rowwise() |> 
  mutate(post = list(data.frame(txt = readLines(post_loc)))) |> 
  filter(! post_name %in% c("2015-03-12-tabforacause.md", 
                          "2015-03-03-readlistfeb2015.md",
                          "2015-04-01-suggestionsfrommarch.md",
                          "2015-04-01-aprilreading.md",
                          "2015-05-22-profname.md"
                          )
         )

posts_info <- posts_raw |> 
  mutate(date = str_sub(post_name, 1, 10),
         author = "Erin Grand"
         ) |> 
  mutate(title = filter(post, str_detect(txt, "title:")) |> pull(txt),
         title = str_remove(title, "title:") |> 
           str_remove_all("[[:punct:]]") |> 
           str_trim(),
         categories = filter(post, str_detect(txt, "tags:")) |> 
                             pull(txt) |> 
                             str_remove("tags:") |> 
                             str_remove_all("[[:punct:]]") |> 
                             str_trim() |> 
                             str_c(collapse = ", "),
         ) 

fill_between <- function(x) {
  x_log = str_detect(x, "---")
  
  # Find the indices of the first and last TRUE
  first_true_index <- which(x_log)[1]
  last_true_index <- which(x_log)[2]

  # Create a range of indices to fill
  indices_to_fill <- first_true_index:last_true_index

  # Set all values within this range to TRUE
  return(x[-indices_to_fill])
}


cat_clean <- posts_info |> 
  select(title, categories) |> 
  mutate(categories = str_replace_all(categories, " ", ","),
         categories = str_split(categories, ",")
  ) |> 
  unnest(categories) |> 
  filter(categories != "") |> 
  mutate(categories = case_when(categories %in% c("R", "rstats", "r") ~ "R",
                                categories %in% c("data", "science") ~ "data science",
                                categories %in% c("grad") ~ "grad school",
                                TRUE ~ categories
  )
  ) |> 
  nest_by(title) |> 
  mutate(categories = pull(data, categories) |> str_c(collapse = ", ")) |> 
  select(-data)


posts_all <- posts_info |> 
  select(-categories) |> 
  left_join(cat_clean) |> 
  mutate(post_txt = list(pull(post, txt) |> 
                       fill_between() %>% 
                        data.frame(x = .) 
                       ),
         yml_txt = (con = glue::glue("---
                                     title: {title}
                                     author: {author}
                                     date: {date}
                                     categories: [{categories}]
                                     image: 'image.jpg'
                                     ---")),
         yml_txt = list(readLines(textConnection(yml_txt))
                         %>%
                           data.frame(x = .)
                         ),
         new_post_txt = list(bind_rows(yml_txt, post_txt) |> pull(x)),
         dir_title = janitor::make_clean_names(title)
         ) 



walk(pull(posts_all, dir_title), ~dir.create(glue::glue("posts/{.x}")))
walk2(pull(posts_all, dir_title), pull(posts_all, new_post_txt), ~writeLines(.y, glue::glue("posts/{.x}/index.qmd")))

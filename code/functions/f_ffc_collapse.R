library(fs)
library(purrr)
library(stringr)

# need function to read in and collapse different ffc outputs
ffc_collapse <- function(datatype, fdir){
  datatype = datatype
  csv_list = fs::dir_ls(path = fdir, regexp = datatype)
  csv_names = fs::path_file(csv_list) %>% fs::path_ext_remove()
  gage_ids = str_extract(csv_names, '([0-9])+')
  # read in all
  df <- purrr::map(csv_list, ~read_csv(.x)) %>%
    map2_df(gage_ids, ~mutate(.x,gageid=.y))

}

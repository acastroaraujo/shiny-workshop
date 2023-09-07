
custom_plot <- function(y_var, country_list, data) {
  
  y_var <- sym(y_var)  
  
  data |> 
    filter(iso3c %in% country_list) |> 
    hchart(
      type = "line", 
      mapping = hcaes(year, !!y_var, group = iso3c, name = country)
    )
  
  
}

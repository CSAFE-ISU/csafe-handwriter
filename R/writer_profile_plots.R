library(dplyr)
library(handwriter)
library(handwriterRF)

wps0003 <- train %>% dplyr::filter(docname == "w0003_s03_pLND_r01")
wps0005 <- train %>% dplyr::filter(docname == "w0005_s02_pLND_r03")
combo <- rbind(wps0003, wps0005)

plot_profiles <- function(profiles, color_by = "docname") {
  profiles <- profiles %>% 
    tidyr::pivot_longer(cols = -tidyselect::any_of(c("docname", 
                                                     "writer", "doc", "total_graphs")), names_to = "cluster", 
                        values_to = "value") %>% 
    dplyr::mutate(docname = factor(docname), 
                  cluster = as.integer(stringr::str_replace(cluster, "cluster", 
                                                            "")))
  measure <- ifelse(max(profiles$value) > 1, "counts", "rates")
  colnames(profiles)[colnames(profiles) == "value"] <- measure
  p <- profiles %>% 
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = .data[[measure]], 
                                 group = docname, 
                                 color = .data[[color_by]])) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point() + 
    ggplot2::labs(color = "Document") +
    ggplot2::theme_bw()
  return(p)
}


plot_profiles(combo)
ggplot2::ggsave("www/writer_profiles.png", width = 6.5, height = 3, units = c("in"))

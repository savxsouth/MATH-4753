#' Project 1 - myddt()
#'
#' @param df RIVER, MILE, SPECIES, LENGTH, WEIGHT, DDT
#' @param species CCATFISH, SMBUFFALO, LMBASS
#'
#' @return A plot of Length vs Weight for a Species of Fish
#' @export
#'
#' @examples
#' myddt(df = ddt, SPECIES  = "CCATFISH")
myddt <- function(df, species){
  library(dplyr)
  library(ggplot2)
  library(Intro2R)
  df = ddt
  print(df)
  tab = table(df$RIVER) / length(df$RIVER)
  print(tab)
  df1 <- df %>% filter(SPECIES==species)
  print(df1)
  g <- ggplot(df1, aes_string(x = "WEIGHT", y = "LENGTH")) +
    geom_point(aes_string(color = "RIVER")) +
    geom_smooth(formula = y~x + I(x^2), method = "lm") +
    ggtitle("Savannah J Southward")
  print(g)

  if (species == "CCATFISH"){
    write.csv(x = df1, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  else if (species == "SMBUFFALO"){
    write.csv(x = df1, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  else{
    write.csv(x = df1, "LvsWforLMBASS.csv", row.names = FALSE)
  }

}

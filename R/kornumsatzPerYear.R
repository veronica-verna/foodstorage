#' @title calculate the quantities of the products per year.
#' @description 
#' from the data kornumsatz, which has the columns "Tag", "Menge", "Produkt" and the data productInfo, with the columns "Produkte_App", "Produkte_Zusammenfassung", the kornumsatz (quantity of products) for every product and year. 
#' @export


kornumsatz_perYear <- function(kornumsatz, productInfo){
  kornumsatz_new <- kornumsatz %>% 
    left_join(productInfo[, c("Produkte_App", "Produkte_Zusammenfassung")], by=c("Produkt" = "Produkte_App")) %>% 
    mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
    filter(Menge > 0) %>% 
    mutate(Jahr = year(Tag)) %>% 
    group_by(Produkte_Zusammenfassung, Jahr) %>% 
    summarise(Umsatz = sum(Menge))
  return(kornumsatz_new)
}
#year(kornumsatz$Tag)
## pro Jahr
## nur positive Mengen!

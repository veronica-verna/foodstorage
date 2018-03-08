# totalDistances muss eingelesen sein
#--> run generateGeoData


meanDists <- totalDistances %>% 
  group_by(Produktgruppe) %>% 
  summarise(avgDistance = mean(Gesamtentfernung, na.rm=T))


ggplot(meanDists, aes(Produktgruppe, avgDistance, fill= Produktgruppe)) + geom_bar(stat = "identity")


###################################### MIRI:

totDist <- totalDistances %>% 
  mutate(numDist = ntile(Gesamtentfernung, 20))

ggplot(totDist, aes(numDist, Gesamtentfernung)) + geom_point()

turnOver2016 <- totalDistances %>%
  group_by(Produktgruppe)%>%
  summarise(Menge = sum(turnover2016, na.rm = T), Distanz = mean(Gesamtentfernung, na.rm = T))

View(totalDistances)
# ist nicht das was wir wollten, aber auch interessant
ggplot(turnOver2016, aes(Distanz, Menge)) + 
  geom_point(aes(color = Produktgruppe, size = 3)) +
  scale_size(guide = "none") +
  ggtitle("Umsatz 2016")

# Counts statt Menge --> Bezug zum Bewicht fehlt
ggplot(totalDistances, aes(x = Gesamtentfernung, fill = Produktgruppe)) + 
  geom_bar(color = "black", binwidth = 1500) 

#position und bins werden nicht gelesen
ggplot(totalDistances, aes(x = Gesamtentfernung, y = turnover2016, fill = Produktgruppe, color = Produktgruppe)) + 
  geom_bar(position = "stack",stat = "identity") 

ggplot(totalDistances, aes(Gesamtentfernung,  fill = Produktgruppe)) + geom_histogram(binwidth = 1000)

#Problem: Zwei stetige Variablen bildet man eigentlich nicht als Barplot ab, wir könnten durch breaks aus der Distanz "künstlich" eine diskrete Variable machen

# Datensatz totalDistances muss geladen sein mit Spalten turnover und Gesamtentfernung

newtotalDistances <- createDistanceCategory(totalDistances)
head(newtotalDistances$Kategorie)

# Finally!!
positions <- c("0-100", "100-200", "200-400", "400-800", "800-1600", "1600-3200", "3200-6400", "6400-12800", "NA")

ggplot(newtotalDistances, aes(x = Kategorie, y = avg.turnover, fill = Produktgruppe)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = positions)+
  labs(title = "Konsumierte Produkte nach Distanz", 
       y = "Umsatz in kg bzw. L",
       x = "Distanz [km]")


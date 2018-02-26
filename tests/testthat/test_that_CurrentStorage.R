# test currentStroage
test_that(
  "currentStorage is doing well", {
    #### load dataset ####
    path <- system.file("data", package = "foodstorage")
    files <- list.files(path)
    # filter all backups (files which end up with .BAK)
    backup <- files[which(stringr::str_detect(files, ".BAK$"))]
    appDB <- dbConnect(SQLite(), file.path(path, backup))
    originalData <- dbGetQuery(appDB, '
SELECT strftime(\'%d/%m/%Y\',transactions.start/1000,\'unixepoch\') AS Tag,
      ROUND(SUM(transaction_products.quantity), 3) AS Menge, 
      transaction_products.unit AS Einheit,
      ROUND(transaction_products.price, 2) AS Preis, 
      transaction_products.title AS Produkt,
      ROUND(SUM(transaction_products.quantity * transaction_products.price), 3) AS Summe
FROM transaction_products 
LEFT JOIN transactions
      ON transactions._id = transaction_products.transaction_id 
WHERE transactions.status IS \'final\' AND transaction_products.account_guid IS \'lager\'
GROUP BY Tag, Produkt, Preis
ORDER BY transactions.start
'
    )
    dbDisconnect(appDB)
    
    kornDB <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
    kornInfo <- dbConnect(SQLite(), file.path(path, kornDB))
    originalInfos <- dbReadTable(kornInfo, "productInfo")
    dbDisconnect(kornInfo)
    
    kornumsatz <- startupSettings(originalData, originalInfos)
    
    Bode <- sort(c(
      "Basilikum", "Basitom", "Basmati Weiß", "Blaumohn", "Buchweizenmehl", 
      "Couscous", "Currychini", "Kichererbsen", "Kräuter der Provence", "Mepfel",
      "Oregano", "Paprika Edelsüß", "Risottoreis", "Rosmarin", 
      "Rote Beete Meerettich", "Linsen Rot", "Schwarzkümmel", "Sendi", "Thymian", 
      "Zwiebelschmelz"
    ))
    
    #### test currentStorage ####
    testBode <- currentStorage(kornumsatz, group = Bode) %>%
      arrange(Produkt_Zusammenfassung)
    expect_equal(
      unique(testBode$Produkt_Zusammenfassung),
      Bode
    )
    testBode <- testBode %>%
      count(Produkt_Zusammenfassung)
    expect_equal(
      unique(testBode$n),
      1
    )
    
    #### test without group input
    testData <- currentStorage(kornumsatz) %>%
      count(Produkt_Zusammenfassung)
    expect_equal(
      unique(testData$n),
      1
    )
  }
)
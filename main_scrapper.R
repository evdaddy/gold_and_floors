library(rvest)
library(xml2)
library(proto)
library(stringi)
library(stringr)
library(gsubfn)
library(dplyr)
library(prodlim)
library(lubridate)
library(ggplot2)

url_old <- "https://realty.yandex.ru/otsenka-kvartiry-po-adresu-onlayn/%D0%9A%D0%BE%D0%BC%D0%BF%D0%BE%D0%B7%D0%B8%D1%82%D0%BE%D1%80%D1%81%D0%BA%D0%B0%D1%8F%20%D1%83%D0%BB%D0%B8%D1%86%D0%B0%2C%2017/kupit/kvartira/1/?from=main_menu"
page_old <- read_html(url_old)

page_number <-  html_text(html_nodes(page_old, css = '.Button__text'))
page_number <- as.list(gsub("[^0-9,]", "", page_number))
for (i in rev(page_number))
{
  if (is.na(as.numeric(i)) == 0)
  {
    page_number <- as.numeric(i)
    break
  }
}
page_number
pages <- sapply(seq(1:page_number), function(n){
  list_page <- paste0("https://realty.yandex.ru/otsenka-kvartiry-po-adresu-onlayn/%D0%9A%D0%BE%D0%BC%D0%BF%D0%BE%D0%B7%D0%B8%D1%82%D0%BE%D1%80%D1%81%D0%BA%D0%B0%D1%8F%20%D1%83%D0%BB%D0%B8%D1%86%D0%B0%2C%2017/kupit/kvartira/", n, "/?from=main_menu")
})
pages

old_tmp <- lapply(pages, function(u){
  page_old <-  read_html(u)
  rows <- html_text(html_nodes(page_old, css = '.OffersArchiveSearchOffers__row'))
  dates <- strapplyc(rows, "\\d+.\\d+.\\d+", simplify = TRUE)[-1]
  dates_old <- vector()
  for (i in 1:length(dates))
  {
    dates_old <- append(dates_old, rev(as.Date.character(x = dates[[i]], format = "%d.%m.%Y"))[1])
  }
  
  tst2 <- html_text(html_nodes(html_nodes(page_old, css = '.OffersArchiveSearchOffers__row'), 'span'))
  tst3 <- gsub("[^0-9,]", "", matrix(unlist(tst2), byrow=TRUE, ncol=5))
  area <- as.numeric(str_replace(str_sub(tst3[,1], start = 1, end = -3) , ",", ".") )
  rooms <- as.numeric(str_replace(str_sub(tst3[,1], start = -1) , ",", ".") )
  price <- as.numeric(str_replace(tst3[,4], ",", "."))
  price <-  price * 1000000
  ads_tmp <-data.frame(Площадь = area,
                                  Комнаты = rooms,
                                  Стоимость = price,
                                  Стоимость_метра = as.numeric(tst3[,5]),
                                  Date = dates_old )
  ads_tmp <- ads_tmp[complete.cases(ads_tmp),]
})

old_tmp
old_ads <- data.frame()
for (i in old_tmp)
{
  old_ads <- rbind(old_ads, i)
}
old_ads <- old_ads[order(old_ads[,5]),]
gold <- read.csv(file = "/Users/ivantrushin/Dev/R/курсовая/floors/gold_rate.csv", sep = ";")
gold[,1] <-  as.Date.character(x = gold[,1], format = "%d.%m.%Y")
for (column in 2:ncol(gold))
{
  gold[,column] <- gsub(",",".",gold[,column])
  gold[,column] <- as.numeric(gold[,column])
}

old_ads <- merge(old_ads, gold, by = "Date", all.x = T)
for (rows in 1:nrow(old_ads))
{
  if(is.na(old_ads[rows,6]))
  {
    old_ads[rows, 6:9] <- gold[which.min(abs(old_ads[rows,1] - gold$Date)),2:5]
  }
}

old_ads$price_gold <- old_ads$Стоимость / old_ads$Gold
old_ads$square_gold <- old_ads$Стоимость_метра / old_ads$Gold
old_ads$price_silver <- old_ads$Стоимость / old_ads$Silver
old_ads$square_silver <- old_ads$Стоимость_метра / old_ads$Silver
old_ads$price_platinum <- old_ads$Стоимость / old_ads$Platinum
old_ads$square_platinum <- old_ads$Стоимость_метра / old_ads$Platinum
old_ads$price_palladium <- old_ads$Стоимость / old_ads$Palladium
old_ads$square_palladium <- old_ads$Стоимость_метра / old_ads$Palladium
old_ads <- old_ads[,-(6:9)]

ggplot(data = old_ads, aes(x = Date, y = price_gold)) + geom_point() + geom_smooth() + labs(title = "Цена квартир в золотом эквиваленте", x = "Даты", y = "Граммы")
сggplot(data = old_ads, aes(x = Date, y = price_silver)) + geom_point() + geom_smooth() + labs(title = "Цена квадратного метра в сербре", x = "Даты", y = "Граммы")
ggplot(data = old_ads, aes(x = Date, y = price_platinum)) + geom_point() + geom_smooth() + labs(title = "Цена квадратного метра в платине", x = "Даты", y = "Граммы")
ggplot(data = old_ads, aes(x = Date, y = price_palladium)) + geom_point() + geom_smooth() + labs(title = "Цена квадратного метра в палладии", x = "Даты", y = "Граммы")
ggplot(data = gold, aes(x = Date, y = Gold)) + geom_point() + geom_smooth() + labs(title = "Цена золота", x = "Даты", y = "Стоимость грамма")
ggplot(data = old_ads, aes(x = Date, y = Стоимость)) + geom_point() + geom_smooth() + labs(title = "Цена квартир", x = "Даты", y = "Стоимость")
ggplot(data = old_ads, aes(x = Date, y = Стоимость)) + geom_point() + geom_smooth() + labs(title = "Цена квартир", x = "Даты", y = "Стоимость")
# linear <- lm(price_gold~Date, data = old_ads)
# summary(linear)
# ggplot() + geom_point(data = old_ads, aes(x = Date, y = Стоимость)) + geom_smooth(data = old_ads, aes(x = Date, y = Стоимость), colour = "Red")
# 
# ggplot() +  geom_point(aes(x = gold[11:1809,1], y = gold[11:1809,2])) + geom_smooth(aes(x = gold[11:1809,1], y = gold[11:1809,2]), colour = "Yellow")


# backpack <- function(volumes, values)
# {
#   all_values <- data.frame()
#   for (a in 0:1)
#   {
#     for (b in 0:1)
#     {
#       for (c in 0:1)
#       {
#         for (d in 0:1)
#         {
#           for (e in 0:1)
#           {
#             tmp_value <- values[1]*a + values[2]*b + values[3]*c + values[4]*d + values[5]*e
#             tmp_volume <- volumes[1]*a + volumes[2]*b + volumes[3]*c + volumes[4]*d + volumes[5]*e
#             if(tmp_volume <= 15)
#             {
#               all_values[nrow(all_values) + 1,1:7] <-  c(tmp_value,tmp_volume,a,b,c,d,e)
#             }
#           }
#         }
#       }
#     }
#   }
#   names <-c('value','volume',1,2,3,4,5)
#   colnames(all_values) <- names
#   tmp_value <- values * as.vector(all_values[which.max(all_values$value),3:7])
#   tmp_volume <- volumes * as.vector(all_values[which.max(all_values$value),3:7])
#   return(list(values = tmp_value,volumes = tmp_volume))
# }
# backpack(c(1,3,5,7,9),c(10,25,45,60,89))
library(foreach)
backpack <- function(volumes, values)
{
  all_values <- data.frame()
  tmp_value <- foreach(a = 0:1, b = 0:1, c = 0:1, d = 0:1, e = 0:1, .combine='c') %do%
    {
     print(values[1]*a + values[2]*b + values[3]*c + values[4]*d + values[5]*e)
    }
  tmp_volume <- foreach(a = 0:1, b = 0:1, c = 0:1, d = 0:1, e = 0:1, .combine='c') %do%
    {
    volumes[1]*a + volumes[2]*b + volumes[3]*c + volumes[4]*d + volumes[5]*e 
    }
  if(tmp_volume <= 15)
  {
    all_values[nrow(all_values) + 1,1:7] <-  c(tmp_value,tmp_volume,a,b,c,d,e)
  }
  names <-c('value','volume',1,2,3,4,5)
  colnames(all_values) <- names
  tmp_value <- values * as.vector(all_values[which.max(all_values$value),3:7])
  tmp_volume <- volumes * as.vector(all_values[which.max(all_values$value),3:7])
  return(list(values = tmp_value,volumes = tmp_volume))
}
backpack(c(1,3,5,7,9),c(10,25,45,60,89))

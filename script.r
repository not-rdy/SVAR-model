library(XML)  # parsing XML files
library(RCurl)  # filling forms
library(readxl)

#Государственные расходы можно подразделить на следующие группы:
#-   военные;
#-   экономические;
#-   на социальные нужды;
#-   на внешнеполитическую деятельность;
#-   на содержание аппарата управления.


setwd("/home/rustem/Документы/research/budget_pulse/data_budget_report")
df <- data.frame("file_name" = rep(NA, 119), "col_length" = rep(NA, 119))

a <- 1
i <- 2011
r <- 1
while(i <= 2020 | a <= 12){
  if( (paste0(as.character(a), "_", as.character(i), ".xlsx") != "7_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "8_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "10_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "12_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "1_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "2_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "3_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "4_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "5_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "6_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "7_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "9_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "10_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "11_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "12_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "1_2016.xlsx")){
    df2 <- read_excel(paste0(as.character(a), "_", as.character(i), ".xls"))
    df[r, 1] <- paste0(as.character(a), "_", as.character(i), ".xls")
    df[r, 2] <- ncol(df2) 
  }else{
    df2 <- read_excel(paste0(as.character(a), "_", as.character(i), ".xlsx"))
    df[r, 1] <- paste0(as.character(a), "_", as.character(i), ".xlsx")
    df[r, 2] <- ncol(df2)
  }
  
  r <- r + 1
  a <- a + 1
  
  if(a > 12){
    i <- i + 1
    a <- 1
  }
}


remove(df, df2, a, i, r)


# Загоняем все файлы по бюджету в два df (df_expenses, df_revenue) 
# и смотрим на уникальные значения статей.

a <- 2
i <- 2011

df <- read_excel("1_2011.xls")
colnames(df) <- c("c_1", "c_2", "c_3", "c_4")
df$date <- as.Date("01012011", "%d%m%Y")
df_revenue <- df[which(df$c_1 == "Д О Х О Д Ы"):
                   (which(df$c_1 == "Р А С Х О Д Ы") - 1), ]

df_expenses <- df[which(df$c_1 == "Р А С Х О Д Ы"):nrow(df), ]

deficit_surplus <- df[df$c_1 == "Д Е Ф И Ц И Т / П Р О Ф И Ц И Т", 
                      c("c_2", "c_3", "date")]

while(i <= 2020 | a <= 12){
  if( (paste0(as.character(a), "_", as.character(i), ".xlsx") != "7_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "8_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "10_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "12_2014.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "1_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "2_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "3_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "4_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "5_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "6_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "7_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "9_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "10_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "11_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "12_2015.xlsx") &
      (paste0(as.character(a), "_", as.character(i), ".xlsx") != "1_2016.xlsx")){
    
    df <- read_excel(paste0(as.character(a), "_", as.character(i), ".xls"))
    colnames(df) <- c("c_1", "c_2", "c_3", "c_4")
    
    if(a < 10){
      df$date <- as.Date(paste0("01", paste0("0", as.character(a)), as.character(i)),
                         "%d%m%Y")
    }else{
      df$date <- as.Date(paste0("01", as.character(a), as.character(i)),
                         "%d%m%Y")
    }
    
    df_1 <- df[which(df$c_1 == "Д О Х О Д Ы"):
                       (which(df$c_1 == "Р А С Х О Д Ы") - 1), ]
    df_revenue <- rbind(df_revenue, df_1)
    
    
    df_2 <- df[which(df$c_1 == "Р А С Х О Д Ы"):nrow(df), ]
    df_expenses <- rbind(df_expenses, df_2)
    
    df_3 <- df[df$c_1 == "Д Е Ф И Ц И Т / П Р О Ф И Ц И Т", c("c_2", "c_3", "date")]
    deficit_surplus <- rbind(deficit_surplus, df_3)
  }else{
    df <- read_excel(paste0(as.character(a), "_", as.character(i), ".xlsx"))
    colnames(df) <- c("c_1", "c_2", "c_3", "c_4")
    
    if(a < 10){
      df$date <- as.Date(paste0("01", paste0("0", as.character(a)), as.character(i)),
                         "%d%m%Y")
    }else{
      df$date <- as.Date(paste0("01", as.character(a), as.character(i)),
                         "%d%m%Y")
    }
    
    df_1 <- df[which(df$c_1 == "Д О Х О Д Ы"):
               (which(df$c_1 == "Р А С Х О Д Ы") - 1), ]
    df_revenue <- rbind(df_revenue, df_1)
    
    
    df_2 <- df[which(df$c_1 == "Р А С Х О Д Ы"):nrow(df), ]
    df_expenses <- rbind(df_expenses, df_2)
    
    df_3 <- df[df$c_1 == "Д Е Ф И Ц И Т / П Р О Ф И Ц И Т", c("c_2", "c_3", "date")]
    deficit_surplus <- rbind(deficit_surplus, df_3)
  }
  
  a <- a + 1
  
  if(a > 12){
    i <- i + 1
    a <- 1
  }
}

# удаляем значения дефицит/профицит из df_expenses
df_expenses <- df_expenses[df_expenses$c_1 != "Д Е Ф И Ц И Т / П Р О Ф И Ц И Т", ]

# смотрим на уникальные значения статей df_expenses и df_revenue
length(unique(df_expenses$c_1))

length(unique(df_revenue$c_1))

# убираем пропуски в расходах
library(dplyr)
df_expenses <- df_expenses[df_expenses$c_1 != "Р А С Х О Д Ы", ]  # удаляем строку РАСХОДЫ

na <- df_expenses[is.na(df_expenses$c_2), ]# смотрим на соседние столбцы при NA в c_2
df_expenses$c_2 <- as.numeric(df_expenses$c_2)
df_expenses$c_2[is.na(df_expenses$c_2)] <- 0

na <- df_expenses[is.na(df_expenses$c_3), ]# смотрим на соседние столбцы при NA в c_3
df_expenses$c_3 <- as.numeric(df_expenses$c_3)
df_expenses$c_3[is.na(df_expenses$c_3)] <- 0

na <- df_expenses[is.na(df_expenses$c_4), ]# смотрим на соседние столбцы при NA в c_4
df_expenses$c_4 <- as.numeric(df_expenses$c_4)
df_expenses$c_4[df_expenses$c_3 == 0 & df_expenses$c_2 == 0] <- 0
df_expenses$c_4[df_expenses$c_3 == 0 & df_expenses$c_2 != 0] <- 0

i <- 1
for(i in 1:3965){
  if(df_expenses$c_3[i] != 0 & df_expenses$c_2[i] != 0){
    df_expenses$c_4[i] <-  df_expenses$c_3[i] / df_expenses$c_2[i] * 100
  }
}

df_expenses$c_4[which(is.na(df_expenses$c_4))] <- Inf

summary(df_expenses)
str(df_expenses)
sum(is.na(df_expenses))


# убираем пропуски в доходах
df_revenue <- df_revenue[df_revenue$c_1 != "Д О Х О Д Ы", ]  # удаляем строку ДОХОДЫ
df_revenue <- df_revenue[!is.na(df_revenue$c_1), ] # удаляем строки с NA в c_1
df_revenue$c_2[is.na(df_revenue$c_2)] <- 0 # присваиваем нули всем NA в c_2
df_revenue$c_3[is.na(df_revenue$c_3)] <- 0 # присваиваем нули всем NA в c_3

for(i in 1:2096){
  if(df_revenue$c_2[i] == 0 & df_revenue$c_3[i] == 0){
    df_revenue$c_4[i] <- 0
  }
  if(df_revenue$c_2[i] == 0 & df_revenue$c_3[i] > 0){
    df_revenue$c_4[i] <- Inf
  }
  if(df_revenue$c_2[i] > 0 & df_revenue$c_3[i] == 0){
    df_revenue$c_4[i] <- 0
  }
}

df_revenue$c_2 <- as.numeric(df_revenue$c_2) # переводим все столбцы в числовой тип
df_revenue$c_3 <- as.numeric(df_revenue$c_3)
df_revenue$c_4 <- as.numeric(df_revenue$c_4)


df_revenue$c_4[is.na(df_revenue$c_4)] <- df_revenue$c_3[is.na(df_revenue$c_4)] /
  df_revenue$c_2[is.na(df_revenue$c_4)] * 100 # заполняем NA в с_4





# c_1 - Наименование статьи
# c_2 - План с учетом изменений на год, тыс.руб.
# c_3 - Отчет, тыс.руб. (нарастающий итог)
# c_4 - % исполнения


# посчитаем точечные ежемесячные данные на 01 число каждого месяца
df_expenses_month <- data.frame("Expenses_fact" = rep(NA, 120),
                                "date" = rep(NA, 120))
df_expenses_month$date <- as.Date(df_expenses_month$date)

df_expenses_month$Expenses_fact[1] <- 0
df_expenses_month$date[1] <- "2011-01-01"
df_expenses_month$Expenses_fact[2] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                        df_expenses$date == "2011-02-01"]
df_expenses_month$date[2] <- "2011-02-01"

m <- 3
y <- 2011
for(i in 3:120){
  if(m == 1){
    df_expenses_month$Expenses_fact[i] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                            df_expenses$date == paste0(y, "-", "01", "-", "01")] -
      df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                        df_expenses$date == paste0(y - 1, "-", "12", "-", "01")]
    df_expenses_month$date[i] <- paste0(y, "-", "0", m, "-", "01")
  }
  
  if(m == 2){
    df_expenses_month$Expenses_fact[i] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                            df_expenses$date == paste0(y, "-", "0", m, "-", "01")]
    df_expenses_month$date[i] <- paste0(y, "-", "0", m, "-", "01")
  }
  
  if(m >= 3 & m <= 9){
    df_expenses_month$Expenses_fact[i] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                            df_expenses$date == paste0(y, "-", "0", m, "-", "01")] -
      df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                        df_expenses$date == paste0(y, "-", "0", m - 1, "-", "01")]
    df_expenses_month$date[i] <- paste0(y, "-", "0", m, "-", "01")
  }
  
  if(m == 10){
    df_expenses_month$Expenses_fact[i] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                            df_expenses$date == paste0(y, "-", m, "-", "01")] -
      df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                        df_expenses$date == paste0(y, "-", "0", m - 1, "-", "01")]
    df_expenses_month$date[i] <- paste0(y, "-", m, "-", "01")
  }
  
  if(m > 10){
    df_expenses_month$Expenses_fact[i] <- df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                                                            df_expenses$date == paste0(y, "-", m, "-", "01")] -
      df_expenses$c_3[df_expenses$c_1 == "ИТОГО расходов" & 
                        df_expenses$date == paste0(y, "-", m - 1, "-", "01")]
    df_expenses_month$date[i] <- paste0(y, "-", m, "-", "01")
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
    y <- y + 1
  }
}



# 1) Приводим расходы к сопоставимому виду по методике А.А. Френкеля
#(расписать подробнее про все индексы. Как рассчитываются и т.д. Взять с сайта росстата)

setwd("/home/rustem/Документы/research/budget_pulse/indexes")

index_cap_constr <- read_excel("data_index_capital_construction.xls")
index_cargotransp <- read_excel("data_index_cargotransp.xls")
index_purch_res <- read_excel("data_index_industry_purchased_resources.xls")
index_price <- read_excel("data_index_price.xls")
index_price_producers <- read_excel("data_index_price_producer.xls")
index_overdue_debt <- read_excel("data_index_overdue_debt.xls")

# Удалим лишние значения и посмотрим на графики. 
# Смотрим на график кредиторской задолженности и средней цены производителей
library(ggplot2)
options(scipen = 999)

# преобразуем в индексы просроченную кредиторскую задолженность
index_overdue_debt$index <- rep(NA, 119)
a <- 1
m <- 0
for(i in 2:119){
  m <- m + 1
  if(m <= 12){
    index_overdue_debt$index[i] <- index_overdue_debt$one[i] / index_overdue_debt$one[a] * 100
    index_overdue_debt$index[i] <-  round(index_overdue_debt$index[i], 2)
  }else{
    a <- a + 12
    index_overdue_debt$index[i] <-  index_overdue_debt$one[i] / index_overdue_debt$one[a] * 100
    index_overdue_debt$index[i] <-  round(index_overdue_debt$index[i], 2)
    m <- 0
  }
}

# Преобразуем в индексы среднюю цену производителей
index_price_producers$index <- rep(NA, 121)
a <- 1
m <- 0
for(i in 2:121){
  m <- m + 1
  if(m <= 12){
    index_price_producers$index[i] <- index_price_producers$one[i] / index_price_producers$one[a] * 100
    index_price_producers$index[i] <-  round(index_price_producers$index[i], 2)
  }else{
    a <- a + 12
    index_price_producers$index[i] <-  index_price_producers$one[i] / index_price_producers$one[a] * 100
    index_price_producers$index[i] <-  round(index_price_producers$index[i], 2)
    m <- 0
  }
}

# абсолютный показатель. Задолженность
ggplot(data = index_overdue_debt, aes(x = two, y = one))+
  ggtitle("Абсолютный показатель просроченной КЗ")+
  geom_line()+
  geom_point()

# индекс (к декабрю предыдущего года). Задолженность
ggplot(data = index_overdue_debt, aes(x = two, y = index))+
  ggtitle("Индекс просроченной КЗ")+
  geom_line()+
  geom_point()

# абсолютный показатель. Средняя цена производителей
ggplot(data = index_price_producers, aes(x = two, y = one))+
  ggtitle("Абсолютный показатель. Средняя цена производителей")+
  geom_line()+
  geom_point()

# индекс (к декабрю предыдущего года). Средняя цена производителей
ggplot(data = index_price_producers, aes(x = two, y = index))+
  ggtitle("Индекс. Средняя цена производителей")+
  geom_line()+
  geom_point()


index_overdue_debt <- index_overdue_debt[2:119, ]
index_cap_constr <- index_cap_constr[1:118, ]
index_cargotransp <- index_cargotransp[1:118, ]
index_price <- index_price[1:118, ]
index_purch_res <- index_purch_res[1:118, ]
index_price_producers <- index_price_producers[2:119, ]

# объеденим все индексы в один df и удалим
indexes <- data.frame("date" = index_overdue_debt$two,
                      "overdue_debt" = index_overdue_debt$index,
                      "cap_constr" = index_cap_constr$one,
                      "cargotransp" = index_cargotransp$one,
                      "price" = index_price$one,
                      "purch_res" = index_purch_res$one,
                      "price_producers" = index_price_producers$index)

indexes$purch_res <- round(indexes$purch_res, digits = 2) # округляем

remove(index_cap_constr, index_cargotransp, index_overdue_debt, index_price,
       index_price_producers, index_purch_res, na, dat) # удаляем


# рассчитаем веса для индексов, участвующих при выведении агрегированного индекса инфляции
# а так же сам агрегированный индекс инфляции

# парные коэффициенты детерминации
R_2 <-  cor(indexes[, 2:7])^2

# веса
w_overdue_debt <- (sum(R_2[, 1]) - 1) / (sum(R_2) - 6)
w_cap_constr <- (sum(R_2[, 2]) - 1) / (sum(R_2) - 6)
w_cargotransp <- (sum(R_2[, 3]) - 1) / (sum(R_2) - 6)
w_price <- (sum(R_2[, 4]) - 1) / (sum(R_2) - 6)
w_purch_res <- (sum(R_2[, 5]) - 1) / (sum(R_2) - 6)
w_price_producers <- (sum(R_2[, 6]) - 1) / (sum(R_2) - 6)

# агрегированный индекс инфляции за каждый период
# в конце умножаем не на предыдущее значение k_n а на последний месяц предыдущего года
k_n <- data.frame("k_n" = rep(NA, 118),
                  "date" = indexes$date)

p <- 109
a <- 1
for(i in 118:1){
  if(i == 118){
    k_n$k_n[i] <- 1
  }
  
  if(i < 118 & i >= 109){
    k_n$k_n[i] <- (w_overdue_debt * indexes$overdue_debt[i] +
                      w_cap_constr * indexes$cap_constr[i] +
                      w_cargotransp * indexes$cargotransp[i] +
                      w_price * indexes$price[i] +
                      w_purch_res * indexes$purch_res[i] +
                      w_price_producers * indexes$price_producers[i]) / 100
  }
  
  if(i < 109){
    k_n$k_n[i] <- ((w_overdue_debt * indexes$overdue_debt[i] +
                       w_cap_constr * indexes$cap_constr[i] +
                       w_cargotransp * indexes$cargotransp[i] +
                       w_price * indexes$price[i] +
                       w_purch_res * indexes$purch_res[i] +
                       w_price_producers * indexes$price_producers[i]) / 100) * k_n$k_n[p]
    a <- a + 1
    
    if(a > 12){
      a <- 1
      p <- p - 12
    }
  }
}

# корректируем на агрегированные индексы инфляции
df_expenses_month_comparable <- df_expenses_month[1:118, ]
df_expenses_month_comparable$Expenses_fact <- df_expenses_month_comparable$Expenses_fact * k_n$k_n

# посмотрим на график с исходными ценами и с ценами приведенными к 2020 году
ggplot() +
  geom_line(data = df_expenses_month_comparable, 
            aes(x = date, y = Expenses_fact),
            col = "red") +
  geom_line(data = df_expenses_month,
            aes(x = date, y = Expenses_fact))

remove(df, df_1, df_2, df_3, df2)

# 2) Оборот розничной торговли, млн.руб.

# Загружаем данные (нарастающим итогом)
setwd("/home/rustem/Документы/research/budget_pulse/data_retail_trade")
df <- data.frame("retail_trade" = rep(NA, 120),
                         "date" = rep(NA, 120))
df$date <- as.Date(df$date)

df2 <- readHTMLTable("2011_1_1.htm")

df$retail_trade[1] <- 0
df$date[1] <- "2011-01-01"

# зададим условие поиска строки в которой есть слово Башкортостан
library(tidyverse)
# str_detect(df2$'NULL'$V1, "Башкортостан")

y <- 2011
m <- 1
for(i in 2:120){
  df2 <- readHTMLTable(paste0(y, "_", "1", "_", m, ".htm"))
  
  if(m < 9){
    df$retail_trade[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(m >= 9 & m != 12){
    df$retail_trade[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(m == 12){
    df$retail_trade[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y + 1, "-", "01", "-", "01")
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
    y <- y + 1
  }
}

df$retail_trade <-  gsub(",", ".", df$retail_trade) # меняем запятые на точки
df$retail_trade <- as.numeric(df$retail_trade) # переводим столец в числовой тип

# получаем данные товарооборота за 1 месяц без нарастающего итога
df_r_trade_month <- data.frame("retail_trade" = rep(NA, 120),
                               "date" = df$date)
df_r_trade_month$retail_trade[1] <- 0

m <- 1
for(i in 2:120){
  if(m == 1){
    df_r_trade_month$retail_trade[i] <- df$retail_trade[i]
  }
  
  if(m >= 2){
    df_r_trade_month$retail_trade[i] <- df$retail_trade[i] - df$retail_trade[i - 1]
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
  }
}

# приводим к сопоставимым ценам
df_r_trade_month <- df_r_trade_month[1:118, ]
df_r_trade_month_comparable <- df_r_trade_month
df_r_trade_month_comparable$retail_trade <- df_r_trade_month_comparable$retail_trade * k_n$k_n

# посмотрим на график исходных цен и сопоставимых (красный сопоставимые)
ggplot()+
  geom_line(data = df_r_trade_month_comparable, 
            aes(x = date, y = retail_trade, col = 'red')) +
  geom_line(data = df_r_trade_month,
            aes(x = date, y = retail_trade))



# 3) Объем платных услуг населению, млн.руб.
# Загружаем данные (нарастающим итогом)
setwd("/home/rustem/Документы/research/budget_pulse/data_paid_services")
df <- data.frame("paid_services" = rep(NA, 120),
                 "date" = rep(NA, 120))
df$date <- as.Date(df$date)

df2 <- readHTMLTable("2011_1_1.htm")

df$paid_services[1] <- 0
df$date[1] <- "2011-01-01"


# зададим условие поиска строки в которой есть слово Башкортостан
library(tidyverse)
# str_detect(df2$'NULL'$V1, "Башкортостан")

y <- 2011
m <- 1
for(i in 2:120){
  df2 <- readHTMLTable(paste0(y, "_", "1", "_", m, ".htm"))
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2014_1_6.htm"){
    df$paid_services[i] <- "101536,6"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2014_1_8.htm"){
    df$paid_services[i] <- "140414,8"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2014_1_9.htm"){
    df$paid_services[i] <- "159667,1"
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2015_1_1.htm"){
    df$paid_services[i] <- "18226,4"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2015_1_3.htm"){
    df$paid_services[i] <- "54650,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2015_1_4.htm"){
    df$paid_services[i] <- "73420,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2015_1_7.htm"){
    df$paid_services[i] <- "128501,0"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2015_1_12.htm"){
    df$paid_services[i] <- "232110,5"
    df$date[i] <- paste0(y + 1, "-", "01", "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_1.htm"){
    df$paid_services[i] <- "17934,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_3.htm"){
    df$paid_services[i] <- "57526,0"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_4.htm"){
    df$paid_services[i] <- "75320,1"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_7.htm"){
    df$paid_services[i] <- "133405,1"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_8.htm"){
    df$paid_services[i] <- "155135,7"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_11.htm"){
    df$paid_services[i] <- "218217,2"
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2016_1_12.htm"){
    df$paid_services[i] <- "240436,8"
    df$date[i] <- paste0(y + 1, "-", "01", "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_1.htm"){
    df$paid_services[i] <- "20054,8"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_4.htm"){
    df$paid_services[i] <- "77990,5"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_5.htm"){
    df$paid_services[i] <- "96959,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_6.htm"){
    df$paid_services[i] <- "117059,0"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_7.htm"){
    df$paid_services[i] <- "139492,8"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_8.htm"){
    df$paid_services[i] <- "161137,6"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_9.htm"){
    df$paid_services[i] <- "182068,3"
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_10.htm"){
    df$paid_services[i] <- "203996,2"
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_11.htm"){
    df$paid_services[i] <- "226781,4"
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2017_1_12.htm"){
    df$paid_services[i] <- "249900,2"
    df$date[i] <- paste0(y + 1, "-", "01", "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_1.htm"){
    df$paid_services[i] <- "20617,2"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_2.htm"){
    df$paid_services[i] <- "40215,1"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_3.htm"){
    df$paid_services[i] <- "61125,5"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_4.htm"){
    df$paid_services[i] <- "81027,7"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_5.htm"){
    df$paid_services[i] <- "101182,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(paste0(y, "_", "1", "_", m, ".htm") == "2018_1_6.htm"){
    df$paid_services[i] <- "121971,9"
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(m < 9 & 
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_10.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_2.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_6.htm"){
    df$paid_services[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y, "-", "0", m + 1, "-", "01")
  }
  
  if(m >= 9 & m != 12 & 
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_10.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_2.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_6.htm"){
    df$paid_services[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y, "-", m + 1, "-", "01")
  }
  
  if(m == 12 & 
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2014_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2015_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2016_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_6.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_7.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_8.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_9.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_10.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_11.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2017_1_12.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_1.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_2.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_3.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_4.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_5.htm" &
     paste0(y, "_", "1", "_", m, ".htm") != "2018_1_6.htm"){
    df$paid_services[i] <- df2$'NULL'$V2[str_detect(df2$'NULL'$V1, "Башкортостан")]
    df$date[i] <- paste0(y + 1, "-", "01", "-", "01")
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
    y <- y + 1
  }
}

df$paid_services[92] <- "144417,7"
df$date[92] <- "2018-08-01"

df$paid_services[93] <- "166959,7"
df$date[93] <- "2018-09-01"

df$paid_services[94] <- "189354,5"
df$date[94] <- "2018-10-01"

df$paid_services[95] <- "212923,9"
df$date[95] <- "2018-11-01"

df$paid_services[96] <- "236662,1"
df$date[96] <- "2018-12-01"

df$paid_services[97] <- "261406,9"
df$date[97] <- "2019-01-01"

df$paid_services[98] <- "20815,3"
df$date[98] <- "2019-02-01"

df$paid_services[99] <- "42159,3"
df$date[99] <- "2019-03-01"

df$paid_services[100] <- "64180,6"
df$date[100] <- "2019-04-01"

df$paid_services[101] <- "85682,9"
df$date[101] <- "2019-05-01"

df$paid_services[102] <- "106898,9"
df$date[102] <- "2019-06-01"

df$paid_services[103] <- "130365,9"
df$date[103] <- "2019-07-01"

df$paid_services[104] <- "152745,4"
df$date[104] <- "2019-08-01"

df$paid_services[105] <- "175461,4"
df$date[105] <- "2019-09-01"

df$paid_services[106] <- "198724,8"
df$date[106] <- "2019-10-01"

df$paid_services[107] <- "222120,2"
df$date[107] <- "2019-11-01"

df$paid_services[108] <- "246975,3"
df$date[108] <- "2019-12-01"

df$paid_services[109] <- "271890,9"
df$date[109] <- "2020-01-01"

df$paid_services[110] <- "22218,5"
df$date[110] <- "2020-02-01"

df$paid_services[111] <- "44430,6"
df$date[111] <- "2020-03-01"

df$paid_services[112] <- "66791,6"
df$date[112] <- "2020-04-01"

df$paid_services[113] <- "80950,6"
df$date[113] <- "2020-05-01"

df$paid_services[114] <- "95695,4"
df$date[114] <- "2020-06-01"

df$paid_services[115] <- "107750,0"
df$date[115] <- "2020-07-01"

df$paid_services[116] <- "122378,6"
df$date[116] <- "2020-08-01"

df$paid_services[117] <- "141092,2"
df$date[117] <- "2020-09-01"

df$paid_services[118] <- "159422,4"
df$date[118] <- "2020-10-01"

df$paid_services[119] <- "178800,1"
df$date[119] <- "2020-11-01"

df$paid_services[120] <- "198699,5"
df$date[120] <- "2020-12-01"



df$paid_services <-  gsub(",", ".", df$paid_services) # меняем запятые на точки
df$paid_services <- as.numeric(df$paid_services) # переводим столец в числовой тип

# убираем нарастующий итог из данных товарооборота
df_p_services_month <- data.frame("paid_services" = rep(NA, 120),
                               "date" = df$date)
df_p_services_month$paid_services[1] <- 0

m <- 1
for(i in 2:120){
  if(m == 1){
    df_p_services_month$paid_services[i] <- df$paid_services[i]
  }
  
  if(m >= 2){
    df_p_services_month$paid_services[i] <- df$paid_services[i] - df$paid_services[i - 1]
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
  }
}

# приводим к сопоставимым ценам
df_p_services_month <- df_p_services_month[1:118, ]
df_p_services_month_comparable <- df_p_services_month
df_p_services_month_comparable$paid_services <- df_p_services_month_comparable$paid_services * k_n$k_n

# посмотрим на график исходных цен и сопоставимых (красный сопоставимые)
ggplot()+
  geom_line(data = df_p_services_month_comparable, 
            aes(x = date, y = paid_services, col = 'red')) +
  geom_line(data = df_p_services_month,
            aes(x = date, y = paid_services))

remove(df, df2)

# 4) Статьи расходов бюджета
library(dplyr)

df <-  group_by(df_expenses, c_1) %>% summarise(N = n())

df <- df[df$N == 119 | df$N == 120, ]
df <- df[-3, ] # всего получилось 13 статей


# объеденим все расходы в список
expenses <- list()
# Жилищно-коммунальное хозяйство
summary(df_expenses[df_expenses$c_1 == df$c_1[1], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[1], ]) # 120 строк

expenses$hau <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[1]],
                           "date" = df_expenses$date[df_expenses$c_1 == df$c_1[1]])
# Здравоохранение
summary(df_expenses[df_expenses$c_1 == df$c_1[2], ]) # 2011-02-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[2], ]) # 119 строк

expenses$health_care <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[2]],
                           "date" = df_expenses$date[df_expenses$c_1 == df$c_1[2]])

# Культура и кинематография
summary(df_expenses[df_expenses$c_1 == df$c_1[3], ]) # 2011-02-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[3], ]) # 119 строк

expenses$culture_cinema <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[3]],
                                   "date" = df_expenses$date[df_expenses$c_1 == df$c_1[3]])

# Национальная безопасность и правоохранительная деятельность
summary(df_expenses[df_expenses$c_1 == df$c_1[4], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[4], ]) # 120 строк

expenses$national_security <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[4]],
                                      "date" = df_expenses$date[df_expenses$c_1 == df$c_1[4]])

# Национальная оборона
summary(df_expenses[df_expenses$c_1 == df$c_1[5], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[5], ]) # 120 строк

expenses$national_defense <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[5]],
                                         "date" = df_expenses$date[df_expenses$c_1 == df$c_1[5]])

# Национальная экономика
summary(df_expenses[df_expenses$c_1 == df$c_1[6], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[6], ]) # 120 строк

expenses$national_economy <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[6]],
                                        "date" = df_expenses$date[df_expenses$c_1 == df$c_1[6]])

# Образование
summary(df_expenses[df_expenses$c_1 == df$c_1[7], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[7], ]) # 120 строк

expenses$education <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[7]],
                                        "date" = df_expenses$date[df_expenses$c_1 == df$c_1[7]])

# Обслуживание государственного и муниципального долга
summary(df_expenses[df_expenses$c_1 == df$c_1[8], ]) # 2011-02-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[8], ]) # 119 строк

expenses$municipal_debt <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[8]],
                                 "date" = df_expenses$date[df_expenses$c_1 == df$c_1[8]])


# Общегосударственные вопросы
summary(df_expenses[df_expenses$c_1 == df$c_1[9], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[9], ]) # 120 строк

expenses$national_issues <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[9]],
                                      "date" = df_expenses$date[df_expenses$c_1 == df$c_1[9]])

# Охрана окружающей среды
summary(df_expenses[df_expenses$c_1 == df$c_1[10], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[10], ]) # 120 строк

expenses$environmental_protection <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[10]],
                                       "date" = df_expenses$date[df_expenses$c_1 == df$c_1[10]])

# Социальная политика
summary(df_expenses[df_expenses$c_1 == df$c_1[11], ]) # 2011-01-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[11], ]) # 120 строк

expenses$social_policy <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[11]],
                                                "date" = df_expenses$date[df_expenses$c_1 == df$c_1[11]])

# Средства массовой информации
summary(df_expenses[df_expenses$c_1 == df$c_1[12], ]) # 2011-02-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[12], ]) # 119 строк

expenses$mass_media <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[12]],
                                     "date" = df_expenses$date[df_expenses$c_1 == df$c_1[12]])

# Физическая культура и спорт
summary(df_expenses[df_expenses$c_1 == df$c_1[13], ]) # 2011-02-01 - 2020-12-01
nrow(df_expenses[df_expenses$c_1 == df$c_1[13], ]) # 119 строк

expenses$sport <- data.frame("c_3" = df_expenses$c_3[df_expenses$c_1 == df$c_1[13]],
                                  "date" = df_expenses$date[df_expenses$c_1 == df$c_1[13]])


expenses$total_expenses <- df_expenses_month
remove(df_expenses_month)
expenses$total_expenses_compar <- df_expenses_month_comparable
remove(df_expenses_month_comparable)

# объединим розничный товарооборот и платные услуги
retail_services <- list()

retail_services$trade <- df_r_trade_month
retail_services$trade_compar <- df_r_trade_month_comparable
retail_services$services <- df_p_services_month
retail_services$services_compar <- df_p_services_month_comparable

remove(df_r_trade_month, df_r_trade_month_comparable, df_p_services_month, df_p_services_month_comparable)
remove(indexes, R_2)
remove(df)


# убираем нарастающий итог в статьях расхода
length(expenses$hau$c_3) # 120
length(expenses$health_care$c_3) # 119
length(expenses$culture_cinema$c_3) # 119
length(expenses$national_security$c_3) # 120
length(expenses$national_defense$c_3) # 120
length(expenses$national_economy$c_3) # 120
length(expenses$education$c_3) # 120
length(expenses$municipal_debt$c_3) # 119
length(expenses$national_issues$c_3) # 120
length(expenses$environmental_protection$c_3) # 120
length(expenses$social_policy$c_3) # 120
length(expenses$mass_media$c_3) # 119
length(expenses$sport$c_3) # 119

# 120
expenses$hau$c_3[1] <- 0
expenses$national_security$c_3[1] <- 0
expenses$national_defense$c_3[1] <- 0
expenses$national_economy$c_3[1] <- 0
expenses$education$c_3[1] <- 0
expenses$national_issues$c_3[1] <- 0
expenses$environmental_protection$c_3[1] <- 0
expenses$social_policy$c_3[1] <- 0


expenses$hau_compar <- data.frame("c_3" = rep(NA, 120),
                                  "date" = expenses$hau$date)
expenses$national_security_compar <- data.frame("c_3" = rep(NA, 120),
                                                "date" = expenses$national_security$date)
expenses$national_defense_compar <- data.frame("c_3" = rep(NA, 120),
                                               "date" = expenses$national_defense$date)
expenses$national_economy_compar <- data.frame("c_3" = rep(NA, 120),
                                               "date" = expenses$national_economy$date)
expenses$education_compar <- data.frame("c_3" = rep(NA, 120),
                                        "date" = expenses$education$date)
expenses$national_issues_compar <- data.frame("c_3" = rep(NA, 120),
                                              "date" = expenses$national_issues$date)
expenses$environmental_protection_compar <- data.frame("c_3" = rep(NA, 120),
                                                       "date" = expenses$environmental_protection$date)
expenses$social_policy_compar <- data.frame("c_3" = rep(NA, 120),
                                            "date" = expenses$social_policy$date)

expenses$hau_compar$c_3[1] <- 0
expenses$national_security_compar$c_3[1] <- 0
expenses$national_defense_compar$c_3[1] <- 0
expenses$national_economy_compar$c_3[1] <- 0
expenses$education_compar$c_3[1] <- 0
expenses$national_issues_compar$c_3[1] <- 0
expenses$environmental_protection_compar$c_3[1] <- 0
expenses$social_policy_compar$c_3[1] <- 0


m <- 1
for(i in 2:120){
  if(m == 1){
    expenses$hau_compar$c_3[i] <- expenses$hau$c_3[i]
    expenses$national_security_compar$c_3[i] <- expenses$national_security$c_3[i]
    expenses$national_defense_compar$c_3[i] <- expenses$national_defense$c_3[i]
    expenses$national_economy_compar$c_3[i] <- expenses$national_economy$c_3[i]
    expenses$education_compar$c_3[i] <- expenses$education$c_3[i]
    expenses$national_issues_compar$c_3[i] <- expenses$national_issues$c_3[i]
    expenses$environmental_protection_compar$c_3[i] <- expenses$environmental_protection$c_3[i]
    expenses$social_policy_compar$c_3[i] <- expenses$social_policy$c_3[i]
  }
  
  if(m >= 2){
    expenses$hau_compar$c_3[i] <- expenses$hau$c_3[i] - 
      expenses$hau$c_3[i - 1]
    
    expenses$national_security_compar$c_3[i] <- expenses$national_security$c_3[i] - 
      expenses$national_security$c_3[i - 1]
    
    expenses$national_defense_compar$c_3[i] <- expenses$national_defense$c_3[i] - 
      expenses$national_defense$c_3[i - 1]
    
    expenses$national_economy_compar$c_3[i] <- expenses$national_economy$c_3[i] - 
      expenses$national_economy$c_3[i - 1]
    
    expenses$education_compar$c_3[i] <- expenses$education$c_3[i] - 
      expenses$education$c_3[i - 1]
    
    expenses$national_issues_compar$c_3[i] <- expenses$national_issues$c_3[i] -
      expenses$national_issues$c_3[i - 1]
    
    expenses$environmental_protection_compar$c_3[i] <- expenses$environmental_protection$c_3[i] -
      expenses$environmental_protection$c_3[i - 1]
    
    expenses$social_policy_compar$c_3[i] <- expenses$social_policy$c_3[i] -
      expenses$social_policy$c_3[i - 1]
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
  }
}

# приводим к спопоставимым ценам
expenses$hau_compar <- expenses$hau_compar[1:118, ]
expenses$national_security_compar <- expenses$national_security_compar[1:118, ]
expenses$national_defense_compar <- expenses$national_defense_compar[1:118, ]
expenses$national_economy_compar <- expenses$national_economy_compar[1:118, ]
expenses$education_compar <- expenses$education_compar[1:118, ]
expenses$national_issues_compar <- expenses$national_issues_compar[1:118, ]
expenses$environmental_protection_compar <- expenses$environmental_protection_compar[1:118, ]
expenses$social_policy_compar <- expenses$social_policy_compar[1:118, ]

expenses$hau_compar$c_3 <- expenses$hau_compar$c_3 * k_n$k_n
expenses$national_security_compar$c_3 <- expenses$national_security_compar$c_3 * k_n$k_n
expenses$national_defense_compar$c_3 <- expenses$national_defense_compar$c_3 * k_n$k_n
expenses$national_economy_compar$c_3 <- expenses$national_economy_compar$c_3 * k_n$k_n
expenses$education_compar$c_3 <- expenses$education_compar$c_3 * k_n$k_n
expenses$national_issues_compar$c_3 <- expenses$national_issues_compar$c_3 * k_n$k_n
expenses$environmental_protection_compar$c_3 <- expenses$environmental_protection_compar$c_3 * k_n$k_n
expenses$social_policy_compar$c_3 <- expenses$social_policy_compar$c_3 * k_n$k_n


# 119
df <- data.frame("c_3" = 0,
                 "date" = "2011-01-01")
df$date <- as.Date(df$date)

expenses$health_care <- rbind(df, expenses$health_care)
expenses$culture_cinema <- rbind(df, expenses$culture_cinema)
expenses$municipal_debt <- rbind(df, expenses$municipal_debt)
expenses$mass_media <- rbind(df, expenses$mass_media)
expenses$sport <- rbind(df, expenses$sport)

summary(expenses$health_care)
summary(expenses$culture_cinema)
summary(expenses$municipal_debt)
summary(expenses$mass_media)
summary(expenses$sport)

remove(df)

expenses$health_care_compar <- data.frame("c_3" = rep(NA, 120),
                                          "date" = expenses$health_care$date)
expenses$culture_cinema_compar <- data.frame("c_3" = rep(NA, 120),
                                          "date" = expenses$culture_cinema$date)
expenses$municipal_debt_compar <- data.frame("c_3" = rep(NA, 120),
                                          "date" = expenses$municipal_debt$date)
expenses$mass_media_compar <- data.frame("c_3" = rep(NA, 120),
                                          "date" = expenses$mass_media$date)
expenses$sport_compar <- data.frame("c_3" = rep(NA, 120),
                                          "date" = expenses$sport$date)

expenses$health_care_compar$c_3[1] <- 0
expenses$culture_cinema_compar$c_3[1] <- 0
expenses$municipal_debt_compar$c_3[1] <- 0
expenses$mass_media_compar$c_3[1] <- 0
expenses$sport_compar$c_3[1] <- 0


m <- 1
for(i in 2:120){
  if(m == 1){
    expenses$health_care_compar$c_3[i] <- expenses$health_care$c_3[i]
    expenses$culture_cinema_compar$c_3[i] <- expenses$culture_cinema$c_3[i]
    expenses$municipal_debt_compar$c_3[i] <- expenses$municipal_debt$c_3[i]
    expenses$mass_media_compar$c_3[i] <- expenses$mass_media$c_3[i]
    expenses$sport_compar$c_3[i] <- expenses$sport$c_3[i]
  }
  
  if(m >= 2){
    expenses$health_care_compar$c_3[i] <- expenses$health_care$c_3[i] -
      expenses$health_care$c_3[i - 1]
    
    expenses$culture_cinema_compar$c_3[i] <- expenses$culture_cinema$c_3[i] -
      expenses$culture_cinema$c_3[i - 1]
    
    expenses$municipal_debt_compar$c_3[i] <- expenses$municipal_debt$c_3[i] -
      expenses$municipal_debt$c_3[i - 1]
    
    expenses$mass_media_compar$c_3[i] <- expenses$mass_media$c_3[i] -
      expenses$mass_media$c_3[i - 1]
    
    expenses$sport_compar$c_3[i] <- expenses$sport$c_3[i] -
      expenses$sport$c_3[i - 1]
  }
  
  m <- m + 1
  
  if(m > 12){
    m <- 1
  }
}

# приводим к сопоставимым ценам
expenses$health_care_compar <- expenses$health_care_compar[1:118, ]
expenses$culture_cinema_compar <- expenses$culture_cinema_compar[1:118, ]
expenses$municipal_debt_compar <- expenses$municipal_debt_compar[1:118, ]
expenses$mass_media_compar <- expenses$mass_media_compar[1:118, ]
expenses$sport_compar <- expenses$sport_compar[1:118, ]

expenses$health_care_compar$c_3 <- expenses$health_care_compar$c_3 * k_n$k_n
expenses$culture_cinema_compar$c_3 <- expenses$culture_cinema_compar$c_3 * k_n$k_n
expenses$municipal_debt_compar$c_3 <- expenses$municipal_debt_compar$c_3 * k_n$k_n
expenses$mass_media_compar$c_3 <- expenses$mass_media_compar$c_3 * k_n$k_n
expenses$sport_compar$c_3 <- expenses$sport_compar$c_3 * k_n$k_n






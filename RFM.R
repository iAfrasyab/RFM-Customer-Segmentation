library(tidyverse)

df_salesData <- read.csv("C:/Users/FUJITSU/Documents/IEC Work/storeData.csv")
date_today <- lubridate::ymd("20111230")
df_salesData |>
  mutate(subtotal=Quantity*UnitPrice,
         Bill_date=strptime(InvoiceDate,format = "%m/%d/%Y %H:%M"))|>
  group_by(CustomerID)|>
  summarise(Total_revenue=sum(subtotal),
            n_transactions= n_distinct(InvoiceNo),
            last_purchase_date=max(Bill_date))|>
  mutate(N_days_lastPurchase=
           difftime(date_today,last_purchase_date,units = ("days")))|>
  mutate(N_Days_Last_Purchase=as.integer(N_days_lastPurchase)) |> 
  janitor::clean_names() |> 
  select(customer_id, total_revenue, n_transactions, 
         n_days_last_purchase = n_days_last_purchase_2) |>
  drop_na() -> df_rfm_1

df_rfm_1 |>
  mutate(monetary_rank = Hmisc::cut2(total_revenue, g = 5),
         recency_rank = Hmisc::cut2(n_days_last_purchase, g = 5),
         frequency_rank = Hmisc::cut2(n_transactions, g = 5)) -> df_rfm_2
df_rfm_2 |>
  mutate(monetary_score = as.integer(monetary_rank),
         frequency_score = as.integer(frequency_rank),
         recency_score = as.integer(recency_rank)) |>
  mutate(recency_score = dense_rank(desc(recency_score))) -> df_rfm_3
df_rfm_3 |>
  dplyr::mutate(labels = ifelse(recency_score >= 4 & frequency_score >=4 & monetary_score >= 4,"Champions",
                                ifelse(recency_score >= 2 & (frequency_score + monetary_score)/2 >= 3, "Loyal Customers",
                                       ifelse(recency_score >=3 & (frequency_score + monetary_score)/2 >=1, "Potential Loyalists", 
                                              ifelse(recency_score >= 4 & ((frequency_score + monetary_score)/2 <=1 & (frequency_score + monetary_score)/2 >=0) , "Recent_customers",
                                                     ifelse((recency_score >=3 & recency_score <=4)  & ((frequency_score + monetary_score)/2 <=1 & (frequency_score + monetary_score)/2 >=0), "Promising",
                                                            ifelse((recency_score >=1 & recency_score <=3) & ((frequency_score + monetary_score)/2 >=2 & (frequency_score + monetary_score)/2 <=3),"Need attention", 
                                                                   ifelse(recency_score >=2 & recency_score <=3 & (frequency_score + monetary_score)/2 >=0 & (frequency_score + monetary_score)/2 <=2, "About to sleep",
                                                                          ifelse(recency_score <=2 & frequency_score <=2 & monetary_score <=2, "Lost",
                                                                                 ifelse((recency_score +  frequency_score)/2 <=2 & monetary_score >=4, "High spending new customer",
                                                                                        ifelse(recency_score <2 & frequency_score >= 3 & monetary_score >= 3, "High value churned", 
                                                                                               ifelse(recency_score <2 & frequency_score <2 & monetary_score >=3, "One time high spending churned",
                                                                                                      ifelse(recency_score <2 & frequency_score >=4 & monetary_score <=3, "Low value loyal churned", ""))))))))))))) -> df_rfm_4



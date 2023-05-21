library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(Hmisc)
library(gapminder)



eu_storedata <- readxl::read_xls("C:/Users/DELL/Downloads/Sample - EU Superstore.xls")
Checkdate <-  as.POSIXct("2019-01-31")
df_eu_storedata <- eu_storedata |>  mutate(Order_ID = paste(`Order ID`, `Customer ID`, sep = '_'))
#Working for customer level metrics
Final_submissions <-
  df_eu_storedata |>
  group_by(`Customer ID`) |> 
  summarise(Customer_name = first(`Customer Name`),
            segment = first(Segment),
            total_profit = sum(Profit),
            average_order_value = (sum(Sales)/n_distinct(`Order ID`)),
            average_order_profit = (sum(Profit)/n_distinct(`Order ID`)),
            average_products_purchased_per_order = (n_distinct(`Product Name`)/n_distinct(`Order ID`)),
            Average_revenue_per_product_for_the_customer = (sum(Sales)/sum(Quantity)),
            Average_profit_per_product_for_the_customer = (sum(Profit))/sum(Quantity),
            total_revenue = sum(Sales), 
            total_transactions = n_distinct(`Order ID`),
            date_last_transaction = as.Date(max(`Order Date`)),
            days_last_transaction = round(as.numeric(difftime(Checkdate,max(`Order Date`),units = "days"))),
            date_first_transaction = as.Date(min(`Order Date`))) |> 
              left_join(
    #Working for median_days_bw_transaction
    df_eu_storedata |> 
      arrange(desc(`Order Date`)) |> 
      #Getting unique combination of customer id and invoice no.
      group_by(`Customer ID`,`Order ID`) |>
      #Finding date for this unique combination
      summarise(date_unique_combination = as.Date(first(`Order Date`))) |> 
      #Arranging for better mathematics
      arrange(desc(date_unique_combination)) |> 
      #Creating a mutation to find days elapsed between transaction by using "ifelse" function so that lag can be used for followup dates and checkdate(2011-12-31) can be used for first dates
      mutate(diff_bw_days = as.integer(lag(date_unique_combination) - date_unique_combination)) |> 
      #Now grouping only on customer ID to get relevant data for each customer level only
      arrange(desc(diff_bw_days)) |>
      group_by(`Customer ID`) |> 
      summarise(median_days_bw_transaction = round(median(diff_bw_days ,na.rm = TRUE)))) |> 
      #Creating RFM segments
      mutate(Recency_segment = cut2(days_last_transaction, g = 5),
         Frequency_segment = cut2(total_transactions, g = 5),
         Revenue_segment = cut2(total_revenue, g = 5)) |> 
  #Scoring RFM segments  
  mutate(Recency_score = as.integer(fct_rev(Recency_segment)),
         Frequency_score = as.integer(Frequency_segment),
         Revenue_score = as.integer(Revenue_segment)) |> 
  #Giving customers ranking(1-5) on basis of RFM score  
  mutate(RFMScore = Recency_score*100 + Frequency_score*10 + Revenue_score) |> 
  mutate(Segment = case_when(
  #Recent purchase, frequent transactions, high spending.
  RFMScore %in% c(555, 554, 544, 545, 454, 455, 445) ~ "Champions",
  #Often spend good money buying your products. Responsive to promotions
  RFMScore %in% c(543, 444, 435, 355, 354, 345, 344, 335) ~ "Loyal Customers",
  #Recent customers but spent a good amount and bought more than once
  RFMScore %in% c(553, 551, 552, 541, 542, 533, 532, 531, 452, 451, 442, 441, 431, 453, 433, 432, 423, 353, 352, 351, 342, 341, 333, 323) ~ "Potential Loyalists",
  #Bought most recently, but not often
  RFMScore %in% c(512, 511, 422, 421, 412, 411, 311) ~ "Recent Customers",
  #Recent shoppers but haven’t spent much
  RFMScore %in% c(525, 524, 523, 522, 521, 515, 514, 513, 425, 424, 413, 414, 415, 315, 314, 313) ~ "Promising",
  #Above-average recency, frequency and monetary values. They may not have bought very recently though
  RFMScore %in% c(535, 534, 443, 434, 343, 334, 325, 324) ~ "Needs Attention",
  #Below average recency, frequency, and monetary values. Will lose them if not reactivated
  RFMScore %in% c(331, 321, 312, 221, 213) ~ "About to Sleep",
  #They spent big money and purchased often. But the last purchase was a long time ago
  RFMScore %in% c(255, 254, 245, 244, 253, 252, 243, 242, 235, 234, 225, 224, 153, 152, 145, 143, 142, 135, 134, 133, 125, 124) ~ "At Risk",
  #Often made the biggest purchases but they haven’t returned for a long time
  RFMScore %in% c(155, 154, 144, 214, 215, 115, 114, 113) ~ "Can’t lose",
  #The last purchase was long ago. Low spenders with a low number of orders
  RFMScore %in% c(332, 322, 231, 241, 251, 233, 232, 223, 222, 132, 123, 122, 212, 211) ~ "Hibernating",
  #Lowest recency, frequency, and monetary scores
  RFMScore %in% c(111, 112, 121, 131, 141, 151) ~"Lost"
)) |> 
left_join(
#trying to classify type of business(higher margin - lower margin) customer is giving to store 
  df_eu_storedata |> 
    group_by(`Customer ID`,`Customer Name`,`Product ID`) |> 
    summarise(profitt = sum(Profit)/sum(Quantity)) |> 
    group_by(`Customer ID`,`Customer Name`) |> 
    summarise(profitability = mean(profitt, na.rm = TRUE)) |> 
    ungroup() |>
    mutate(profitability_group = cut2(profitability, g=5) |> 
             factor(labels = c("Very Low", "Low", "Moderate", "High", "Very High"))) |>
    select(`Customer ID`,`Customer Name`,profitability_group)) |> 
left_join(  
#figuring out whether customer is a Regular one or Promotional   
    df_eu_storedata |>
      group_by(`Customer Name`) |>
      summarise(undisc_quantity = sum(ifelse(Discount == 0,Quantity,0)), 
                discount_quantity =sum(ifelse(Discount>0,Quantity,0)), 
                undisc_purchase = sum(ifelse(Discount == 0,Sales,0)), 
                discount_purchase =sum(ifelse(Discount>0,Sales,0))) |> 
      mutate(shopper_type = case_when(
        discount_quantity > undisc_quantity & discount_purchase > undisc_purchase ~ "promotional_shopper",
        discount_purchase >= 2*undisc_purchase ~ "promotional_shopper",
        TRUE ~ "Regular_shopper")) |> 
      select(`Customer Name`,shopper_type),
    by = "Customer Name") |> 
left_join(
#finding out most "spent-on" category and subcategory of each customer(no tie observed)   
    df_eu_storedata |> 
      group_by(`Customer Name`,Category,`Sub-Category`) |>
      summarise(max_sales = sum(Sales)) |> 
      group_by(`Customer Name`) |> 
      slice_max(max_sales, n = 1, with_ties = FALSE) |> 
      rename(sales_category = Category, sales_sub_category = `Sub-Category`) |> 
      left_join(
        df_eu_storedata  |> 
          group_by(`Customer Name`, Category) |> 
          mutate(category_sales = sum(Sales)) |> 
          group_by(`Customer Name`)  |> 
          summarise(Customer_sale = sum(Sales),
                    percent_spending_on_sales_category = round((max(category_sales)/Customer_sale)*100)) |> 
          mutate(percent_spending_on_sales_category = paste0(percent_spending_on_sales_category, "%"))) |> 
      mutate(sales_category = paste0(percent_spending_on_sales_category, " ", sales_category)) |> 
      select(`Customer Name`,`sales_sub_category`,`sales_category`),
    by = "Customer Name") |> 
#finding out most "frequently purchased" category and subcategory of each customer(tie is broken by quantity,followed by sales) 
left_join(
    df_eu_storedata |> 
      group_by(`Customer Name`,Category,`Sub-Category`) |>
      summarise(most_frequently_purchased = n_distinct(`Order ID`),
                most_sales_quantity = sum(Quantity),
                most_sales = sum(Sales)) |> 
      group_by(`Customer Name`) |> 
      slice_max(most_frequently_purchased, n = 1, with_ties = TRUE) |> 
      slice_max(most_sales_quantity, n = 1, with_ties = TRUE) |> 
      slice_max(most_sales, n = 1, with_ties = TRUE) |> 
      select(`Customer Name`, Category,`Sub-Category`) |> 
      rename(most_frequently_purchased_category = Category, most_frequently_purchased_sub_category = `Sub-Category`),
    by = "Customer Name") |> 
left_join(
#finding out favorite ship mode of each customer on basis of two factors. 1st(freuqently order placed) 2nd(most quantity bought)
    df_eu_storedata |> 
      group_by(`Customer Name`,`Ship Mode`) |> 
      summarise(n_Order_Id = n_distinct(`Order ID`),
                max_qty = sum(Quantity)) |> 
      group_by(`Customer Name`) |> 
      slice_max(n_Order_Id, n = 1, with_ties = TRUE) |> 
      slice_max(max_qty, n = 1, with_ties = FALSE) |>
      #not breaking tie any further beacuse the maximun order IDs in such case are only 6
      select(`Customer Name`,`Ship Mode`)) |> 
left_join(
#analysing base region of a customer with conditions, 1st(frequent order), 2nd(Most Qty)
    df_eu_storedata |> 
      group_by(`Customer Name`,`Region`) |> 
      summarise(n_order_Id = n_distinct(`Order ID`),
                max_qty = sum(Quantity)) |> 
      group_by(`Customer Name`) |> 
      slice_max(n_order_Id, n = 1, with_ties = TRUE) |> 
      slice_max(max_qty, n = 1, with_ties = FALSE) |> 
      select(`Customer Name`,`Region`)) |> 
left_join(  
#analysing base country of a customer with conditions, 1st(frequent order), 2nd(Most Qty), 3rd(minimum variablity in state), 4th(minimum variablity in city), 5th(maximum sales)
    df_eu_storedata |> 
      group_by(`Customer Name`,Country) |> 
      summarise(n_order_Id = n_distinct(`Order ID`),
                max_qty = sum(Quantity),
                n_state = n_distinct(State),
                n_city = n_distinct(City),
                sale = sum(Sales)) |> 
      group_by(`Customer Name`) |> 
      slice_max(n_order_Id, n = 1, with_ties = TRUE) |> 
      slice_max(max_qty, n = 1, with_ties = TRUE) |> 
      slice_min(n_state, n = 1, with_ties = TRUE) |> 
      slice_min(n_city, n = 1, with_ties = TRUE) |> 
      slice_max(sale, n = 1, with_ties = TRUE) |>
      select(`Customer Name`,`Country`)) |>  
left_join(
#analysing base state of a customer with conditions, 1st(frequent order), 2nd(Most Qty), 3rd(minimum variablity in city), 4th(maximum sales)
    df_eu_storedata |> 
      group_by(`Customer Name`,State) |> 
      summarise(n_order_Id = n_distinct(`Order ID`),
                max_qty = sum(Quantity),
                n_city = n_distinct(City),
                sale = sum(Sales)) |> 
      group_by(`Customer Name`) |> 
      slice_max(n_order_Id, n = 1, with_ties = TRUE) |> 
      slice_max(max_qty, n = 1, with_ties = TRUE) |> 
      slice_min(n_city, n = 1, with_ties = TRUE) |> 
      slice_max(sale, n = 1, with_ties = TRUE) |>
      select(`Customer Name`,`State`)) |>
left_join(
#analysing base city of a customer with conditions, 1st(frequent order), 2nd(Most Qty), 3rd(maximum sales)
    df_eu_storedata |> 
      group_by(`Customer Name`,City) |> 
      summarise(n_order_Id = n_distinct(`Order ID`),
                max_qty = sum(Quantity),
                sale = sum(Sales)) |> 
      group_by(`Customer Name`) |> 
      slice_max(n_order_Id, n = 1, with_ties = TRUE) |> 
      slice_max(max_qty, n = 1, with_ties = TRUE) |> 
      slice_max(sale, n = 1, with_ties = TRUE) |>
      select(`Customer Name`,`City`)) |>
left_join(
#finding Average time to deliver orders for the customer.
    df_eu_storedata |> 
      group_by(`Customer Name`,`Order ID`) |> 
      summarise(S = difftime(`Ship Date`,`Order Date`)) |> 
      group_by(`Customer Name`) |> 
      summarise(avg_shipping_hours = round(mean(as.numeric(S)/3600)))) |> 
left_join(
#Percentage_of_orders_with_multiple_products_for_the_customer
    df_eu_storedata |> 
      group_by(`Customer ID`,`Customer Name`,`Order ID`) |> 
      summarise(num_products = case_when(n_distinct(`Product ID`) > 1 ~ "Multiple", TRUE ~ "Single")) |> 
      group_by(`Customer Name`) |> 
      summarise(multiple_products_per_order_percentage = (sum(case_when(num_products == "Multiple" ~ 1, TRUE ~ 0))/n_distinct(`Order ID`))*100) |> 
      select(`Customer Name`,multiple_products_per_order_percentage)) |> 
left_join(
#calculating the year-over-year(last year) growth in sales for each customer
    df_eu_storedata |> 
      group_by(`Customer ID`,`Customer Name`,year = lubridate::year(`Order Date`)) |> 
      summarise(total_sales = sum(`Sales`)) |> 
      group_by(`Customer ID`) |> 
      mutate(growth_rate = (total_sales / lag(total_sales) - 1) * 100) |> 
      filter(year == last(year)) |> 
      select(`Customer ID`,`Customer Name`,year,growth_rate)) |> 
select(`Customer ID`,
       Customer_name,
       segment,
       total_profit,
       average_order_value,
       average_order_profit,
       average_products_purchased_per_order,
       Average_revenue_per_product_for_the_customer,
       Average_profit_per_product_for_the_customer,
       total_revenue,
       total_transactions,
       date_last_transaction,
       days_last_transaction,
       date_first_transaction,
       median_days_bw_transaction,
       Recency_score,
       Frequency_score,
       Revenue_score,
       RFMScore,
       Segment,
       profitability_group,
       shopper_type,
       sales_sub_category,
       sales_category,
       most_frequently_purchased_category,
       most_frequently_purchased_sub_category,
       `Ship Mode`,
       Region,
       Country,
       State,
       City,
       avg_shipping_hours,
       multiple_products_per_order_percentage,
       year,
       growth_rate)
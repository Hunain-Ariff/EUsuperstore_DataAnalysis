#finding percentage of customers who are regular and promotional
  FinalSubmission |> 
  group_by(shopper_type) |> 
  summarise(n_customer = n_distinct(`Customer.ID`)) |> 
  mutate(percentage = (n_customer/795)*100) |> view()
  #RESULT : Regular_shopper,Customer = 528,percentage = "66.415"
  #        : Promotional_shopper,Customer =  267,percentage = "33.584"
  
#finding average discount that customers get on their order
 average_discount_percentage_per_order <- df_eu_storedata |> 
  group_by(`Customer Name`,Order_ID) |> 
  summarise(OID_discount = sum(Discount)) |>  
  group_by(`Customer Name`) |> 
  summarise(Cust_disc = mean(OID_discount)*100) |> 
  mutate(discount_shopping = mean(Cust_disc),
         normal_shopping = (100-mean(Cust_disc))) |> view()
  #RESULT : "20.113%" is the average discount each customer get 

#Percentage of orders with high-priced products
 Percentage_of_orders_with_high_priced_products_3 <- df_eu_storedata |> 
  mutate(unit_price = Sales/Quantity) |> 
  mutate(Factors = cut2(unit_price)) |> 
  group_by(Factors) |> 
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(Factors, n=1, with_ties = FALSE) |> 
   mutate(high_percentage = (orders/5121)*100,
          remainin_percentage = 100-((orders/5121)*100)) |> view()
  #RESULT : Unit Price Range[473.47,908.55], Orders = 148 
  #         (148/5121)*100 = "2.85%"
  
#working for CLV
  Final_submissions |>
    mutate(CV = mean(total_transactions) * mean(average_order_value)) |> 
    mutate(CLS = mean(difftime(date_last_transaction, date_first_transaction, units = "days") / 365)) |> 
    left_join(df_eu_storedata |>
                mutate(year = lubridate::year(`Order Date`)) |>
                group_by(`Customer ID`, year) |>
                summarise(year_finding = n_distinct(Order_ID)) |>
                group_by(`Customer ID`) |>
                summarise(avg_order_per_year = mean(year_finding)), by = "Customer ID") |> view()
    summarise(dataset_avg_order_per_year = mean(avg_order_per_year), 
              CLV = as.integer(CV * CLS * dataset_avg_order_per_year)) |>
    view()
  #RESULT : CV  = 3711.283
  #       : CLS = 2.71042 Year
  #       : average order per year by customer = 2.058071
  #       : Customer_lifetime_value = 20702
  
#Finding New Customer Revenue Retention (NCRR)
  
  # Identify new customers who made their first purchase in 2017
  new_customers_2017 <- df_eu_storedata |>
    group_by(`Customer ID`) |>
    summarise(first_purchase_date = min(`Order Date`)) |>
    filter(first_purchase_date >= '2017-01-01' & first_purchase_date <= '2017-12-31') |>
    select(`Customer ID`) 
  
  # Identify customers who made purchases in both 2017 and 2018
  customers_2017_2018 <- df_eu_storedata |>
    group_by(`Customer ID`) |>
    summarise(total_sales_2017 = sum(Sales[`Order Date` >= '2017-01-01' & `Order Date` <= '2017-12-31']),
              total_sales_2018 = sum(Sales[`Order Date` >= '2018-01-01' & `Order Date` <= '2018-12-31'])) |> 
    filter(total_sales_2017 > 0 & total_sales_2018 > 0) |> 
    select(`Customer ID`) 
  
  # Calculate NCRR  
  NCRR <- (df_eu_storedata |>
             filter(`Customer ID` %in% customers_2017_2018$`Customer ID`) |>
             group_by(`Customer ID`) |>
             summarise(total_sales_2017 = sum(Sales[`Order Date` >= '2017-01-01' & `Order Date` <= '2017-12-31']),
                       total_sales_2018 = sum(Sales[`Order Date` >= '2018-01-01' & `Order Date` <= '2018-12-31']))) |>
    left_join(new_customers_2017, by = 'Customer ID') |> 
    summarise(revenue_from_new_customers_2017 = sum(total_sales_2017),
              revenue_lost_due_to_churn = sum(total_sales_2017[is.na(total_sales_2018)]),
              revenue_from_new_customers_2018 = sum(total_sales_2018[!is.na(total_sales_2018)])) |>
    mutate(NCRR = (revenue_from_new_customers_2018 - revenue_lost_due_to_churn) / revenue_from_new_customers_2017 * 100) |> 
    view()
  
  #RESULT : NCRR = 125.4128%
  #       : revenue_from_new_customers_2018 = 850278.6
  #       : revenue_lost_due_to_churn = 0
  #       : revenue_from_new_customers_2017 = 677983.8 
  
#SALES GROWTH RATE FOR EACH YEAR
  # Calculate sales growth rate for 2016
  sales_growth_rate_2016 <- (sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2016-01-01' & df_eu_storedata$`Order Date` <= '2016-12-31']) - 
                               sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2015-01-01' & df_eu_storedata$`Order Date` <= '2015-12-31'])) / 
    sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2015-01-01' & df_eu_storedata$`Order Date` <= '2015-12-31']) * 100
  
  # Calculate sales growth rate for 2017
  sales_growth_rate_2017 <- (sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2017-01-01' & df_eu_storedata$`Order Date` <= '2017-12-31']) - 
                               sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2016-01-01' & df_eu_storedata$`Order Date` <= '2016-12-31'])) / 
    sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2016-01-01' & df_eu_storedata$`Order Date` <= '2016-12-31']) * 100
  
  # Calculate sales growth rate for 2018
  sales_growth_rate_2018 <- (sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2018-01-01' & df_eu_storedata$`Order Date` <= '2018-12-31']) - 
                               sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2017-01-01' & df_eu_storedata$`Order Date` <= '2017-12-31'])) / 
    sum(df_eu_storedata$Sales[df_eu_storedata$`Order Date` >= '2017-01-01' & df_eu_storedata$`Order Date` <= '2017-12-31']) * 100
  
  #RESULT : sales_growth_rate_2016 = 36.84067%
  #       : sales_growth_rate_2017 = 16.22155%
  #       : sales_growth_rate_2018 = 37.05488%
  
#Calculating profit percentage on store's total sales
  df_eu_storedata |>
    summarise(total_profit = sum(Profit), total_sales = sum(Sales)) |>
    mutate(profit_margin = (total_profit / total_sales) * 100)
  #RESULT :  total_profit = 372830 
  #          total_sales = 2938089
  #          profit_margin  = 12.7
  
# Calculate churn rate
  df_eu_storedata |>
    group_by(`Customer ID`) |>
    summarise(last_purchase_date = max(`Order Date`)) |>
    mutate(churned = ifelse(last_purchase_date < "2018-01-01", 1, 0)) |>
    summarise(churn_rate = (sum(churned)/n()) * 100)
  #RESULT : churn rate = 11.2%

#
  #Finding orders percentage per ship mode  
ship_mode <- df_eu_storedata |> 
  group_by(`Ship Mode`) |> 
  summarise(ship_mode_orders = n_distinct(Order_ID)) |> 
  mutate(ship_mode_orders.percentage = round((ship_mode_orders/n_distinct(df_eu_storedata$Order_ID)) * 100)) |> view()
 #RESULT : First Class = 15%
 #       : Same Day = 5%
 #       : Second Class = 20%
 #       : Standard Class = 59%

#Finding orders percentage per segment
segment_orders <- df_eu_storedata |> 
  group_by(`Segment`) |> 
  summarise(segment_orders = n_distinct(Order_ID)) |> 
  mutate(segment_orders.percentage = round((segment_orders/n_distinct(df_eu_storedata$Order_ID)) * 100)) |> view()
 #RESULT = Consumer = 52%
 #         Corporate = 31%
 #         Home Office = 18%

#Finding sales percentage per segment
segment_sales <- df_eu_storedata |> 
  group_by(`Segment`) |> 
  summarise(segment_sales = sum(Sales)) |> 
  mutate(segment_sales.percentage = round((segment_sales/sum(df_eu_storedata$Sales)) * 100)) |> view()
#RESULT = Consumer = 52%
#         Corporate = 31%
#         Home Office = 17%

#Finding quantity percentage per segment
df_eu_storedata |> 
  group_by(`Segment`) |> 
  summarise(segment_quantity = sum(Quantity)) |> 
mutate(segment_quantity.percentage = round((segment_quantity/sum(df_eu_storedata$Quantity)) * 100)) |> view()
#RESULT = Consumer = 52%
#         Corporate = 31%
#         Home Office = 17%

#Finding top 3 regions for orders
df_eu_storedata |> 
  group_by(Region) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 5, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
  #RESULT : Central = 57.820%
  #         North = 21.616%
  #         South = 20.562%

#Finding top 3 countries for orders
df_eu_storedata |> 
  group_by(Country) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 3, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
  #RESULT France = 28.822%
  #       Germany = 19.898%
  #       United Kingdom = 16.656%


#Finding top 3 states of FRANCE for orders
df_eu_storedata |> 
  filter(Country == "France") |>
  group_by(State) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 3, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
  #RESULT : Ile-de-France = 9.802%
  #         Provence-Alpes-Côte d'Azur = 4.003%
  #         Auvergne-Rhône-Alpes = 2.597%


#Finding top 3 states of GERMANY for orders
df_eu_storedata |> 
  filter(Country == "Germany") |>
  group_by(State) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 3, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
  #RESULT : North Rhine-Westphalia = 6.873657%
  #         Berlin = 2.050381%
  #         Bavaria = 1.972271%

#Finding top 3 states of UNITED KINGDOM for orders
df_eu_storedata |> 
  filter(Country == "United Kingdom") |>
  group_by(State) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 5, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
    #RESULT : England = 15.0751806%
    #         Scotland = 1.2302285%
    #         Wales = 0.3514938%


#Finding top  city of "Ile-de-France" for orders
df_eu_storedata |> 
  filter(State == "Ile-de-France") |>
  group_by(City) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 1, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
 #RESULT : Paris = 1.874%


#Finding top  city of " North Rhine-Westphalia" for orders
df_eu_storedata |> 
  filter(State == "North Rhine-Westphalia") |>
  group_by(City) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 1, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
#RESULT : Cologne = 0.6639328%


#Finding top  city of " England " for orders
df_eu_storedata |> 
  filter(State == "England") |>
  group_by(City) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 1, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
#RESULT : London = 2.597149%

#Finding top 5 cities as per orders
df_eu_storedata |> 
  group_by(City) |>
  summarise(orders = n_distinct(Order_ID)) |> 
  slice_max(orders, n = 5, with_ties = FALSE) |> 
  mutate(Percentage_of_orders = (orders/n_distinct(df_eu_storedata$Order_ID) * 100)) |> view()
  #RESULT : London = 2.597149%
  #         Berlin = 2.050381%
  #         Vienna = 2.050381%
  #         Paris = 1.874634%
  #         Madrid = 1.698887%

#Finding top 3 categories for orders
 #creating product transaction level variable df
 variable_df <- df_eu_storedata |> 
  mutate(Product_ID = paste(`Product ID`, `Order ID`, sep = '_')) 
 #working further with new df
 variable_df |> 
  group_by(Category) |>
  summarise(orders = n_distinct(Product_ID)) |> 
  slice_max(orders, n = 5, with_ties = FALSE) |>  
  mutate(Percentage_of_orders = ((orders/n_distinct(variable_df$Product_ID)) * 100)) |> view()
  #RESULT : Office Supplies = 65.869%
  #         Technology = 19.117%
  #         Furniture = 15.013%

 #Finding top 3 Office supplies sub categories
 variable_df |>
   filter(Category == "Office Supplies") |>
   group_by(`Sub-Category`) |>
   summarise(orders = n_distinct(Product_ID)) |>
   slice_max(orders, n = 3, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/n_distinct(variable_df$Product_ID)) * 100)) |>
   view()
 #RESULT : Art = 14.322%
  #         Binders = 13.151%
  #         Storage = 13.101%
 
 #Finding top 3 Technology sub categories
 variable_df |>
   filter(Category == "Technology") |>
   group_by(`Sub-Category`) |>
   summarise(orders = n_distinct(Product_ID)) |>
   slice_max(orders, n = 3, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/n_distinct(variable_df$Product_ID)) * 100)) |>
   view()
 #RESULT = Phones = 5.705135%
 #         Copiers = 4.654189%
 #         Accessories = 4.524072%
 
 #Finding top 3 Furniture sub categories
 variable_df |>
   filter(Category == "Furniture") |>
   group_by(`Sub-Category`) |>
   summarise(orders = n_distinct(Product_ID)) |>
   slice_max(orders, n = 3, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/n_distinct(variable_df$Product_ID)) * 100)) |>
   view()
 #RESULT : Bookcases = 4.8443%
 #         Chairs = 4.64418%
 #         Furnishings = 4.614153%
 
#Findng top 5 products as per orders
product_order <- variable_df |>
   group_by(`Product Name`) |>
   summarise(orders = n_distinct(Order_ID)) |>
   slice_max(orders, n = 5, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/n_distinct(variable_df$Product_ID)) * 100)) |>
   view()
 #RESULT : Eldon File Cart, Single Width = 0.3002702%
 #         Rogers File Cart, Single Width = 0.2702432%
 #         Sanford Pencil Sharpener, Water Color = 0.2702432%
 #         Stanley Pencil Sharpener, Water Color = 0.2502252%
 #         Avery Index Tab, Clear = 0.2402162%
 

 
 #Findng top 5 products as per sales
 product_order <- variable_df |>
   group_by(`Product Name`) |>
   summarise(orders = sum(Sales)) |>
   slice_max(orders, n = 5, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/sum(variable_df$Sales)) * 100)) |>
   view()
 #RESULT : Nokia Smart Phone, Full Size = 1.1405824%
 #         Hoover Stove, Red = 0.7197564%
 #         Hamilton Beach Stove, Silver = 0.6210780%
 #         Office Star Executive Leather Armchair, Adjustable = 0.5116767%
 #         Apple Smart Phone, Full Size = 0.5054023%
  
 
 #Findng top 5 products purchased on DISCOUNT
 discount_order <- variable_df |>
   group_by(`Product Name`) |>
   summarise(orders = sum(Sales[Discount>0])) |>
   slice_max(orders, n = 5, with_ties = FALSE) |>
   mutate(Percentage_of_orders = ((orders/sum(variable_df$Sales[variable_df$Discount>0])) * 100)) |>
   view()
 #RESULT : Nokia Smart Phone, Full Size = 1.018751%
 #         Office Star Executive Leather Armchair, Adjustable = 0.9634216%
 #         SAFCO Executive Leather Armchair, Black = 0.868953%
 #         Smead Lockers, Blue = 0.8073413%
 #         Hon Executive Leather Armchair, Adjustable = 0.7825465%
     
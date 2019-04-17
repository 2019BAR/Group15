
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(ggthemes)
library(wordcloud2)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, d3heatmap)
library(tidytext)
library(tidyr)
library(stringr)
library(stringi)


# 讀資料
# read data
order = fread("olist_orders_dataset.csv") # 訂單與運送狀態
review = fread("olist_order_reviews_dataset11.csv", encoding = "UTF-8") # 商品滿意度調查
payment = fread("olist_order_payments_dataset.csv") # 付款方式
item = fread("olist_order_items_dataset.csv") # 訂單價值
customer = fread("olist_customers_dataset.csv") # 客戶資料
product = fread("olist_products_dataset.csv") # 產品資訊
seller = fread("olist_sellers_dataset.csv") # 賣家資訊
geolocation = fread("olist_geolocation_dataset.csv") # 巴西郵遞區號與地理位置
name = fread("product_category_name_translation.csv") # 英文名



#訂單編號VS消費金額、運費以及總付款金額(order_money)
order_money=select(item,order_id,price,freight_value)
order_money=
  order_money%>%
  mutate(total= price + freight_value)%>%
  group_by(order_id)

#統整在一起
H_data = order_money
order_money = NULL

#訂單編號_購買(order_time)
order_time = select(order,order_id,order_purchase_timestamp)
order_time$order_purchase_timestamp=
  as.POSIXct(order_time$order_purchase_timestamp, 
             format="%Y-%m-%d %H:%M:%S")
order_time$month=format(order_time$order_purchase_timestamp,format="%Y-%m")

#統整在一起
H_data = merge(H_data, order_time, by="order_id")
order_time =NULL



#訂單編號_類別(order_cato)
tras = select(product, product_id ,product_category_name)
product_cato = select(item ,order_id ,product_id)
order_cato = merge( tras, product_cato ,by = "product_id", na.rm=TRUE)
order_cato = 
  order_cato%>%
  merge( name , by="product_category_name", na.rm=TRUE)%>%
  select(order_id,product_category_name_english)

#統整在一起
H_data = merge(H_data, order_cato, by="order_id")
H_data$count = 1
order_cato =NULL
tras=NULL
product_cato=NULL



# #分析
# ###各個類別購買次數跟購買總金額
# #購買金額最高的前六個分別是：
# bed_bath_table,health_beauty,computer_accessories,
# furniture_decor,watches_gifts,sports_leisure
# 
# #購買次數最多的前六個分別是：
# bed_bath_table,furniture_decor,health_beauty,
# computer_accessories, sports_leisure,horsewares


money_cato =
  H_data%>%
  select(product_category_name_english,total,count)%>%
  group_by(product_category_name_english) %>% 
  summarise(total = sum(total),count=sum(count))
money_cato_t = arrange(money_cato,desc(total))
money_cato_t =money_cato_t[1:6, ]
money_cato_c = arrange(money_cato,desc(count))
money_cato_c =money_cato_c[1:6, ]
money_cato_t$product_category_name_english
money_cato_c$product_category_name_english

money_cato_t = NULL
money_cato_c = NULL


###季成長趨勢


library(ggplot2) 
K = H_data
K=
  K%>%
  group_by(month)%>%
  summarise(total = sum(total),price = sum(price))
K$season =1
K$season[1:3]="16-4"
K$season[4:6]="17-1"
K$season[7:9]="17-2"
K$season[10:12]="17-3"
K$season[13:15]="17-4"
K$season[16:18]="18-1"
K$season[19:21]="18-2"
K$season[22:24]="18-3"

L = K%>%
  group_by(season)%>%
  summarise(total = sum(total),price = sum(price))

qplot(x=month,y=total,data=K,
      color = rgb(255, 233, 139, maxColorValue = 255),size=30) +
  labs(x = "日期",y = "金額",title = "總金額月趨勢圖")+ guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

qplot(x=month, y=price,data=K,
      color = rgb(94, 199, 245, maxColorValue = 255),size=30)+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "日期",y = "金額",title = "消費金額月趨勢圖")+ guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

qplot(season,total,data=L,
      color = rgb(255, 233, 139, maxColorValue = 255) ,size=60)+
  labs(x = "日期",y = "金額",title = "總金額季趨勢圖")+ guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

qplot(season,price, data=L,
      color= '#5ECAFD',size=100)+
  labs(x = "日期",y = "金額",title = "消費金額季趨勢圖")+  guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

K=L=O=P=NULL

time_cat_mon  = filter(H_data, 
                       product_category_name_english == "bed_bath_table"| 
                         product_category_name_english == "health_beauty" | 
                         product_category_name_english == "computers_accessories"| 
                         product_category_name_english =="furniture_decor"| 
                         product_category_name_english =="watches_gifts"| 
                         product_category_name_english =="sports_leisure"| 
                         product_category_name_english =="housewares" )
ggplot()+
  geom_bar(data = time_cat_mon,
           aes(x=month,y=total, fill = product_category_name_english),
           stat = "identity")+ 
  labs(x = "日期",y = "金額",title = "總金額月趨勢圖")+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=30)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))+ 
  theme(legend.text = element_text(size=20))

ggsave("plot.pdf", width = 4, height = 3)
# 
# K=
#   time_cat_mon%>%
#   group_by(month,product_category_name_english)%>%
#   summarise(total = sum(total),count=sum(count))
# K$product_category_name_english=as.factor(K$product_category_name_english)
# 
# ggplot(data = K,group = 1)+
#   geom_line(
#     aes(x=month,y=count,color = product_category_name_english)
#   ) + 
#   labs(x = "日期",y = "金額",title = "總金額月趨勢圖")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# 
# ###特定類別成長趨勢
# ####bed_bath_table
# 
# par(mfrow=c(2,3))
# ```{r}
#床
K=
  H_data%>%
  filter(product_category_name_english=="bed_bath_table")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col='red',size=60)
  ) +
  geom_abline(x=month,y=total,col='red')+
  labs(x = "日期",y = "金額",title = "bed_bath_table月趨勢圖")+ guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

```{r}
#美妝
K=
  H_data%>%
  filter(product_category_name_english=="health_beauty")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col="orange",size=60)
  ) +
  labs(x = "日期",y = "金額",title = "health_beauty月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))
```

```{r}
#電腦
K=
  H_data%>%
  filter(product_category_name_english=="computers_accessories")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,color ='yellow',size=60)
  ) +
  labs(x = "日期",y = "金額",title = "computers_accessories月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))
#furniture_decor
K=
  H_data%>%
  filter(product_category_name_english=="furniture_decor")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col = "green",size=60)
  ) +
  labs(x = "日期",y = "金額",title = "furniture_decor月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))
```{r}
#watches_gifts
K=
  H_data%>%
  filter(product_category_name_english=="watches_gifts")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col= 'blue',size=60)
  ) +
  labs(x = "日期",y = "金額",title = "watches_gifts月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

```{r}
#sports_leisure
K=
  H_data%>%
  filter(product_category_name_english=="sports_leisure")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col = 'purple',size=60)
  ) +
  labs(x = "日期",y = "金額",title = "sports_leisure月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))

```{r}
#housewares
K=
  H_data%>%
  filter(product_category_name_english=="housewares")%>%
  group_by(month)%>%
  summarise(total = sum(total))

ggplot(data = K)+
  geom_point(
    aes(x=month,y=total,col='pink',size=150)
  ) +
  labs(x = "日期",y = "金額",title = "housewares月趨勢圖")+guides(color=FALSE,size=FALSE)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle = -60,size=20)) +
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.y = element_text(size=30)) +
  theme(title = element_text(size=30))




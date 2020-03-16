

library(readr)
library(dplyr)
library(plyr)

aisles <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/aisles.csv")
departments <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/departments.csv")
order_products__train <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/order_products__train.csv")
order_products__prior <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/order_products__prior.csv")
orders <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/orders.csv")
products <- read_csv("C:/Users/kabir/OneDrive/All Project/instacart_online_grocery_shopping/Raw Data/products.csv")

#check
order17=filter(orders,order_number=="17")
order17
#check

dim(order_products__train)
dim(order_products__prior)
order_products_all=rbind(order_products__train,order_products__prior)
dim(order_products_all)

dim(products)
dim(aisles)
pr_as <- left_join(products,aisles, by = c("aisle_id"="aisle_id"))
dim(pr_as)
head(pr_as)

dim(departments)
pr_as_dp <- left_join(pr_as,departments, by = c("department_id"="department_id"))
dim(pr_as_dp)
head(pr_as_dp)


#oa_or <- left_join(order_products__all,orders, by = c("order_id"="order_id"))
oa_or <- left_join(orders,order_products_all, by = c("order_id"="order_id"))

dim(oa_or)


#pr_as_dp_oa_or=left_join(pr_as_dp,oa_or,by = c("product_id"="product_id"))
pr_as_dp_oa_or=left_join(oa_or,pr_as_dp,by = c("product_id"="product_id"))

dim(pr_as_dp_oa_or)




testuser=filter(pr_as_dp_oa_or,user_id=="1540")
testuser

testuser1=filter(pr_as_dp_oa_or,user_id=="36855")
testuser1
class(testuser)

testuser2=filter(pr_as_dp_oa_or,eval_set=="test")
testuser2

write.csv(testuser1, file = "test1.csv")




poisson.process=function(lamda,time,k){
  t=(lamda*time)^k
  t1=exp(-lamda*time)
  t2=factorial(k)
  result=(t*t1)/t2
  return(result)
} 

poisson.process(3/42,10,1)

#-----------------------------
rm(futurecart)
futurecart=data.frame()
futurecart[1,1]=1
futurecart[1,2]=1
futurecart[1,3]=1
futurecart[1,4]=1
futurecart

tu=filter(orders,eval_set=="test" )
t=unique(tu$user_id)


i=1
for (var1 in 1:75000) {
  v2=filter(pr_as_dp_oa_or,user_id==tu$user_id[var1])
  v3=unique(v2$product_name)
  for (var2 in 1:length(v3)){
    testday=(filter(v2,eval_set=="test"))
    time=testday$days_since_prior_order
    v4=filter(v2,product_name==v3[var2])
    time1=sum(v4$days_since_prior_order,na.rm = TRUE)
    count1=nrow(v4)
    lamda=count1/time1
    k=1
    futurecart[i,1]=tu$user_id[var1]
    futurecart[i,2]=testday$order_id
    futurecart[i,3]=v3[var2]
    futurecart[i,4]=poisson.process(lamda,time,1)
    i=i+1
  }
  
}






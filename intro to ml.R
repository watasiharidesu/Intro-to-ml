library(dplyr)
library(caret)

tr = train
te = test

tr_n = tr[,sapply(tr, is.numeric)]
tr_c = tr[,sapply(tr, is.character)]

nearZ = checkConditionalX(tr_n, factor(tr_n$target))
col_ct = sapply(tr_c, function(x) length(unique(x)))

y = tr$target
y = data.frame(count = y)
y$color = NA
y$color[y$count == 1] = "1"
y$color[y$count == 0] = "0"
y$count = as.numeric(y$count)

tr = tr[, -nearZ]
te = te[, -nearZ]

nearZ

tr = tr[, !names(tr) == "ID"]
tr = tr[, !names(tr) %in% names(col_ct[col_ct==1])]
te = te[, !names(te) == "ID"]
te = te[, !names(te) %in% names(col_ct[col_ct==1])]

tr_m = tr[1,]
for (i in 1:ncol(tr)){
  tr_m[1,i] = mean(tr[,i] == "NA") + mean(tr[,i] == "[]") + mean(tr[,i] == "") 
}

mi = names(tr_m)[colSums(tr_m > 0.5) > 0]

tr = tr[, !names(tr) %in% mi]
te = te[, !names(te) %in% mi]

tr[tr==""] = -1
tr[tr=="[]"] = -1
tr[tr=="NA"] = -1
te[te==""] = -1
te[te=="[]"] = -1
te[te=="NA"] = -1

one = rep(0,ncol(tr))
one = data.frame(one)

for(i in 1:ncol(tr)){
  n = sum(tr[,i] == -1)
  one[i,] = n
}

summary(one)

names(one) = "m"
one$p = one$m / nrow(tr)

ones = one[one$p == 0, ]

summary(ones)

ggplot(ones, aes(x = p)) + geom_histogram()


te_n = te[,sapply(te, is.numeric)]
write.csv(te_n, "te.csv")

tr_n = tr[,sapply(tr, is.numeric)]
tr_c = tr[,sapply(tr, is.character)]


tr_date = tr_c[,grep("JAN1|FEB1|MAR1", tr_c),]
tr_c = tr_c[, !colnames(tr_c) %in% colnames(tr_date)]
tr_date = sapply(tr_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))

num = sapply(tr_n, function(x) length(unique(x)))
cat = sapply(tr_c, function(x) length(unique(x)))
dat = sapply(tr_date, function(x) length(unique(x)))

summary(num)
summary(cat)
summary(dat)

num_d = data.frame(count = num, type = "Numerical")
cat_d = data.frame(count = cat, type = "Categorical")
dat_d = data.frame(count = dat, type = "Date")

num_sm = cat_d[cat_d$count == 2,]
this = num_sm %>% row.names()

tr = tr[, !names(tr) %in% this]

cat_d[cat_d$count == 3,]

all = rbind(data.frame(count=num, type="Numerical"), 
               data.frame(count=cat, type="Character"), 
               data.frame(count=dat, type="Date")) 

y2 = data.frame(type = c(0, 1), percentage = c(76.74532, 23.25468), target = c("0", "1"))
ggplot() + geom_bar(aes(y = percentage, x = 0, fill = target), data = y2, stat="identity") + 
  geom_text(data = y2, aes(x = 0, y = percentage-5, label = paste0(percentage,"%")))

ggplot(num_d, aes(x = count, fill=type)) + 
  geom_histogram(binwidth = 1, alpha = 0.9, position="dodge") + 
  xlab("Unique values per feature (0-100)") + theme(legend.position = "none") +
  xlim(c(0,100)) + theme(axis.title.x=element_text(size=12, ,face="bold"))

ggplot(cat_d, aes(x = count, color=type)) + 
  geom_histogram(binwidth = 1, alpha = 0.9, position="dodge") + 
  xlab("Unique values per feature (0-10)") + theme(legend.position = "none") +
  xlim(c(0,10)) + theme(axis.title.x=element_text(size=12, ,face="bold"))



ggplot (data=tr, aes(x=target,y=VAR_0538, colour=target, group = target)) + geom_boxplot()

names(tr_n)

set.seed(123)

tr_nt = cbind(tr_n, y)
write.csv(tr_nt,"trnt.csv")


fit = lm(target~., trs)
su = summary(fit)$coefficients
su = as.data.frame(su)
sus = su[order(su$Estimate),]
sus %>% head(20)

bi = c("VAR_1673", "VAR_1677", "VAR_1008", "VAR_1000", "VAR_0989", "VAR_1688", 
       "VAR_1693", "VAR_1675", "VAR_1603", "VAR_1604")

ggplot (data=tr, aes(x=target,y=VAR_1673, colour=target, group = target)) + geom_boxplot()

trf = train[,c(bi,"target")]
write.csv(trf, "trf.csv")



tef = test[,bi]
write.csv(tef, "tef.csv")



for (i in 1:ncol(tr_c)){
  if(length(tr_c[,i] %>% unique) == 4){
    print(names(tr_c)[i])
  }
}


#包####
library(rvest)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(gridExtra)
library(randomForest)
library(showtext)
#安居客数据爬取####
basicURLA <-"https://wx.zu.anjuke.com/fangyuan"
WebSpiderA <- function(m){
  url <- str_c(basicURLA,"/p",m)
  web <- read_html(url,encoding = 'uft-8')
  
  title <- web %>% 
    html_nodes("address") %>%
    html_text() #获取小区名
  lab <- web %>% 
    html_nodes("h3 a") %>%
    html_text() #获取信息
  mes <- web %>% 
    html_nodes(".tag") %>%
    html_text() #获取信息
  pri <-web %>% 
    html_nodes("strong") %>%
    html_text() #获取price
  
  data.frame(title,lab,mes,pri)
} 
resultsA <- data.frame()
for(m in 1:100){ #100为小区信息的总页码 
  resultsA <- bind_rows(resultsA,WebSpiderA(m))#合并每一次循环输出的数据
}
resultsA$title<-str_replace_all(resultsA$title," ","")

write.csv(resultsA,file = 'anjuke.csv')

#Excel数据清洗后，数据读取、观测####
ajk <- fread("anjuke.csv",encoding = 'UTF-8')
summary(ajk)
glimpse(ajk)
sapply(ajk,function(x)sum(x==''))
#去除商铺、重复房源
ajk<-ajk[-1591,]
ajk <- ajk[-1721,]
ajk <-ajk[-14,]
ajk <-ajk[-1682,]
ajk <- ajk[,-1]
ajk<- unique(ajk,fromLast = T) #去重
summary(ajk)#712个小区、2470条
#转因子
ajk <-ajk %>%
  mutate(
    title = factor(title),
    dis = factor(dis),
    com = factor(com),
    htype = factor(htype),
    rty = factor(rty),
    fit = factor(fit),
    sumflo = factor(sumflo)
     )
glimpse(ajk)
#小区名分析####

nlevels(ajk$title)

tita <- ajk %>%
  group_by(title,dis) %>%
  count() %>%
  arrange(desc(n))
tit30 <- as.data.frame(tita[1:30,])
#房源数量前30的小区名

pa1 <- 
  tita[1:30,] %>%
  ggplot(aes(reorder(title,n),y=n,fill=n)) +
  geom_bar(stat = 'identity') + coord_flip() +
  labs(x = '',y = '',title ='可租房源数量前30的小区',legend='ss') + theme_light(base_size = 13) +
  geom_text(aes(label = n),hjust = -0.20 ,size =4)
pa1

#租房类型分布####
pa2<- ajk %>%
  ggplot(aes(x = rty,fill = rty)) +
  geom_bar() + 
  theme(legend.position = 'null') +
  labs(x='',y='',title='租房类型数量分布') +
  scale_fill_brewer(palette = 'Reds')
pa2

ajk %>%
  group_by(rty) %>%
  count() %>%
  ggplot(aes(x=rty,y = n,fill=rty)) +geom_bar(stat = 'identity') + scale_y_sqrt() +
  geom_text(aes(label = n), size = 5,vjust=-.3) + theme(axis.text.x  = element_text(size = 12, vjust = 0.5, hjust = 0.5
  )) + 
  theme(legend.position = 'null') + labs(x='',y='',title='租房类型数量分布')

#户型分布####
pa3 <- ajk %>%
  ggplot(aes(reorder(htype,rep(1,length(htype)),sum),fill=htype)) +
  geom_bar() + theme(legend.position = 'null') + coord_flip() +
  labs(x='',y='',title='户型数量分布') + theme(axis.text.y  = element_text(size = 12, vjust = 0.5, hjust = 0.5
  )) 
pa3
#房源所在楼层分布####
ajk$flo <- as.factor(ajk$flo)
pa4<-ajk %>%
  ggplot(aes(flo)) + 
  geom_bar(fill='orange') +theme_hc() +labs(x='',y='',title='房源所在楼层分布')
#房源总楼层分布####
ajk1 <-ajk %>%
  mutate(
    sumflo = factor(sumflo,ordered = T,
    levels = c('1层','2层','3层','4层','5层','6层','7层','8层','9层','10层','11层','12层','13层','14层',
              '15层','16层','17层','18层','19层','20层','21层','22层','23层','24层','25层','26层','27层','28层','29层','30层'
                      ,'31层','32层','33层','34层','35层',
                                  '36层','37层','38层','39层','40层'
                                  ,'43层','45层',
                                  '48层','50层',
                                  '51层','52层','53层','65层')))

pa5 <- ajk1 %>%
  ggplot(aes(sumflo)) +
  geom_histogram(stat="count",fill ='skyblue') +theme_hc() +
  labs(x='',y='',title='可租房源总层数分布') + theme(axis.text.x = element_text(angle =75, hjust =1))

pa45 <- grid.arrange(pa4,pa5,ncol=1)
#整体价格####
summary(ajk$pri)  
pa6 <- ajk %>%
  ggplot(aes(pri)) +
  geom_histogram(bins = 50,fill="red") + scale_x_log10() + 
  labs(x='租金',y='数量',title='整体租金分布')+ theme_hc()
ajk %>%
  ggplot(aes(pri)) + geom_density() + scale_x_log10()


#区域房源分布####

pa7 <- ajk %>%
  group_by(dis) %>%
  count() %>%
  ggplot(aes(dis,n,fill =dis)) +
  geom_col() +theme_hc() + labs(x='',y='',title='区域房源分布') + 
  geom_text(aes(label= n,vjust=-0.5)) + scale_fill_brewer(palette = 'Reds')

#商圈房源分布####
nlevels(ajk$com)
pa8<- ajk %>%
  ggplot(aes(reorder(com,rep(1,length(com)),sum),fill=com)) +
  geom_bar() +theme(legend.position = 'null') + coord_flip() + 
  labs(x='',y='数量',title='商圈房源分布') +
  theme(axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5
                                   ))
#房源装修情况####
nlevels(ajk$fit)
table(ajk$fit)
ajk1 <- ajk %>%
  mutate(fit = factor(fit,order = T,levels = c('毛坯','简单装修','中等装修','精装修'
                                               ,'豪华装修')))
pa9 <- ajk1 %>%
  ggplot(aes(fit,fill = fit))+ geom_bar() + 
  labs(x='',y='数量',title='房源装修情况分布') + scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = 'NULL') + theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5
  )) + theme(axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5
  ))
#租金分析#####
#前30大小区租金分布情况
ajktbl <- tbl_df(ajk)
tita <- as.data.frame(tita)
ajktbl$tcn <- apply(ajktbl,1,function(x) tita[which(tita['title'] == x['title']),2])


ajktbl1 <- ajktbl %>%
  arrange(desc(tcn),title) %>%
  select(title,pri,tcn,dis)

tit11 <- ajktbl1[1:587,] %>%
  group_by(title) %>%
  summarise(mean=mean(pri),median=median(pri),sd=sd(pri))


pa10 <- ajktbl1[1:600,] %>%
  filter(title != '万达公寓') %>%
  ggplot(aes(title,pri,fill = title)) +
  geom_boxplot() +
  scale_y_sqrt() + labs(x='',y='租金') +
  labs(x='',y='租金',title='') +
  theme_wsj() + theme(axis.text.x = element_text(size=13,face = 'bold',angle = 80, hjust =1)) +
  theme(legend.position = "none")
  
#租房类型看看价格
pa11 <- ajk %>%
  ggplot(aes(rty,pri,fill = rty)) +
  geom_boxplot() +scale_y_log10() + theme_light(base_size = 13) + labs(x='',y='租金',title='') 

ajk%>%
  ggplot(aes(pri,fill=rty)) + scale_x_log10() + scale_fill_brewer(palette = 'Set1') +
  geom_density(colour='black',size=1.25) + theme_hc()



#户型看价格
pa12 <- ajk %>%
  ggplot(aes(htype,pri,fill=htype)) +
  geom_boxplot() + theme_wsj() +
  theme(axis.text.x = element_text(face = 'bold',angle = 80, hjust =1)) +
  theme(legend.position = 'none') +scale_y_log10() + labs(x='',y='租金')
pa12
table(ajk$htype)

#所在楼层价格分布
pa13 <- ajk %>%
  ggplot(aes(flo,pri,fill=flo)) + theme(legend.position = 'null') +
  geom_boxplot() + scale_y_log10() + labs(x='',y='租金',title='房源所在楼层租金分布')
#总楼层价格分布

pa14 <- ajk1 %>%
  ggplot(aes(sumflo,pri,fill=sumflo)) +
  geom_boxplot() + theme(legend.position = 'null') + scale_y_log10() +
  theme(axis.text.x = element_text(face = 'bold',angle = 80, hjust =1)) +
  labs(x='',y='租金',title='房源所在总楼层租金分布')

grid.arrange(pa13,pa14,nrow=2)
#区位价格
pa15 <- ajk %>%
ggplot(aes(dis,pri, fill =dis)) +
  geom_boxplot() +scale_y_log10()  +
  theme(legend.position = 'null') + scale_fill_brewer(palette = 'Paired') +
  theme_wsj(base_size = 15) + labs(x='',y='租金',title = '') +
  theme(legend.position = 'none')

dis1 <- ajk %>%#每个区域平均价、中位数、方差
  group_by(dis) %>%
  summarise(mean=mean(pri),median=median(pri),sd=sd(pri))


#装修情况租金
fit1 <- ajk1 %>%
  filter(fit!='毛坯') %>%
ggplot(aes(fit,fill = fit))+ geom_bar() + 
  labs(x='',y='数量',title='房源装修情况分布') + scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = 'NULL') + theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5
  )) + theme(axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5
  ))


pa16 <- fit1 %>%  
  ggplot(aes(fit,pri,fill=fit)) +
  geom_boxplot() + scale_y_log10() + labs(x='',y='租金',title = '各装修情况的租金分布') +
  theme(legend.position = 'null') + theme_hc(base_size = 13)

ajk1 %>%
  group_by(fit) %>%
  summarise(mean=mean(pri),median=median(pri),sd=sd(pri))
#商圈租金
nlevels(ajk$com)

com1 <- ajk %>%
  group_by(com) %>%
  summarise(mean=mean(pri),median=median(pri),sd=sd(pri)) %>%
  arrange(mean)

pa17 <- ajk %>%
  ggplot(aes(com,pri,fill=com)) +
  geom_boxplot() +
  scale_y_log10() + labs(x='',y='',title='') + 
  theme_wsj() +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size=13,face = 'bold',angle = 80, hjust =1))

pa17
#RF模型#####
ajk$pri2[ajk$pri >2000 & ajk$pri <=3000] <- 2
ajk$pri2[ajk$pri <= 2000 & ajk$pri>1000] <-1
ajk$pri2[ajk$pri >0 &ajk$pri <=1000] <-0
ajk$pri2[ajk$pri >3000 & ajk$pri <=4000] <- 3
ajk$pri2[ajk$pri >4000] <- 4
ajk$pri2 <- as.factor(ajk$pri2)

ajk$flo <- as.numeric(ajk$flo)
ajk$tcnt[ajk$flo>=1 & ajk$flo <=10] <- '低层'
ajk$tcnt[ajk$flo>=11 & ajk$flo <=20] <- '中层'
ajk$tcnt[ajk$flo>=21 & ajk$flo <=30] <- '高层'
ajk$tcnt[ajk$flo>=31 ] <- '超高层'
ajk$tcnt <- as.factor(ajk$tcnt)

ajk$sumflo2 <- gsub('层','',ajk$sumflo)
ajk$sumflo2 <- as.numeric(ajk$sumflo2)
ajk$tcnt2[ajk$sumflo2>=1 & ajk$sumflo2 <=10] <- '低层'
ajk$tcnt2[ajk$sumflo2>=11 & ajk$sumflo2 <=20] <- '中层'
ajk$tcnt2[ajk$sumflo2>=21 & ajk$sumflo2 <=30] <- '中高层'
ajk$tcnt2[ajk$sumflo2>=31 & ajk$sumflo2 <=40] <-'高层'
ajk$tcnt2[ajk$sumflo2 > 40] <- '超高层'
ajk$tcnt2 <- as.factor(ajk$tcnt2)

modela2 <- randomForest(as.factor(pri2) ~ com+htype+rty+fit+tcnt+tcnt2,data = ajk,ntree
                       =1500)
importance3 <- importance(modela2)
varImportance3 <- data.frame(Variables = row.names(importance3), 
                             Importance = round(importance3[ ,'MeanDecreaseGini'],2))

fix(varImportance3)
p22<- varImportance3 %>%
  ggplot(aes(reorder(Variables,Importance),Importance,fill =Variables)) +scale_fill_brewer(palette = 'Blues') +
  geom_col() +coord_flip() +labs(x='',y='重要性') +theme_few(base_size = 13)

plot(modela2)
print(modela2)
#图表美化#####

#区域、租房类型、价格
ajk %>%
  ggplot(aes(rty,pri,fill=dis)) + scale_y_log10() +
  geom_boxplot() + ggtitle('') +
  theme_calc(base_size = 15) +
  scale_fill_calc() +
  guides(fill=guide_legend(title= NULL)) + 
  facet_grid(.~dis) + labs(x='',y='')

#前30房源
tita[1:30,] %>%
  ggplot(aes(reorder(title,n),y=n,fill=n)) +
  geom_bar(stat = 'identity') + coord_flip() +
  labs(x = '',y = '',title ='可租房源数量前30的小区') + 
  theme_tufte(base_size = 13) +
  geom_text(aes(label = n),hjust = -0.20 ,size =4) +
  theme(legend.position = 'none') +
  theme(text=element_text(size = 13,face = 'bold',family = "arial"))
# 辖区
ajk %>%
  group_by(dis) %>%
  count() %>%
  ggplot(aes(dis,n,fill =dis)) +
  geom_col() +theme_hc() + labs(x='',y='',title='区域房源分布') + 
  geom_text(aes(label= n,vjust=-0.5)) + scale_fill_brewer(palette = 'Reds') +
  theme(legend.position = 'none') +
  theme(text = element_text(size = 18,face = 'bold'))
#商圈分布

ajk %>%
  group_by(com) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(com,n),n,fill=com)) +
  geom_col() + coord_flip() +
  theme(legend.position = 'none') +
  labs(x='',y='数量',title='商圈房源分布') +
  theme_tufte() + theme(legend.position = 'none') +
  geom_text(aes(label = n,hjust=-0.1,vjust=0.5)) +
  theme(text = element_text(size = 15,face = 'bold'))
#租房类型分布
ajk %>%
  group_by(rty) %>%
  count() %>%
  ggplot(aes(x=rty,y = n,fill=rty)) +
  geom_bar(stat = 'identity') + 
  scale_y_sqrt() +
  geom_text(aes(label = n), size = 5,vjust=-.3) + 
  theme(axis.text.x  = element_text(size = 12, vjust = 0.5, hjust = 0.5
  )) +  theme_hc() +
  theme(legend.position = 'none') + 
  labs(x='',y='',title='租房类型数量分布') +
  scale_fill_hc() +
  theme(text = element_text(face = 'bold',size = 15))

#户型分布
ajk %>%
  group_by(htype) %>%
  count() %>%
  ggplot(aes(reorder(htype,n),n,fill=htype)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(x='',y='') +
  theme_tufte() + theme(legend.position = 'none') +
  geom_text(aes(label=n,hjust=-0.1,vjust=0.5)) +
  theme(text = element_text(face = 'bold',size = 15)) +
  ggtitle('房源户型数量分布')

#其它
ajk1 %>%
  filter(fit!='毛坯') %>%
  group_by(fit) %>%
  count() %>%
  ggplot(aes(fit,n,fill = fit))+ geom_bar(stat = 'identity') + 
  labs(x='',y='数量',title='') + 
  theme_wsj() +
  theme(legend.position = 'none') + 
  theme(text = element_text(face = 'bold',size = 15)) +
  geom_text(aes(label=n,vjust=-0.5)) +
  scale_fill_wsj()
#
pp1 <- ajktbl1[1:587,] %>%
  filter(dis == '滨湖区') %>%
  ggplot(aes(title,pri,fill=title)) +
  geom_boxplot()  +
  theme_wsj() + theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 80, hjust =1)) +
  theme(text = element_text(face = 'bold',size = 15)) +
  labs(x='',y='')

pp2 <- ajktbl1[1:587,] %>%
  filter(dis == '梁溪区') %>%
  ggplot(aes(title,pri,fill=title)) +
  geom_boxplot()  +
  theme_wsj() + theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 80, hjust =1)) +
  theme(text = element_text(face = 'bold',size = 15)) +
  labs(x='',y='')

pp3 <- ajktbl1[1:587,] %>%
  filter(dis == '新吴区') %>%
  ggplot(aes(title,pri,fill=title)) +
  geom_boxplot()  +
  theme_wsj() + theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 80, hjust =1)) +
  theme(text = element_text(face = 'bold',size = 15)) +
  labs(x='',y='')

pp4 <- ajktbl1[1:587,] %>%
  filter(dis == '惠山区') %>%
  ggplot(aes(title,pri,fill=title)) +
  geom_boxplot()  +
  theme_wsj() + theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 80, hjust =1)) +
  theme(text = element_text(face = 'bold',size = 15)) +
  labs(x='',y='')
grid.arrange(pp1,pp2,pp3,pp4,ncol=2)

ajaj <- ajk %>%
  filter(fit != '毛坯') %>%
  mutate(fit = factor(fit,order = T,levels = c('毛坯','简单装修','中等装修','精装修'
                                               ,'豪华装修'))
ajaj %>% 
  ggplot(aes(fit,pri,fill=dis)) + scale_y_log10() +
  geom_boxplot() + ggtitle('') +
  theme_wsj(base_size = 15) +
  scale_fill_wsj() + theme(axis.text.x = element_text(angle = 80, hjust =1)) +
  theme(legend.position = 'none') + 
  facet_grid(.~dis) + labs(x='',y='')

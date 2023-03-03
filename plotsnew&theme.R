
##--Data#####
#1. install packages
install.packages("ggplot2")
install.packages("data.table")
install.packages("gghighlight")

#2. Read libraries
library(data.table)
library(ggplot2)
library(gghighlight)

#3. Opening Data

areas<-c("029","453","113","201","439") #Bexar, Travis, Dallas, Harris, Tarrant
areas_names<-c("Bexar", "Travis", "Dallas", "Harris", "Tarrant")
naics_codes<-fread("Task 1/Employment/industry-titles-csv.csv")

EMP_totals_monthly<-fread("Task 1/Employment/Employment_raw.csv")

#4. Filter Industry Codes

EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("3254", "5417"),] #biosciences

EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("5415", "518","5112"),] #IT

EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("2361", "2362", "2373"),] #Construction

#5. Cleaning and filtering data

vars_keep<-names(EMP_totals_monthly)[c(10:12,16,19:21,25)]
vars_id<-c("area_fips","industry_code","own_code","year","qtr")
EMP_totals_monthly<-EMP_totals_monthly[,c(vars_id,vars_keep),with=F]
EMP_totals_monthly<-melt.data.table(EMP_totals_monthly,id.vars =vars_id ,measure.vars = vars_keep)

EMP_totals_monthly[grepl(pattern = "month1",x = variable)==T & qtr==1,month:=1]
EMP_totals_monthly[grepl(pattern = "month2",x = variable)==T & qtr==1,month:=2]
EMP_totals_monthly[grepl(pattern = "month3",x = variable)==T & qtr==1,month:=3]
EMP_totals_monthly[grepl(pattern = "month1",x = variable)==T & qtr==2,month:=4]
EMP_totals_monthly[grepl(pattern = "month2",x = variable)==T & qtr==2,month:=5]
EMP_totals_monthly[grepl(pattern = "month3",x = variable)==T & qtr==2,month:=6]
EMP_totals_monthly[grepl(pattern = "month1",x = variable)==T & qtr==3,month:=7]
EMP_totals_monthly[grepl(pattern = "month2",x = variable)==T & qtr==3,month:=8]
EMP_totals_monthly[grepl(pattern = "month3",x = variable)==T & qtr==3,month:=9]
EMP_totals_monthly[grepl(pattern = "month1",x = variable)==T & qtr==4,month:=10]
EMP_totals_monthly[grepl(pattern = "month2",x = variable)==T & qtr==4,month:=11]
EMP_totals_monthly[grepl(pattern = "month3",x = variable)==T & qtr==4,month:=12]


EMP_totals_monthly[grepl(pattern = "emplvl",x = variable),variable2:='Employment level']
EMP_totals_monthly[grepl(pattern = "avg_wkly_wage",x = variable),variable2:='Ave. Weekly Wage']
EMP_totals_monthly[grepl(pattern = "lq",x = variable),variable2:='LQ employment']
EMP_totals_monthly[grepl(pattern = "lq_avg",x = variable),variable2:='LQ ave. wage']

EMP_totals_monthly[,County:=factor(x = area_fips,labels = areas_names)]
EMP_totals_monthly<-merge(EMP_totals_monthly,naics_codes,by="industry_code")

#6. Set key for

#employment
setkey(EMP_totals_monthly,year,month)
EMP_totals_monthly[,year_label:=as.character(year)]
EMP_totals_monthly[month!=1,year_label:=""]

#wages
setkey(EMP_totals_monthly,year,qtr)
EMP_totals_monthly[,year_label:=as.character(year)]
EMP_totals_monthly[qtr!=1,year_label:=""]

##---Theme#####
theme_EDR<-  theme_classic()+
  theme( plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         legend.title = element_blank(),
         legend.position = "",
         legend.justification = "left",
         axis.text.y = element_text(family = "Helvetica Neue"))

##--Biosciences#####

EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("3254", "5417"),]

#2 
bio <-EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==3254 & County=="Bexar",]$year_label

#3. Employment Plot

bb<-ggplot(data = EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(month,year),y=value,group=County))+
  geom_line(color='#0C2340')+
  geom_point(aes(shape=County), color = '#F15A22',size=1)+
  scale_x_discrete(label=bio)+
  facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
  labs(title = "", subtitle = "" ,x="",y="Number of employed")+
  gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
  theme_EDR

plot(bb)


ggsave(filename = 'BioEmployment.png',plot = bb, w = 6, h =7, units = "in")



#4. Wages Plot

biowage <-EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==3254 & County=="Bexar",]$year_label

bb2<-ggplot(data = EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(qtr,year),y=value,group=County))+
  geom_line(color='#0C2340')+
  geom_point(aes(shape=County), color = '#F15A22',size=1)+
  scale_x_discrete(label=biowage)+
  facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
  labs(title = "", subtitle = "" ,x="",y="Average Wages")+
  gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
  theme_EDR
plot(bb2)

ggsave(filename = 'BioWage.png',plot = bb2, w = 6, h =7, units = "in")

##--IT#####

#1 Industry codes
EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("5415", "518","5112"),]

#2 
itemp <-EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==518 & County=="Bexar",]$year_label

#3. Employment Plot

ii<-ggplot(data = EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(month,year),y=value,group=County))+
  geom_line(color='#0C2340')+
  geom_point(aes(shape=County), color = '#F15A22',size=1)+
  scale_x_discrete(label=itemp)+
  facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
  labs(title = "", subtitle = "" ,x="",y="Number of employed")+
  gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
  theme_EDR

plot(ii)
 
ggsave(filename = 'ITEmployment.png',plot = ii, w = 6, h =7, units = "in")

#4. Wages Plot

itwage <-EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==518 & County=="Bexar",]$year_label

ii2<-ggplot(data = EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(qtr,year),y=value,group=County))+
  geom_line(color='#0C2340')+
  geom_point(aes(shape=County), color = '#F15A22',size=1)+
  scale_x_discrete(label=itwage)+
  facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
  labs(title = "", subtitle = "" ,x="",y="Average Wages")+
  gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
  theme_EDR
plot(ii2)

ggsave(filename = 'ITWage.png',plot = ii2, w = 6, h =7, units = "in")

 ##--Construction#########

#1.Industry codes

EMP_totals_monthly<-EMP_totals_monthly[industry_code%in%c("2361", "2362", "2373"),]

#2 
consemp <-EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==2362 & County=="Bexar",]$year_label

#3. Employment Plot
tt<-ggplot(data = EMP_totals_monthly[variable2=="Employment level" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(month,year),y=value,group=County))+
  geom_line(color='#0C2340')+
  geom_point(aes(shape=County), color = '#F15A22',size=1)+
  scale_x_discrete(label=consemp)+
  facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
  labs(title = "Employment Levels", subtitle = "Construction" ,x="",y="Number of employed")+
  gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
  theme_EDR
 
  plot(tt)
  ggsave(filename = 'ConstEmp.png',plot = tt, w = 6, h =7, units = "in")
  
  #4. Wages Plot
  
conswage<-EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5 &industry_code==2361 & County=="Bexar",]$year_label
  
  tt2<-ggplot(data = EMP_totals_monthly[variable2=="Ave. Weekly Wage" & year>=2019 & County%in%c("Bexar", "Travis") & own_code==5,],aes(x=interaction(qtr,year),y=value,group=County))+
    geom_line(color='#0C2340')+
    geom_point(aes(shape=County), color = '#F15A22',size=1)+
    scale_x_discrete(label=conswage)+
    facet_wrap(~industry_title,scales = "free_y",ncol = 1)+
    labs(title = "", subtitle = "" ,x="",y="Average Wages")+
    gghighlight(County=="Bexar",use_direct_label = F,calculate_per_facet = T)+ 
    theme_EDR
plot(tt2)

ggsave(filename = 'ConstWage.png',plot = tt2, w = 6, h =7, units = "in")

# Levelized Cost Analysis
# Deposit with known data
# PBH May 2024


# Load deposits -----
source("Scripts/Supply Model/Prepare Deposit Data/01-LoadDepositData.R", encoding = "UTF-8")

url_fig <- "Figures/Deposit/%s.png"


# Filter -----
df <- df %>% 
  filter(!is.na(USD_pertonne_Li),!is.na(Investment_original))

df %>% dplyr::select(Resource_Type,USD_pertonne_Li,Investment_original) %>% 
  # group_by(Resource_Type) %>% 
  skimr::skim_without_charts()


# Levelized cost of extraction over lifetime -------

discount_rate=0.03
ramp_up=4 # years

df <- df %>% 
  mutate(depletion_rate=if_else(Resource_Type=="Hard Rock",20,33))

# levelized cost

# time period, ramp up, and discount rate
f.levelizedVector <- function(t,r,ramp){
  
  # create vector of size t
  vec <- rep(1/t,t)
  
  # do ramp up
  for (i in 1:ramp){
    vec[i]=vec[i]/4*i
  }
  
  # extend to complete
  while(sum(vec)<1){
    vec[length(vec)+1]=1/t
  }
  vec[length(vec)] = 1-sum(vec[-length(vec)])
  # sum(vec)
  
  # discount
  discount <- 1:length(vec)
  discount <- 1/(1+r)^discount
  
  # multiplier to the marginal cost
  return(sum(discount*vec))
}
f.levelizedVector(33,0.03,4)
f.levelizedVector(20,0.03,4)


# Analysis on levelized vector

## Sensitivity -------
sens <- expand.grid(r1=c(0.03,0.07,0.1),ramp1=c(4,6,8),t1=c(20,33,50))

levelizedFactor <- c()
for (i in 1:nrow(sens)){
  levelizedFactor[i]=f.levelizedVector(sens$t1[i],sens$r1[i],sens$ramp1[i])
  }
sens$levelizedFactor = levelizedFactor
sens$ramp1 = paste0("Ramp ",sens$ramp1," years")  

ggplot(sens,aes(factor(t1),levelizedFactor,fill=factor(r1)))+
  geom_col(position=position_dodge())+
  facet_wrap(~ramp1)+
  labs(y="",x="Time period [years]",title="Multiplier to Marginal cost",fill="Discount \nrate")

## Existing mines using max depletion rates ------------

levelizedFactor <- c()
for (i in 1:nrow(df)){
  levelizedFactor[i]=f.levelizedVector(df$depletion_rate[i],discount_rate,ramp_up)
}
df$levelizedFactor = levelizedFactor

names(df)

df <- df %>% 
  mutate(reserve=Reserve_Li_ktons*1e3,
         resource=(Reserve_Li_ktons+Resource_Li_ktons+Resource_Inferred_Li_ktons)*1e3)

# levelized cost per ton
data_fig <- df %>% 
  dplyr::select(Deposit_Name,Resource_Type,USD_pertonne_Li,Investment_original,levelizedFactor,reserve,
                resource) %>% 
  pivot_longer(c(reserve,resource), names_to = "key", values_to = "size") %>% 
  filter(!is.na(size)) %>% 
  mutate(Operation=USD_pertonne_Li*levelizedFactor,
         Investment=Investment_original*1e6/size) %>% 
  pivot_longer(c(Operation,Investment), names_to = "type", values_to = "cost")
  

# label
data_fig <- data_fig %>% 
  group_by(Deposit_Name,key) %>% 
  mutate(share_inv=cost/sum(cost)) %>% ungroup() %>% 
  mutate(label_perc=if_else(type=="Investment",
                            paste0(round(share_inv*100,0),"%"),
                            ""))

data_fig <- data_fig %>% mutate(cost_LCE=cost/5.323)

ggplot(data_fig,aes(Deposit_Name,cost_LCE,fill=type))+
  geom_col(position = position_stack())+
  geom_text(aes(label=label_perc),
            position = position_stack(vjust = .5))+
  coord_flip()+
  facet_grid(Resource_Type~key,scales = "free_y",space = "free_y")+
  labs(x="",y="Levelized cost of extraction [USD/ton LCE]",fill="")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")


## Existing Mines with production capacity

# new depletion rate
df1 <- df %>% 
  dplyr::select(Deposit_Name,Resource_Type,USD_pertonne_Li,Investment_original,levelizedFactor,reserve,
                resource,Production_Li_ktons) %>% 
  pivot_longer(c(reserve,resource), names_to = "key", values_to = "size") %>% 
  filter(!is.na(size)) %>% 
  mutate(depletion_rate1=as.integer(size/Production_Li_ktons/1e3))
  
levelizedFactor <- c()
for (i in 1:nrow(df1)){
  levelizedFactor[i]=f.levelizedVector(df1$depletion_rate1[i],discount_rate,ramp_up)
}
df1$levelizedFactor = levelizedFactor


# levelized cost per ton
data_fig <- df1 %>% 
  mutate(Operation=USD_pertonne_Li*levelizedFactor,
         Investment=Investment_original*1e6/size) %>% 
  pivot_longer(c(Operation,Investment), names_to = "type", values_to = "cost")

# label
data_fig <- data_fig %>% 
  group_by(Deposit_Name,key) %>% 
  mutate(share_inv=cost/sum(cost)) %>% ungroup() %>% 
  mutate(label_perc=if_else(type=="Investment",
                            paste0(round(share_inv*100,0),"%"),
                            ""))

data_fig <- data_fig %>% mutate(cost_LCE=cost/5.323)

ggplot(data_fig,aes(Deposit_Name,cost_LCE,fill=type))+
  geom_col(position = position_stack())+
  geom_text(aes(label=label_perc),
            position = position_stack(vjust = .5))+
  coord_flip()+
  facet_grid(Resource_Type~key,scales = "free_y",space = "free_y")+
  labs(x="",y="Levelized cost of extraction [USD/ton LCE]",fill="")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")




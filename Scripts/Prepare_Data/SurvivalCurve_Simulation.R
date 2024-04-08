## Survival Curve based on Normal Distribution
## PBH January 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# parameters -----
mean_ev <- 17
sd_ev <- 4
mean_lib <- 15
sd_lib <- 4

# corr_ev_lib <- 0.3 # NOT USED FOR NOW

# normal dist
pnorm(8, mean = mean_lib, sd = sd_lib) # 4% of failure before 8 years
plogis(8,mean_lib,sd_lib*sqrt(3)/pi) # 4% as well with logistic dis


# Function to get flows (numbers of cars,EV,LIB) depending on the 
# vehicle and battery starting age
# Discretized by year using Normal Distribution
# n vehicles: vehicles currently on stock, 
f.getOutflows <- function(n_veh=1,EV_age,LIB_age, maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic"){
  
  # get probability of failure based on CDF of Normal
  # EV
  # y1 = pnorm(EV_age+1, mean = mean_ev, sd = sd_ev)-pnorm(EV_age, mean = mean_ev, sd = sd_ev) 
  # # LIB
  # y2 = pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib)-pnorm(LIB_age, mean = mean_lib, sd = sd_lib)
  
  # option 2: get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year
  
  if(dist.Age=="Normal"){
    y1 = (1-pnorm(EV_age+1, mean = mean_ev, sd = sd_ev))/
      (1-pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1-pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib))/
      (1-pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else{ # Logistic
    y1 = (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi))/ # CONVERT SCALE TO Stand Dev.
      (1-plogis(EV_age, mean_ev, sd_ev*sqrt(3)/pi))
    y2 = (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi))/
      (1-plogis(LIB_age, mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  
  # max age
  if(EV_age>=maxEV_age) {y1 = 0}
  if(LIB_age>=maxLIB_age) {y2 = 0}
  
  # # get probabilities as independent events
  # ret <- tibble(
  #   both_fail=y1*y2*n_veh,
  #   ev_fail=y1*(1-y2)*n_veh,
  #   lib_fail=(1-y1)*y2*n_veh,
  #   none=(1-y1)*(1-y2)*n_veh) # none fails
  
  # case 2 - independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail=(1-y1)*y2*n_veh,
    lib_fail=y1*(1-y2)*n_veh,
    none=y1*y2*n_veh)
  
  return(ret)
}

# Example
f.getOutflows(100,0,0)
f.getOutflows(100,16,12) # mean ages
f.getOutflows(100,26,14)
f.getOutflows(100,30,14) # all EV fail


# Loop to get outflows through time
aux=c()
start=100
for(i in 0:29){ 
  o=f.getOutflows(start,i,i)
  aux=rbind(aux,o)
  start=o$none
}

colSums(aux)[1:3]
sum(colSums(aux)[1:3]) # sums to 100
aux$none # goes to zero

aux$year <- 1:30

data_fig <- aux %>% 
  mutate(Surviving_EV=none,
         Outflow_EV=cumsum(both_fail+ev_fail),
         Outflow_LIB=cumsum(both_fail+lib_fail)) %>% 
  dplyr::select(year,Surviving_EV,Outflow_EV,Outflow_LIB) %>% 
  pivot_longer(c(-year), names_to = "key", values_to = "value") %>% 
  filter(year<23) %>% 
  mutate(key=str_replace(key,"_"," "))

ggplot(data_fig,aes(year,value,col=key,group=key))+
  geom_line(linewidth=2)+
  geom_text(aes(label=key),data=filter(data_fig,year==20),
            nudge_y = 8, nudge_x = 0.5,
            size=18*5/14 * 0.8)+
  theme(legend.position = "none")+
  labs(x="Year",y="Number of EV/LIB",col="",
       title="Brand new EV"
       # title="New EV with 4 year old battery"
       # title="8 year-old EV with new battery"
       # title="8 year-old EV with 7 year-old battery"
  )



# Cohort Outflows --------------

# parameters
# Other parameters
ev_age_newLib <- 8 # year were a new battery is needed, after that an old battery will be sufficient
# 8 years assuming a warranty over this period
max_reuse_lib <- 0.5
ssps_perc <- 0.7
recycling_perc <- 1-ssps_perc


# life time parameters
life_param <- tibble(Vehicle=c("Two/Three Wheelers","Car","Van","Bus",
                               "Medium truck","Heavy truck"),
                     mean_ev=c(12,17,18,16,17,17),
                     sd_ev=rep(4,6),
                     mean_lib=c(15,15,15,8,8,8),
                     sd_lib=rep(4,6))

# Data from ICCT
icct <- readxl::read_excel("Data/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year")


# Historical EV sales for stock
EV_historical <- read.csv("Results/Intermediate Results/historicalEV_sales.csv")

# just United States for now
# icct %>% head
# icct <- icct %>% filter(Vehicle=="Car",
#                         Country=="United States",
#                         # Country=="China",
#                         Scenario=="Baseline",Powertrain=="BEV") %>%
#   dplyr::select(Year,Sales)

# Whole world
icct <- icct %>% 
  # filter(Vehicle=="Car",Powertrain=="BEV") %>% 
  filter(Powertrain %in% c("BEV")) %>% # not worth doing PHEV, sales go to zero by 2050, and minor influence in demand
  group_by(Vehicle, Powertrain,Year,Scenario) %>% summarise(Sales=sum(Sales))

# add historical
EV_historical <- EV_historical %>% rename(Year=year) %>% 
  filter(Year<2022) %>% 
  # filter(ICCT_Country=="United States") %>% 
  group_by(Year) %>% reframe(Sales=sum(unit)) %>% 
  mutate(Vehicle="Car",Powertrain="BEV") #only stock data available


## Loop ------
scenarios <- icct$Scenario %>% unique()
vehicles <- icct$Vehicle %>% unique()
powers <- icct$Powertrain %>% unique()
# scenarios <- "Ambitious"
icct_orig <- icct
icct_new <- c()
# max_reuse_lib <- 0 # no LIB reuse case

for (veh in vehicles){
  
  # life params
  mean_ev <- life_param %>% filter(Vehicle==veh) %>% pull(mean_ev)
  sd_ev <- life_param %>% filter(Vehicle==veh) %>% pull(sd_ev)
  mean_lib <- life_param %>% filter(Vehicle==veh) %>% pull(mean_lib)
  sd_lib <- life_param %>% filter(Vehicle==veh) %>% pull(sd_lib)
  
  for (pow in powers){
    cat("",veh,"-",pow,"\n")
    
    if (veh=="Two/Three Wheelers" & pow=="PHEV"){break} # no much sales for this
    
    for (scen in scenarios){
      cat("Scenario ",scen,"\n")
      
      # Filters
      icct <- icct_orig %>% 
        filter(Scenario==scen) %>% 
        filter(Vehicle==veh) %>% 
        filter(Powertrain==pow)
      
      start_year <- 2022
      
      # add historical sales
      if(veh=="Car" & pow=="BEV"){
        EV_historical$Scenario <- scen
        icct <- rbind(EV_historical,icct)
        start_year <- 2015
      }
      
      ## Loop by years 
      # Matrix update idea
      # Key: Update matrix of vehicle age and battery age stock accordingly
      matrix_data <- matrix(0, nrow = 31, ncol = 31)
      rownames(matrix_data) <-paste0("EV_",0:30) # ROWS are EV
      colnames(matrix_data) <- paste0("LIB_",0:30) # COLS are Battery
      
      # Loop through years
      icct$Year %>% range()
      icct$add_LIB <-icct$LIB_SSPS <- icct$LIB_recycling <- icct$LIB_reuse_EV <- icct$EV_Stock <- 0
      icct$add_LIB_vector <-icct$LIB_SSPS_vector <- icct$LIB_recycling_vector <- c()
        
      for (y in start_year:2050){
        
        # if (y==2043){break} # debbug
      
        # Assign new sales to top left cuadrant (0,0)
        matrix_data[1, 1] <- icct$Sales[y-start_year+1]
        
        # clear stock of 10 or less batteries or EVs
        matrix_data[matrix_data < 10] <- 0
        
        # Get new matrix of EV stock with ages
        new_matrix <- matrix_ev <- matrix_lib <- matrix(0, nrow = 31, ncol = 31)
        rownames(new_matrix) <-paste0("EV_",0:30) # ROWS are EV
        colnames(new_matrix) <- paste0("LIB_",0:30) # COLS are Battery
        
        
        for (i in 1:31) { # EV
          for (j in 1:31) { # LIB
            if (matrix_data[i, j] != 0) {
              result <- f.getOutflows(matrix_data[i, j],i-1,j-1) # age is minus 1 for the index
              if (i!=31 & j!=31){ # to avoid border case
                new_matrix[i + 1, j + 1] <- result$none # move 1 age for both EV and LIB
                matrix_ev[i+1,j+1] <- result$lib_fail # EVs that need LIB
                matrix_lib[i+1,j+1] <- result$ev_fail # LIBs available to use
              } else if (j==31 & i!=31){ # BATTERIES TOO OLD
                matrix_ev[i+1,j] <- result$lib_fail # EVs that need LIB, no LIBs available as they died
              } else if (j!=31 & i==31){ # EV TOO OLD
                matrix_lib[i,j+1] <- result$ev_fail # LIBs available to use, no EV at border
              }
            }
          }
        }
        # get vector of outflows of EV and outflows of LIBs
        ev_need <- rowSums(matrix_ev)
        lib_available <- colSums(matrix_lib)
        
        # assigning old batteries TO EVs
        lib_to_EV <- lib_available*max_reuse_lib
        lib_available <- lib_available-lib_to_EV
        
        # first match year to year with offset of years - 8 years
        ev_need <- c(ev_need,rep(0,ev_age_newLib))
        lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
        allocation <- pmin(ev_need,lib_to_EV)
        
        ev_need <- ev_need - allocation
        lib_to_EV <- lib_to_EV - allocation
        
        # remove offsets
        ev_need <- ev_need[1:31]
        lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
        allocation <- allocation[-(1:ev_age_newLib)]
        
        # update new_matrix with stock of EVs and old batteries
        for (i in 1:(31-ev_age_newLib)){
          new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i]+allocation[i]
        }
        
        allocation <- sum(allocation)
        
        # do rest of allocation with LOOP
        start_bat <- 1
        for (i in 31:1) { # start with old
          if (i<=ev_age_newLib){
            # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATE IT
          } else {
            for (j in start_bat:31) {
              allocated <- min(ev_need[i], lib_to_EV[j])
              ev_need[i] <- ev_need[i] - allocated
              lib_to_EV[j] <- lib_to_EV[j] - allocated
              # update new_matrix with stock of EVs and old batteries
              new_matrix[i,j] <- new_matrix[i,j]+allocated
              allocation <- allocation+allocated
              start_bat <- j
              if (ev_need[i] == 0) { break }
            }
          }
        }
        
        # add remaining batteries back to pool
        lib_available <- lib_available+lib_to_EV
        
        # add EVs with new batteries to stock - note, no other battery with 0 age
        new_matrix[,1] <-  ev_need
        
        # assign numbers for Year - totals and vector
        icct$add_LIB[y-start_year+1] <- round(sum(ev_need),0) # additional new LIBs required
        icct$add_LIB_vector[y-start_year+1] <- list(round(ev_need[-1],0)) 
        icct$LIB_SSPS[y-start_year+1] <- round(sum(lib_available)*ssps_perc,0) # outflow for SSPS use
        icct$LIB_SSPS_vector[y-start_year+1] <- list(round(lib_available[-1]*ssps_perc,0)) # outflow for SSPS use
        icct$LIB_recycling[y-start_year+1] <- round(sum(lib_available)*recycling_perc,0)
        icct$LIB_recycling_vector[y-start_year+1] <- list(round(lib_available[-1]*recycling_perc,0))
        icct$LIB_reuse_EV[y-start_year+1] <- round(allocation,0)
        icct$EV_Stock[y-start_year+1] <- round(sum(new_matrix),0)
        
        # end for loop, next year
        matrix_data <- new_matrix
        
        # keep balance of removed EV Sales from stock
        
        rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
        
      }
      rm(i,j)
      # save data
      icct_new <- rbind(icct_new,icct)
    }
  }
}

icct <- icct_new


## some analysis stats ----

# battery needs cumulative
sum(icct$Sales)/1e6 # 155M veh. equivalent 
(sum(icct$Sales)+sum(icct$add_LIB))/1e6 # 197M
(sum(icct$Sales)+sum(icct$add_LIB))/sum(icct$Sales)-1 # 26% more
sum(icct$add_LIB)/1e6 # 41M ADDITIONAL
sum(icct$LIB_reuse_EV)/1e6 # 32M were used for reuse
sum(icct$LIB_SSPS)/1e6 # 6.5M
sum(icct$LIB_recycling)/1e6 # 1.6M

# 2050 YEAR
icct$Sales[29]/1e6 # 7.45M veh. equivalent 
(icct$Sales[29]+icct$add_LIB[29])/1e6 # 10.3M
(icct$Sales[29]+icct$add_LIB[29])/icct$Sales[29]-1 # 37.8% more
icct$add_LIB[29]/1e6 # 2.8M ADDITIONAL
icct$LIB_reuse_EV[29]/1e6 # 4M were used for reuse
icct$LIB_SSPS[29]/1e6 # 0.8M
icct$LIB_recycling[29]/1e6 # 0.2M

# figures
head(icct)
data_fig <- icct %>%
  filter(Year>2021) %>% 
  dplyr::select(-EV_Stock,-add_LIB_vector,-LIB_SSPS_vector,-LIB_recycling_vector) %>% 
  rename(`Additional LIB \n required`=add_LIB) %>% 
  rename(`LIBs for \n recycling`=LIB_recycling) %>% 
  rename(`2-hand LIBs \n for SSPS`=LIB_SSPS) %>% 
  rename(`2-hand LIBs \n used for EVs`=LIB_reuse_EV) %>% 
  pivot_longer(c(-Year,-Scenario,-Vehicle,-Powertrain), names_to = "key", values_to = "value") %>% 
  filter(Scenario=="Ambitious") %>% 
  mutate(value=value/1e6) 

data_fig2 <- data_fig %>% filter(Vehicle=="Car", Powertrain=="BEV")

data_fig2 %>%  
  ggplot(aes(Year,value,col=key,group=key))+
  geom_line(linewidth=1)+
  geom_text(data=filter(data_fig2,Year==2050),x=2052,aes(label=key),
            nudge_y = c(0,5,-5,2,18),
            lineheight = 0.8,
            size=11*5/14 * 0.8)+
  facet_wrap(~Scenario)+
  labs(x="",y="Units, in millions",col="")+
  theme(legend.position = "none")+
  coord_cartesian(xlim=c(2023.4,2053))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050), 
                     labels = c("2022", "2030", "2040", "2050"))

# f.fig.save("Figures/Reuse_Battery/World_outflows_0reuse.png")
f.fig.save("Figures/Reuse_Battery/World_outflows.png")

# All vehicles
data_fig2 <- data_fig %>% filter(Powertrain=="BEV")

data_fig2 %>%  
  ggplot(aes(Year,value,col=key,group=key))+
  geom_line(linewidth=1)+
  facet_wrap(~Vehicle,scales = "free_y")+
  labs(x="",y="Units, in millions",col="Flow",caption="Different scales per panel.")+
  coord_cartesian(xlim=c(2022,2050))+
  guides(col= guide_legend(reverse = TRUE))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050), 
                     labels = c("2022", "2030", "2040", "2050"))

f.fig.save("Figures/Reuse_Battery/World_outflows_vehs.png")

# stock
icct %>% 
  mutate(EV_Stock=EV_Stock/1e6) %>% 
  # filter(Vehicle=="Car") %>% 
  filter(Powertrain=="BEV") %>% 
  filter(Scenario=="Ambitious") %>%
  ggplot(aes(Year,EV_Stock))+
  geom_line(linewidth=1)+
  facet_wrap(~Vehicle)+
  labs(x="",y="",title="EV stock [millions]",col="")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050), 
                     labels = c("2022", "2030", "2040", "2050"))
f.fig.save("Figures/Reuse_Battery/World_EVStock.png",w=8.7)


## save stats as World -----
# all as percentage of that year sales
icct <- icct %>% 
  filter(Year>2021) %>% 
  mutate(perc_add_lib=add_LIB/Sales,
         perc_lib_reuse_ev=LIB_reuse_EV/Sales,
         perc_lib_ssps=LIB_SSPS/Sales,
         perc_lib_recycling=LIB_recycling/Sales)
icct

# Save vector variables as strings
icct <- icct %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) 

write.csv(icct,"Results/Intermediate Results/world_outflows_LIB.csv",row.names = F)


# Survival Curve - Normal CDF --------
# Generate data
data <- data.frame(x = seq(0, 40, length.out = 1000), 
                   y = 1-pnorm(seq(0, 40, length.out = 1000), mean = mean_ev, sd = sd_ev))

# Create CDF plot
ggplot(data, aes(x, y)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format())+
  # coord_cartesian(expand = F)+
  labs(title = "",
       x = "Years",
       y = "% Surviving")


## Using Bivariate get Outflows ----

# ALMOST IDENTIAL AS WITH NO CORRELATION
# Discretized by year using Bivariate Normal Distribution
library(mvtnorm)
f.getOutflows_correlated <- function(n_veh = 1, EV_age, LIB_age, correlation = 0) {
  
  # Create a covariance matrix
  cov_matrix <- matrix(c(sd_ev^2, correlation * sd_ev * sd_lib, correlation * sd_ev * sd_lib, sd_lib^2), ncol = 2)
  
  # Calculate bivariate normal probabilities
  # prob_both_fail <- pmvnorm(lower = c(-Inf, -Inf), upper = c(EV_age, LIB_age), 
  #                           mean = c(mean_ev, mean_lib), sigma = cov_matrix) -
  #   pmvnorm(lower = c(-Inf, -Inf), upper = c(EV_age - 1, LIB_age - 1), 
  #           mean = c(mean_ev, mean_lib), sigma = cov_matrix)
  # 
  # 
  prob_both_fail <- pmvnorm(lower = c(EV_age-1, LIB_age-1), upper = c(EV_age, LIB_age), 
                            mean = c(mean_ev, mean_lib), sigma = cov_matrix)
  
  
  prob_ev_fail <- pmvnorm(lower = c(EV_age-1, -Inf), upper = c(EV_age, LIB_age-1), mean = c(mean_ev, mean_lib), sigma = cov_matrix)+
    pmvnorm(lower = c(EV_age-1, LIB_age), upper = c(EV_age, Inf), mean = c(mean_ev, mean_lib), sigma = cov_matrix)

  prob_lib_fail <- pmvnorm(lower = c(-Inf, LIB_age-1), upper = c(EV_age-1, LIB_age),                            mean = c(mean_ev, mean_lib), sigma = cov_matrix)+
    pmvnorm(lower = c(EV_age, LIB_age-1), upper = c(Inf, LIB_age), mean = c(mean_ev, mean_lib), sigma = cov_matrix)
  
  # 4 quadrants
  prob_none <- pmvnorm(lower = c(-Inf,-Inf), upper = c(EV_age-1, LIB_age-1), mean = c(mean_ev, mean_lib), sigma = cov_matrix)+
    pmvnorm(lower = c(EV_age, LIB_age), upper = c(Inf,Inf), mean = c(mean_ev, mean_lib), sigma = cov_matrix)+
    pmvnorm(lower = c(-Inf, LIB_age), upper = c(EV_age-1,Inf), mean = c(mean_ev, mean_lib), sigma = cov_matrix)+
    pmvnorm(lower = c(EV_age, -Inf), upper = c(Inf,LIB_age-1), mean = c(mean_ev, mean_lib), sigma = cov_matrix)
  
  
  prob_both_fail+prob_ev_fail+prob_lib_fail+prob_none
  
  # Calculate flows
  ret <- tibble(
    both_fail = prob_both_fail * n_veh,
    ev_fail = prob_ev_fail * n_veh,
    lib_fail = prob_lib_fail * n_veh,
    none = prob_none * n_veh
  )
  
  return(ret)
}

f.getOutflows(100,16,12)
f.getOutflows_correlated(100,16,12,correlation = 0)
f.getOutflows_correlated(100,16,12,correlation = 0.3)
f.getOutflows_correlated(100,16,12,correlation = 0.8)
f.getOutflows_correlated(100,16,12,correlation = 1)


# Loop to get outflows through time
aux=c()
start=100
for(i in 1:20){ 
  o=f.getOutflows_correlated(start,i+4,i,correlation = 0)
  aux=rbind(aux,o)
  start=o$none
}
aux
colSums(aux)[1:3]
sum(colSums(aux)[1:3])+aux[20,4] # sums to 100

aux$year <- 1:20

data_fig <- aux %>% 
  mutate(Surviving_EV=none,
         Outflow_EV=cumsum(both_fail+ev_fail),
         Outflow_LIB=cumsum(both_fail+lib_fail)) %>% 
  dplyr::select(year,Surviving_EV,Outflow_EV,Outflow_LIB) %>% 
  pivot_longer(c(-year), names_to = "key", values_to = "value") %>% 
  mutate(key=str_replace(key,"_"," "))

ggplot(data_fig,aes(year,value,col=key,group=key))+
  geom_line(linewidth=2)+
  geom_text(aes(label=key),data=filter(data_fig,year==19),nudge_y = 5,
            size=18*5/14 * 0.8)+
  theme(legend.position = "none")+
  labs(x="Year",y="Number of EV/LIB",col="",
       title="Brand new EV"
       # title="New EV with 7 year old battery"
       # title="8 year-old EV with new battery"
       # title="8 year-old EV with 7 year-old battery"
  )





## OLD 


icct %>% 
  mutate(age=0)
  # complete(age = 0:30) %>%
  # fill(Year, Sales, .direction = "down")

library(purrr)

icct %>% 
  # mutate(age=0) %>% 
  group_by(Year) %>%
  mutate(Sales = f.getOutflows(Sales[-1],EV_age = 0, LIB_age = 0))


icct %>%
# create stock at age 0
  mutate(Stock = Sales) %>% 
  mutate(age=0)
  

f.getOutflows(100,0,0)

icct %>%
  # filter(Year==2022) %>%
  group_by(Year) %>% 
  mutate(EV_Stock = first(Sales)) %>% ungroup() %>% 
  crossing(age = 0:30) %>% 
  group_by(Year) %>% 
  mutate(EV_Stock = Reduce(function(x, y) f.getOutflows(x,y,y),
                        age[-1], init = first(EV_Stock), accumulate = TRUE)) %>% ungroup()

# moved by 1 year, but works
f.getOutflows(767757,1,1)
f.getOutflows(763332.9,2,2)

### NEXT TO DO:
# GET LIB OUTFLOW, GET EV NEEDING LIB

attr(a,"rows")
a.rows

x=icct %>% filter(Year==2022)
x$age=0

# for approach
for (a in 1:30){
  
  y=tail(x,1)
  y$age=a
  y$Sales=y$Sales*0.9
  
  x=rbind(x,y)
}
x



icct %>% 
  group_by() %>% 
  mutate(Sales=lag(Sales)*0.9)




icct %>%
  crossing(age = 0:30)

icct %>%
  crossing(age = 0:30) %>% as.tibble() %>%
  group_by(Year) %>%
  mutate(Sales=lag(Sales)*0.9)


# dummy check ------ 
start=767757
start=100
cumProb=c()
stock=c()
removal=0
removal_cum=0

# fraction year to year approach

# first calculate it from a complete CDF of Normal distribution - using the cumulative
cumProb=c()
fraction_y2y <- c()
for (i in 0:30){
  cumProb[i+1] <- pnorm(i+1, mean = mean_ev, sd = sd_ev)-pnorm(i, mean = mean_ev, sd = sd_ev)
  fraction_y2y[i+1] <- (1-pnorm(i+1, mean = mean_ev, sd = sd_ev))/
    (1-pnorm(i, mean = mean_ev, sd = sd_ev))
}
sum(cumProb)
fraction_y2y

#use fraction for stock
start=100
stock=c()
for (i in 0:30){
  stock[i+1]=start
  start=start*fraction_y2y[i+1]
}
stock # WORKS, but now how to calculate it from a joint????
100-cumsum(cumProb*100)



# data bivariate for understanding
none <- matrix(0, nrow = 31, ncol = 31)
rownames(none) <-paste0("EV_",0:30) # ROWS are EV
colnames(none) <- paste0("LIB_",0:30) # COLS are Battery
both_fail <- ev_fail <- lib_fail <- none

for (i in 0:31) { # EV
  for (j in 0:31){
    y1 = pnorm(i+1, mean = mean_ev, sd = sd_ev)-pnorm(i, mean = mean_ev, sd = sd_ev) 
    y2 = pnorm(j+1, mean = mean_lib, sd = sd_lib)-pnorm(j, mean = mean_lib, sd = sd_lib)
    o <- f.getOutflows(1,i,j)
    
    none[i,j] <- o$none
    both_fail[i,j] <- o$both_fail
    ev_fail[i,j] <- o$ev_fail
    lib_fail[i,j] <- o$lib_fail
  }}

df_export <- rbind(as.data.frame(as.table(none)) %>% mutate(var="none"),
                   as.data.frame(as.table(both_fail)) %>% mutate(var="both_fail"),
                   as.data.frame(as.table(ev_fail)) %>% mutate(var="ev_fail"),
                   as.data.frame(as.table(lib_fail)) %>% mutate(var="lib_fail"))
df_export



# Survival Curve - 2 independent Normal CDF ------------------
library(ggnewscale)
data <- expand.grid(x1 = seq(0, 30),
                    x2 = seq(0, 30))

data <- data %>% mutate(y1 = pnorm(x1, mean = mean_ev, sd = sd_ev),
                        y2 = pnorm(x2, mean = mean_lib, sd = sd_lib))


data <- data %>% mutate(
  both_fail=y1*y2,
  ev_only=y1*(1-y2),
  lib_only=(1-y1)*y2,
  none=(1-y1)*(1-y2),
  check=both_fail+ev_fail+lib_fail+none)
data$max_column = colnames(data)[max.col(data[, c(5:8)], ties.method = "first")+4]

head(data)

alpha_value=1

ggplot(data,aes(x = x1, y = x2)) +
  geom_tile(data=filter(data,max_column=="none"),
            aes(fill=none),alpha=alpha_value) +
  scale_fill_gradient(low = "white", high = "red",labels = scales::percent_format()) +
  new_scale_fill() +
  geom_tile(data=filter(data,max_column=="both_fail"),
            aes(fill=both_fail),alpha=alpha_value) +
  scale_fill_gradient(low = "white", high = "blue",labels = scales::percent_format()) +
  new_scale_fill() +
  geom_tile(data=filter(data,max_column=="ev_fail"),
            aes(fill=ev_fail),alpha=alpha_value) +
  scale_fill_gradient(low = "white", high = "yellow",labels = scales::percent_format()) +
  new_scale_fill() +
  geom_tile(data=filter(data,max_column=="lib_fail"),aes(fill=lib_fail),alpha=alpha_value) +
  scale_fill_gradient(low = "white", high = "green",labels = scales::percent_format()) +
  # facet_wrap(~key)+
  labs(x = "EV Years",
       y = "LIB Years",
       fill="")+
  coord_cartesian(expand = F)



# Survival Curve - Bivariate Normal CDF --------
library(mvtnorm)


# Generate bivariate normal data
data <- expand.grid(x = seq(0, 30, length.out = 100),
                    y = seq(0, 30, length.out = 100))
corr_ev_lib=1
corr_ev_lib=0.3
corr_ev_lib=0

library(VGAM)
data$z <- 1-pbinorm(data$x, data$y, mean1 = mean_ev, mean2 = mean_lib, 
                    var1 = sd_ev^2, var2 = sd_lib^2, 
                    cov12 = sd_ev*sd_lib*corr_ev_lib)

# Create heatmap
ggplot(data, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "white",labels = scales::percent_format()) +
  labs(x = "EV Years",
       y = "LIB Years",
       fill="% Surviving")+
  coord_cartesian(expand = F)

# EoF

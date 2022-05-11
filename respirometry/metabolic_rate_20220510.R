# Title: metabolic rate in diploid and triploid pacific oysters with temperature/desiccation
# Author: Matthew George; mattgeorgephd@gmail.com
# Date: 07/21/2021

## clear
rm(list = ls())

## Grab the WD from the file location
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path ))
getwd()

## Load R packages
library(readxl)
library("xlsx")
library(ggplot2)
library(stringr)
library(tidyverse)
# library(tidyverse)
# library(Johnson)
# library(agricolae)
# library(nlme)
# library(multcomp)
# library(nlme)
# library(multcomp)
# library(RColorBrewer)
# library(scales)


## Set ggplot theme
my_theme <- theme(line              = element_line(size = 1.5),
                  rect              = element_rect(size = 1.5),
                  text              = element_text(size = 14,color ="black"),
                  panel.background  = element_blank(),
                  panel.grid.major  = element_blank(), 
                  panel.grid.minor  = element_blank(),
                  axis.text.x       = element_text(size = 16,color = "black"),
                  axis.text.y       = element_text(size = 16,color = "black"),
                  axis.title.x      = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                  axis.title.y      = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                  axis.ticks.x      = element_line(color = "black"),
                  axis.ticks.y      = element_line(color = "black"),
                  # axis.line         = element_line(color = "black", size = 0.1),
                  panel.border      = element_rect(color = "black", fill = NA, size = 1.5),
                  legend.key        = element_blank()) # removes background of legend bullets



#######################################################################################################################################
# Navigate to data folder and import file list

getwd()
setwd("data/20220510")


# grab list of file names from folder
f <- Sys.glob("*.xlsx")
tail(f) # check for ~$ files at end. 
#f <- f[-66] # If you see one, remove

# Loop through and grab data using lapply
dat = lapply(f, function(i)
{
  x = read_excel(i, sheet = "SABD0002000012, Ch 1", col_names = TRUE)
  # Get the columns you want
  x = x[, c(3, 7, 9)] 
  if(length(x[,3])>15) x=x[c(-1:-10),] # remove the first 10 to get rid of noise
  colnames(x) <- c("time", "oxy", "temp")
  # print(i)
  # Break up the name and add columns for oyster name and treatment setpoint
  output1 <- strsplit(i,'\\.') # split name by period
  output1 <- unlist(output1,use.names=FALSE) # get rid of the annoying list format
  output2 <- output1[1] # remove file extension
  output3 <- strsplit(output2[1],'\\_') # split file name by underscore
  output3 <- unlist(output3,use.names=FALSE) # get rid of the annoying list format
  date_collected <- output3[1] # get date collected
  ploidy  <- output3[2] # get ploidy (D or T)
  oyster <- output3[4] # get oyster name
  tempset <- output3[3] # get temp set point
  x$date = date_collected
  x$ploidy = ploidy
  x$oyster = oyster
  x$tempset = as.numeric(tempset)
  x #need to call x at the end
})

library(dplyr)

# consolidate dataset into a data frame
dat = do.call("rbind.data.frame", dat)
dat <- unite(data=dat,"ID",c(ploidy,oyster),sep="_",remove=FALSE)
dat <- unite(data=dat,"ID_sort",c(ID,tempset),sep="_",remove=FALSE)

# nest data by run
dat_nested <- dat %>% 
  group_by(ID_sort) %>%
  nest() %>% 
  ungroup()

# loop through the data and scale time by start (set first time point to zero)
steps <- seq(from=1,to=nrow(dat_nested),by=1)
for (i in steps) {
  
  pulled_data   <- dat_nested[[2]][[i]]
  steps_pulled  <- seq(from=1,to=nrow(pulled_data),by=1)
  start         <- pulled_data[1,1]
  adjusted_time <- data.frame(time = integer(max(steps_pulled)))
  
  for (j in steps_pulled) {
    a <- pulled_data[j,1]
    b <- a - start
    adjusted_time[j,1] = b
  }
  
  pulled_data[,1] = adjusted_time
  dat_nested[[2]][[i]] <- pulled_data
  
  if(i == min(steps)) print("Wait")
  if(i == max(steps)/2) print("Wait")
  if(i == max(steps)) print("Finished")
  
}

# Export list of file names before review
getwd(); setwd('..'); setwd('..') setwd('constraints/20220510'); getwd()
write.xlsx(x=dat_nested$ID_sort, file="constraints.xlsx", sheetName = "sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


# Plot the lines one by one to find ones to trash. Add start and stop to for recalcs in constraints tab of MR_output.

getwd(); setwd('..'); setwd('..'); setwd('plots/20220510'); getwd()


steps <- seq(from=1,to=nrow(dat_nested),by=1)
for (i in steps){
  pulled_data   <- dat_nested[[2]][[i]]
  time <- pulled_data$time
  oxy <- pulled_data$oxy
  ID <- pulled_data$ID
  tempset <- pulled_data$tempset
  lm_eqn <- function(time,oxy){
    m <- lm(oxy ~ time,pulled_data);
    eq <- substitute(italic(oxy) == a + b %.% italic(time)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  p <- ggplot(data=pulled_data, aes(time,oxy)) +
    geom_smooth(aes(time,oxy),method="lm",color="red",se=FALSE) +
    geom_point() + 
    ggtitle(dat_nested$ID_sort[i]) +
    geom_text(x = max(time)-8, y = max(oxy)-0.2, label = lm_eqn(df), parse = TRUE) +
    my_theme
  ggsave(filename = paste(ID,"_",tempset,".png",sep=""),
         plot   = p,
         dpi    = 300,
         device = "png",
         width  = 5,
         height = 5,
         units  = "in")
  
}

# dat %>%
#   group_by(oyster) %>%
#   filter(ploidy == "T" & temp_set == 35) %>%
#   ggplot(aes(time,oxy)) + geom_point() + my_theme

#####################################################################################################################
## Calculate metabolic rate

## Input constraints
setwd('..'); getwd(); setwd('..'); getwd(); setwd('constraints/20220510'); getwd()

constraints <- read_excel("constraints.xlsx", sheet = "sheet1", col_names = TRUE)

# calculate slope
steps <- seq(from=1,to=nrow(dat_nested),by=1)
oxy_hr <- data.frame(ID_sort = character(nrow(dat_nested)),umol_L = integer(nrow(dat_nested)),stringsAsFactors=FALSE)

for (i in steps) {
  
  a <- dat_nested[[2]][[i]]
  
  if(constraints[i,3] == 1){
    start_calc = round((constraints[i,4]/0.33),0)
    stop_calc  = round((constraints[i,5]/0.33),0) 
    
    x <- a$time[start_calc[1,1]:stop_calc[1,1]]
    y <- a$oxy[start_calc[1,1]:stop_calc[1,1]]
    
  } 
  else {
    start_calc = 0
    stop_calc = nrow(a)-1
    
    x <- a$time[start_calc:stop_calc]
    y <- a$oxy[start_calc:stop_calc]    
    
  }
  
  fit <- lm(y ~ x)
  fit
  
  a <- as.numeric(fit$coefficients[2])*100+as.numeric(fit$coefficients[1])
  b <- as.numeric(fit$coefficients[2])*160+as.numeric(fit$coefficients[1])
  
  oxy_hr[i,2] = a-b
  oxy_hr[i,1] = as.character(dat_nested[[1]][[i]])
  
} ## looks into the constraints tab of MR_output and calculates slope, umol/L oxy consumption per hour, outputs oxy_hr 

# output calculated umol/L/hr
getwd(); setwd('..'); setwd('..'); setwd('output')
write.xlsx(x=oxy_hr,file="20220510.xlsx", sheetName = "sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

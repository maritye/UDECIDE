### Plots for UDECIDE poster at AGU
## Needed to be run in previous version of R (R/3.3.2) to get over dplyr and rlang conflict.

# First, comparison of Min daily temperatures in DPLE and  LENS

pax <-  c("gghighlight", "ggrepel", "ggplot2", "lubridate", "bindrcpp", 
  "tibble",  "purrr", "tidyr", "dplyr", "stats",  
  "graphics",  "grDevices", "utils", "datasets",  "methods",  
  "base") 
lapply(pax, function(x) {library(x, character.only = TRUE)})

# reads in temperature for one huc from one ensemble member for CESM_LE. tidy up
#setwd("~/UDECIDE/data")

load("CESM__DP_LE_tn_test.RData")
load("CESM_LE_HUCS_TMN_abs.RData")

#### Compare mean daily minimum values.
projtemp_df <- as.tibble(bind_cols(data.frame(dates=le_dates), data.frame(quants=hucquants[,,1])))
names(projtemp_df) <- c("dates","mean", "min","q10", "q25", "q50", "q75", "q90", "max")
temp_proj_df <- projtemp_df %>% 
  select(dates, mean) %>%
  mutate(year = year(dates),
         mon.day = format(dates, "%m/%d"),
         temp_inC = mean - 273.15) 

predtemp_df <- as.tibble(bind_cols(data.frame(dates=dple_dates), data.frame(quants=dp.tnquants)))
names(predtemp_df) <- c("dates","mean", "min","q10", "q25", "q50", "q75", "q90", "max")
temp_pred_df <- predtemp_df %>% 
  select(dates, mean) %>%
  mutate(year = year(dates),
         mon.day = format(dates, "%m/%d"),
         temp_inC = mean - 273.15) 


# create 8 colour diverging palette from colorbrewer. Colour blind safe. This goes red to blue, so reverse it.
ColBlindTemp <- rev(c('#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4'))
# edit these two once you get data!
labels <- c("-3", "-1", "1", "3", "5", "7", "9", "11", "13", "15", "17")
breaks <- c(seq(-3, 17, by = 2))

proj_p <- temp_proj_df %>% 
  ggplot(aes(x = mon.day, y = year, fill = temp_inC)) +
  geom_tile() +
  #creates tidy colour bar with numbers below bins
  scale_fill_gradientn(
    colours = ColBlindTemp,
    labels = labels,
    breaks = breaks,
    #check the limits!
    limits = c(-2.6, 16.3),
    name=expression("Temp" 
                    ~degree~C)) +
  # Tidies up x and y axes
  scale_y_reverse(limits = c(2080, 2005), expand = c(0, 0),
                  breaks = c(2005, seq(2010, 2080, by = 10))) +
  scale_x_discrete(breaks = c("01/01", "02/01", "03/01", "04/01", "05/01", "06/01", "07/01", "08/01", "09/01", "10/01", "11/01", "12/01"),
                   labels = month.abb) +
  # adds main title at top left 
  labs(title = "CESM LENS Ensemble Mean for HUC1",
       subtitle = glue::glue("
                             One Row = One Year
                             Daily Minimum Temperature (Celsius)
                             ")) +
  theme_minimal() +
  geom_vline(xintercept=c(60, 152, 244, 335)) +
  # moves legend to bottom.    
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right"#),
        #plot.margin=unit(c(1,1,1.5,1.2),"cm")
        )


  

pred_p <- temp_pred_df %>% 
  # leave out Nov/Dec 2015 and 1 Jan 2026
  filter(year !=2015 & year !=2026) %>%
  ggplot(aes(x = mon.day, y = year, fill = temp_inC)) +
  geom_tile() +
  #creates tidy colour bar with numbers below bins
  scale_fill_gradientn(
    colours = ColBlindTemp,
    labels = labels,
    breaks = breaks,
    #check the limits!
    limits = c(-2.6, 16.3),
    name=expression("Temp"
                    ~degree~C)) +
  # Tidies up x and y axes
  scale_y_reverse(limits = c(2080, 2005), expand = c(0, 0),
                  breaks = c(2005, seq(2010, 2080, by = 10))) +
  scale_x_discrete(breaks = c("01/01", "02/01", "03/01", "04/01", "05/01", "06/01", "07/01", "08/01", "09/01", "10/01", "11/01", "12/01"),
                   labels = month.abb) +
  # adds main title at top left  
  labs(title = "CESM DPLE Ensemble Mean for HUC1") +
  theme_minimal() +
  geom_vline(xintercept=c(60, 152, 244, 335)) +
  # moves legend to bottom.    
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))
#need to define this function!!! see bottom of page
pdf("../CompareMinT.pdf", width=12, height=6)
print(multiplot(proj_p, pred_p, cols=2))
dev.off()

#### Reproduce climate stripes
by_year_lens <- group_by(temp_proj_df, year)
mean_an_tn_lens <- summarise(by_year_lens, meantn = mean(temp_inC, na.rm = T))

# 10 Class Divergent Rd-Bu: http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=10
temp_cols <- rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7',
                   '#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))  
brx <- seq(2,10,2)
labs <- c("2", "4", "6", "8", "10")
#plot the large ensemble mean
pdf("../CESMLENS_minT_an.pdf")
mean_an_tn_lens %>%
  ggplot(aes(x = as.factor(year), fill = meantn)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(expand = c(0, 0.01)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(colors = temp_cols, "Average Daily Minimum Temperature (Celsius)",
                       breaks = brx,
                       limits = c(2, 10),
                       labels = labs) +
  labs(title = "CESM LENS Ensemble Mean for HUC1 (2005-2080)",
       subtitle = "One Stripe = One Year, Left: 2005, Right: 2080") +
  geom_vline(aes(xintercept=11), linetype="dashed") +
  geom_vline(aes(xintercept=20), linetype="dashed") +
  theme_void() +
  theme(legend.position = "bottom")
dev.off()

# Now create the same for the prediction ensemble mean and quartiles
by_year_dp <- predtemp_df %>%
               mutate(year = year(dates),
                      mean = mean - 273.15,
                      q25 = q25 - 273.15,
                      q75 = q75 - 273.15) %>%
  group_by(year) %>%
  filter(year !=2015 & year !=2026)
mean_an_tn_dps <- summarise(by_year_dp, 
                            meantn = mean(mean, na.rm = T),
                            lowqtn = mean(q25, na.rm = T),
                            hiqtn = mean(q75, na.rm = T))

meanpred <- mean_an_tn_dps %>%
  ggplot(aes(x = as.factor(year), fill = meantn)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(expand = c(0, 0.01)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(colors = temp_cols, "Average Daily Minimum Temperature (Celsius)",
                       breaks = brx,
                       limits = c(2, 10),
                       labels = labs) +
  labs(title = "CESM DP Ensemble Mean for HUC1 (2016-2025)",
       subtitle = " ") +
  theme_void() +
  theme(legend.position = "bottom")

pred25 <- mean_an_tn_dps %>%
  ggplot(aes(x = as.factor(year), fill = lowqtn)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(expand = c(0, 0.01)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(colors = temp_cols, "",
                       breaks = brx,
                       limits = c(2, 10),
                       labels = labs) +
  labs(title = "CESM DP Ensemble Lower Quartile",
       subtitle = " ") +
  theme_void() +
  theme(legend.position = "none")

pred75 <- mean_an_tn_dps %>%
  ggplot(aes(x = as.factor(year), fill = hiqtn)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(expand = c(0, 0.01)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_gradientn(colors = temp_cols, "",
                       breaks = brx,
                       limits = c(2, 10),
                       labels = labs) +
  labs(title = "CESM DP Ensemble Upper Quartile",
       subtitle = " ") +
  theme_void() +
  theme(legend.position = "none")


pdf("../CESMDP_minT_anv.pdf", width=12, height=6)
multiplot(pred25, meanpred, pred75, cols = 3)
dev.off()



########## seasonal daily minimum temperature comparison
summerbyY <- by_year_lens %>%
             filter(month(dates)>=6 & month(dates)<=8) %>%
             group_by(year)
summertn_proj <- summarise(summerbyY, mean = mean(temp_inC, na.rm = T))

#all projection data, HUC1
summer <- which(month(le_dates)>=6 & month(le_dates)<=8)
hucarray_sum <- as.tibble(hucarrays[summer,,1])
huc1_sum_tn <- hucarray_sum %>%
  mutate(year = year(le_dates[summer]))

huc1_tnsum <-  apply(huc1_sum_tn, 2, function(x) running(x, width=92, by=92))
huc1_tnsum[,1:40] <- huc1_tnsum[,1:40] -273.15

# prediction data
summerbyY10 <- by_year_dp %>%
  filter(month(dates)>=6 & month(dates)<=8) %>%
  group_by(year)
summertn_pred <- summarise(summerbyY10, mean = mean(mean, na.rm = T))

#all projection data, HUC1
summer10 <- which(month(dple_dates)>=6 & month(dple_dates)<=8)
hucarray_sum10 <- as.tibble(dparrays.tn[summer10,])
huc1_sum_tn10 <- hucarray_sum %>%
  mutate(year = year(dple_dates[summer]))

huc1_tnsum10 <-  apply(huc1_sum_tn10, 2, function(x) running(x, width=92, by=92))
huc1_tnsum10[,1:40] <- huc1_tnsum10[,1:40] -273.15

pdf("../MeansummerTn.pdf")
plot(summertn_proj$year, summertn_proj$mean,  ty="l", col="lightgrey", main="Mean Summer Daily Minimum Temperature", xlab="Years", ylab=expression("Temperature " ~degree~C), ylim=c(5,15))
apply(huc1_tnsum[,1:40], 2, function(x) {lines(huc1_tnsum[,41], x,col="lightgrey")})
apply(huc1_tnsum10[,1:40], 2, function(x) {lines(huc1_tnsum10[,41], x,col="lightblue")})
lines(summertn_proj$year, summertn_proj$mean)
lines(summertn_pred$year,summertn_pred$mean, col="blue")
dev.off()




##### Function
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
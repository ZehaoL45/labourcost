################################################################################################
#
# CODE NAME:       	MC_10h
# 
# AUTHOR(S):		Zehao Li, University College London (zehao.li@ucl.ac.uk) 
#                       
# PURPOSE:		Monte-Carlo simulation of labour and logistical supply in person-days. 
#             The working hour is set to 5.
#
# ACKNOWLEDGEMENTS:	Giacomo Fontana, Andrew Bevan
# COPYRIGHT:		(C) 2024 by Zehao Li
# LICENCE：		MIT LICENCE
#			
################################################################################################

# Generate 1000 random values for various energetics rates
quarrywall_list <- runif(1000, min = 0.057, max = 0.114)
leveling_list <- runif(1000,min = 0.3, max = 0.5)
in_buildstoneupto0.02_list <- runif(1000, min=in_buildstoneupto0.02*0.9, max=in_buildstoneupto0.02*1.1)
in_buildstone0.02to0.05_list <- runif(1000, min=in_buildstone0.02to0.05 *0.9, max=in_buildstone0.02to0.05 *1.1)
in_buildstoneover0.05_list <- runif(1000, min=in_buildstoneover0.05*0.9, max=in_buildstoneover0.05*1.1)
earthrate_list <- runif(1000, min=0.42, max=0.64)


# Define a function to calculate various construction metrics based on random energetics values
calc_indepentrates <- function(quarrywall,leveling,upto0.02, from0.02to0.05, over0.05, earth) {
  # Assigning input parameters to local variables for energetics indexes
  in_quarrywall <- quarrywall
  in_quarryrubble <- 0.5
  in_leveling <- leveling
  in_buildrubbleearth <- 0.375
  in_quarrylost <- 0.15 # Accounting for a 15% loss in material during quarrying
  # Calculating energetics for different construction processes using the input parameters and predefined rates
  energetics <- matrix(1:4, nrow = 4, ncol = 1)
  energetics <- rbind(in_quarrywall,
                      in_quarryrubble,
                      in_leveling,
                      in_buildrubbleearth)
  rownames(energetics) <- c("Stone quarry (m^3/ph)",
                            "Rubble quarry (m^3/ph)",
                            "Leveling (m^3/ph)",
                            "Assembly rubble and earth fill (m^3/ph)")
  colnames(energetics) <- ("Rates")
  kable(energetics, digits = 4) %>%
    kable_styling(bootstrap_options = c("condensed"),
                  full_width = T,
                  position = "left")
  
  
  in_buildstoneupto0.02 <- upto0.02
  in_buildstone0.02to0.05 <- from0.02to0.05
  in_buildstoneover0.05 <- over0.05
  
  wallheight <- 3
  wallwidth <- 2.9
  
  ### Type-A
  tA_materialstone <- (tA_volstones+tA_volstones*in_quarrylost)*wallheight/in_quarrywall
  tA_materialrubble <- tA_volrubble*wallheight/in_quarryrubble
  tA_materialtotal <- tA_materialstone+tA_materialrubble
  ### Type-B
  tB_materialstone <- (tB_volstones+tB_volstones*in_quarrylost)*wallheight/in_quarrywall
  tB_materialrubble <- tB_volrubble*wallheight/in_quarryrubble
  tB_materialtotal <- tB_materialstone+tB_materialrubble
  ### Type-C
  tC_materialstone <- (tC_volstones+tC_volstones*in_quarrylost)*wallheight/in_quarrywall
  tC_materialrubble <- tC_volrubble*wallheight/in_quarryrubble
  tC_materialtotal <- tC_materialstone+tC_materialrubble
  ## Leveling the terrain
  ### Type-A
  tA_leveling <- wallwidth/in_leveling
  ### Type-B
  tB_leveling <- wallwidth/in_leveling
  ### Type-C
  tC_leveling <- wallwidth/in_leveling
  ## Wall assembly
  ### Type-A
  tA_assemblyfacade <- (((tA_facadevol*(composition["upto0.02","tA"]/100))/in_buildstoneupto0.02)+
                          ((tA_facadevol*(composition["0.02to0.05","tA"]/100))/in_buildstone0.02to0.05)+
                          ((tA_facadevol*(composition["over0.05","tA"]/100))/in_buildstoneover0.05))*wallheight*2
  tA_assemblyfill <- tA_volrubble*wallheight/in_buildrubbleearth
  tA_assemplytotal <- tA_assemblyfacade+tA_assemblyfill
  ### Type-B
  tB_assemblyfacade <- (((tB_facadevol*(composition["upto0.02","tB"]/100))/in_buildstoneupto0.02)+
                          ((tB_facadevol*(composition["0.02to0.05","tB"]/100))/in_buildstone0.02to0.05)+
                          ((tB_facadevol*(composition["over0.05","tB"]/100))/in_buildstoneover0.05))*wallheight*2
  tB_assemblyfill <- tB_volrubble*wallheight/in_buildrubbleearth
  tB_assemplytotal <- tB_assemblyfacade+tB_assemblyfill
  ### Type-C
  tC_assemblyfacade <- (((tC_facadevol*(composition["upto0.02","tC"]/100))/in_buildstoneupto0.02)+
                          ((tC_facadevol*(composition["0.02to0.05","tC"]/100))/in_buildstone0.02to0.05)+
                          ((tC_facadevol*(composition["over0.05","tC"]/100))/in_buildstoneover0.05))*wallheight*2
  tC_assemblyfill <- tC_volrubble*wallheight/in_buildrubbleearth
  tC_assemplytotal <- tC_assemblyfacade+tC_assemblyfill
  
  ## Total
  ### Type-A
  tA_total <- tA_materialtotal+tA_leveling+tA_assemplytotal
  ### Type-B
  tB_total <- tB_materialtotal+tB_leveling+tB_assemplytotal
  ### Type-C
  tC_total <- tC_materialtotal+tC_leveling+tC_assemplytotal
  ### mean values
  meanmaterialstone <- mean(c(tA_materialstone,tB_materialstone,tC_materialstone))
  meanmaterialrubble <- mean(c(tA_materialrubble,tB_materialrubble,tC_materialrubble))
  meanleveling <- mean(c(tA_leveling,tB_leveling,tC_leveling))
  meanassemblyfacade <- mean(c(tA_assemblyfacade,tB_assemblyfacade,tC_assemblyfacade))
  meanassemblyfill <- mean(c(tA_assemblyfill,tB_assemblyfill,tC_assemblyfill))
  #meansup <- mean(c(tA_sup,tB_sup,tC_sup))
  meantotal <- mean(c(tA_total,tB_total,tC_total))
  averagewalltotal <- mean(c(tA_total,tB_total,tC_total))
  
  # Total by site in ph
  qin_stonelength <- 345053
  han_n_stonelength <- 8533
  han_s_stonelength <- 10012
  # Total by site in days
  workinghour <- 10
  qin_stonetotal <- averagewalltotal*qin_stonelength/workinghour
  han_n_stonetotal <- averagewalltotal*han_n_stonelength/workinghour
  han_s_stonetotal <- averagewalltotal*han_s_stonelength/workinghour
  
  # Total by site in ph
  qin_earth <- 79481
  han_n_earth <- 468398
  han_s_earth <- 398739
  qin_unsure <- 43212
  han_n_unsure <- 26544
  han_s_unsure <- 0
  qin_earthlength <- qin_earth + qin_unsure
  han_n_earthlength <- han_n_earth + han_n_unsure
  han_s_earthlength <- han_s_earth + han_s_unsure
  # Volume of the wall per meter (m^3)
  upper <- 1.5
  under <- 2.9
  height <- 3
  wallvolume <- 1*((upper+under)/2)*height
  #Xie Liye's experimental data m^3/person-day
  earthratehour <- earth
  earthrate <- earthratehour*workinghour
  # Total volume by site
  qin_earthvolume <- qin_earthlength*wallvolume
  han_n_earthvolume <- han_n_earthlength*wallvolume
  han_s_earthvolume <- han_s_earthlength*wallvolume
  
  qin_earthtotal <- qin_earthvolume/earthrate
  han_n_earthtotal <- han_n_earthvolume/earthrate
  han_s_earthtotal <- han_s_earthvolume/earthrate
  
  # Total length of stone-rammed-earth wall
  qinstoneearthlength <- 6521
  han_n_stoneearthlength <- 14822
  han_s_stoneearthlength <- 110676
  
  # Total workinghours of stone-earth wall. Working hous/10 hours per day
  qinstoneearthtotal <- ((meanmaterialstone + meanleveling + meanassemblyfacade)*qinstoneearthlength/workinghour+ (wallvolume/earthrate)*qinstoneearthlength)
  han_n_stoneearthtotal <- ((meanmaterialstone + meanleveling + meanassemblyfacade)*han_n_stoneearthlength/workinghour+ (wallvolume/earthrate)*han_n_stoneearthlength)
  han_s_stoneearthtotal <- ((meanmaterialstone + meanleveling + meanassemblyfacade)*han_s_stoneearthlength/workinghour+ (wallvolume/earthrate)*han_s_stoneearthlength)
  
  qinlabour <- qin_stonetotal + qin_earthtotal + qinstoneearthtotal
  han_n_labour <- han_n_stonetotal + han_n_earthtotal + han_n_stoneearthtotal
  han_s_labour <- han_s_stonetotal + han_s_earthtotal + han_s_stoneearthtotal
  
  # combine the ratio of the total Qin wall
  qinlength <- 474267
  han_n_length <- 518297
  han_s_length <- 519427
  qinlengthtotal <- 474267
  
  qinratio <- 0.94
  qinconstructtime <- 2*180*qinratio
  qinlabourforce <- qinlabour/qinconstructtime
  han_n_constructtime <- 180
  han_n_labourforce <- han_n_labour / han_n_constructtime
  han_s_constructtime <- 180
  han_s_labourforce <- han_s_labour / han_s_constructtime
  
  qinbeacon_stone <- 353
  qinbeacon_earth <- 438
  qinbeacon_earthstone <- 37 + qinbeacon_stone
  han_n_beacon_stone <- 5
  han_n_beacon_earth <- 2
  han_n_beacon_earthstone <- 4 + han_n_beacon_stone
  han_s_beacon_stone <- 37
  han_s_beacon_earth <- 56
  han_s_beacon_earthstone <- 5 + han_s_beacon_stone
  qinbeacon_total <- qinbeacon_earth + qinbeacon_earthstone
  han_n_beacon_total <- han_n_beacon_earth + han_n_beacon_earthstone
  han_s_beacon_total <- han_s_beacon_earth + han_s_beacon_earthstone
  
  # yard wall
  ### yard wall (by linear m)
  yardsectiondepth <- 0.5
  yardsectionheight <- 1
  yardvolsection <- 1*yardsectiondepth*yardsectionheight
  yard_volstones <- mean_facadevol*2*yardsectionheight
  yard_volrubble <- yardvolsection-yard_volstones
  
  #Yard wall ph/m
  yardwallheight <- 2
  yardwallwidth <- 0.5
  yardwalllength <- 65.25
  in_buildyard <- 1 / (6 * (4/2 + 0.2*4*(2-1) + 0.3/(2*0.5))) 
  ## Material acquisition
  yard_materialstone <- (yard_volstones+yard_volstones*in_quarrylost)*yardwallheight/in_quarrywall
  yard_materialrubble <- yard_volrubble*yardwallheight/in_quarryrubble
  yard_materialtotal <- yard_materialstone+yard_materialrubble
  ## Leveling the terrain
  yard_leveling <- yardwallwidth/in_leveling
  ## Wall assembly
  yard_assemblyfacade <- (mean_facadevol/in_buildyard)*yardwallheight*2
  yard_assemblyfill <- yard_volrubble*yardwallheight/in_buildrubbleearth
  yard_assemplytotal <- yard_assemblyfacade+yard_assemblyfill
  ## Supervision
  #yard_sup <- in_supervision*(yard_materialtotal+yard_leveling+yard_assemplytotal)
  ## Total
  yard_total <- yard_materialtotal+yard_leveling+yard_assemplytotal
  
  # qin stone beacon yard labour cost
  qinbeacon_yardcost <- qinbeacon_total * qin_beaconyard_length * yard_total
  qinbeacon_yardcost
  
  # qin earth beaon labour cost
  #Average volume of beacon towers in cubic metre m^3
  qinbeaconr1 <- 3
  qinbeaconr2 <- 5
  qinbeaconheight <- 4
  qin_beaconvolume <- (1/3)*pi*qinbeaconheight*(qinbeaconr1^2+qinbeaconr2^2+(qinbeaconr1*qinbeaconr2))
  qin_beaconearth_labour <- (qin_beaconvolume/earthrate) * qinbeacon_total
  
  han_n_beaconvolume <- 7.3*6.5*3
  han_s_beaconvolume <- 7.3*6.5*3
  
  qinbeacon_earthcost <- qinbeacon_total * (qin_beaconvolume/earthrate)
  han_n_beacon_earthcost <- han_n_beacon_total * (han_n_beaconvolume/earthrate)
  han_s_beacon_earthcost <- han_s_beacon_total * (han_s_beaconvolume/earthrate)
  qinbeacon_earthcost
  han_n_beacon_earthcost
  han_s_beacon_earthcost
  
  #Average volume of beacon towers in cubic metre m^3
  l <- sqrt((qinbeaconr1-qinbeaconr2)^2+qinbeaconheight^2)
  s <- pi*(qinbeaconr1^2 + qinbeaconr2^2 + qinbeaconr1*l + qinbeaconr2*l)- pi*qinbeaconr1^2
  in_buildqinbeacon <- 1 / (6 * (4/2 + 0.2*4*(l-1) + 0.3/(2*0.3)))
  #Average length of the walls of yards affiliated to Qin beacon tower in metre
  qinbeaconfacadelength <- s/l
  qinbeaconfacadedepth <- 0.3
  qinbeaconfacadesectionheight <- 1
  qinbeaconfacadevolsection <- 1*qinbeaconfacadedepth*qinbeaconfacadesectionheight
  qinbeaconfacade_volstones <- mean_facadevol*1*qinbeaconfacadesectionheight
  qinbeaconfacade_volrubble <- qinbeaconfacadevolsection-qinbeaconfacade_volstones
  
  #qin earth beacon stone cover labour cost ph/m
  qinbeaconfacadewallheight <- l
  qinbeaconfacadewallwidth <- 0.3
  
  ## Material acquisition
  qinbeaconfacade_materialstone <- (qinbeaconfacade_volstones+qinbeaconfacade_volstones*in_quarrylost)*qinbeaconfacadewallheight/in_quarrywall
  qinbeaconfacade_materialrubble <- qinbeaconfacade_volrubble*qinbeaconfacadewallheight/in_quarryrubble
  qinbeaconfacade_materialtotal <- qinbeaconfacade_materialstone+qinbeaconfacade_materialrubble
  ## Leveling the terrain
  qinbeaconfacade_leveling <- qinbeaconfacadewallwidth/in_leveling
  ## Wall assembly
  qinbeaconfacade_assemblyfacade <- (mean_facadevol/in_buildqinbeacon)*qinbeaconfacadewallheight*1
  qinbeaconfacade_assemblyfill <- qinbeaconfacade_volrubble*qinbeaconfacadewallheight/in_buildrubbleearth
  qinbeaconfacade_assemplytotal <- qinbeaconfacade_assemblyfacade+qinbeaconfacade_assemblyfill
  
  ## Total
  qinbeaconfacade_total <- qinbeaconfacade_materialtotal+qinbeaconfacade_leveling+qinbeaconfacade_assemplytotal+qinbeaconfacade_sup
  
  # Qin beacon stone cover tbl-costs ph
  qin_beaconfacade_length <- qinbeaconfacadelength*qinbeacon_earthstone
  qinbeacon_beaconfacade <- qinbeacon_earthstone * qin_beaconfacade_length* qinbeaconfacade_total
  
  ### yard wall (by linear m)
  hanbeaconfacadesectiondepth <- 1.1
  hanbeaconfacadesectionheight <- 1
  hanbeaconfacadevolsection <- 1*hanbeaconfacadesectiondepth*hanbeaconfacadesectionheight
  hanbeaconfacade_volstones <- mean_facadevol*1*hanbeaconfacadesectionheight
  hanbeaconfacade_volrubble <- hanbeaconfacadevolsection-hanbeaconfacade_volstones
  
  #Costs estimated for building beacon facade wall expressed as a linear rate of ph per m.
  hanbeaconfacadewallheight <- 3
  hanbeaconfacadewallwidth <- 1.1
  in_buildhanbeacon <- 1 / (6 * (4/2 + 0.2*4*(3-1) + 0.3/(2*1.1)))
  han_beaconvolume <- 7.5*6.5*3
  ## Material acquisition
  hanbeaconfacade_materialstone <- (hanbeaconfacade_volstones+hanbeaconfacade_volstones*in_quarrylost)*hanbeaconfacadewallheight/in_quarrywall
  hanbeaconfacade_materialrubble <- hanbeaconfacade_volrubble*hanbeaconfacadewallheight/in_quarryrubble
  hanbeaconfacade_materialtotal <- hanbeaconfacade_materialstone+hanbeaconfacade_materialrubble
  ## Leveling the terrain
  hanbeaconfacade_leveling <- hanbeaconfacadewallwidth/in_leveling
  ## Wall assembly
  hanbeaconfacade_assemblyfacade <- (mean_facadevol/in_buildhanbeacon)*hanbeaconfacadewallheight*1
  hanbeaconfacade_assemblyfill <- hanbeaconfacade_volrubble*hanbeaconfacadewallheight/in_buildrubbleearth
  hanbeaconfacade_assemplytotal <- hanbeaconfacade_assemblyfacade+hanbeaconfacade_assemblyfill
  ## Supervision
  #hanbeaconfacade_sup <- in_supervision*(hanbeaconfacade_materialtotal+hanbeaconfacade_leveling+hanbeaconfacade_assemplytotal)
  ## Total
  hanbeaconfacade_total <- hanbeaconfacade_materialtotal+hanbeaconfacade_leveling+hanbeaconfacade_assemplytotal+hanbeaconfacade_sup
  
  # Costs estimated for building beacon facade wall expressed as person-day. 
  han_n_beaconfacade_length <- 27*han_n_beacon_earthstone
  han_n_beacon_beaconfacade <- han_n_beacon_earthstone * han_n_beaconfacade_length* hanbeaconfacade_total
  han_s_beaconfacade_length <- 27*han_s_beacon_earthstone
  han_s_beacon_beaconfacade <- han_s_beacon_earthstone * han_s_beaconfacade_length* hanbeaconfacade_total
  
  qinbeacon_construction <- qinbeacon_earthcost+(qinbeacon_beaconfacade+qinbeacon_yardcost)/workinghour
  han_n_beacon_construction <- han_n_beacon_earthcost+(han_n_beacon_beaconfacade)/workinghour
  han_s_beacon_construction <- han_s_beacon_earthcost+(han_s_beacon_beaconfacade)/workinghour
  
  qintotal <-  qinlabour+qinbeacon_construction
  han_n_total <- han_n_labour+han_n_beacon_construction
  han_s_total <- han_s_labour+han_s_beacon_construction
  
  qin_personday_total <- qinlabour+qinbeacon_construction
  han_personday_total <- han_n_labour+han_n_beacon_construction+han_s_labour+han_s_beacon_construction
  
  qin_labourforce_total <- qin_personday_total*1.16/qinconstructtime
  han_n_labourforce_total <- (han_n_labour+han_n_beacon_construction)*1.16/han_n_constructtime
  han_s_labourforce_total <- (han_n_labour+han_n_beacon_construction)*1.16/han_s_constructtime
  
  # Immigration and cart transportation
  
  immigration <- 350000
  immigration_foodcost <- immigration * 360 *2 *0.83
  immigration_cart <- immigration_foodcost/250
  immigration_carthauler <- immigration_cart * 6
  
  qin_foodcost <- qintotal*0.83
  han_n_foodcost <- han_n_total*0.83
  han_s_foodcost <- han_s_total*0.83
  
  qin_foodcost_withloss <- qin_foodcost/0.39
  han_n_foodcost_withloss <- han_n_foodcost / 0.923
  han_s_foodcost_withloss <- han_s_foodcost / 0.923
  
  qin_cart <- qin_foodcost_withloss/250
  han_n_cart <- han_n_foodcost/250
  han_s_cart <- han_s_foodcost/250
  
  qin_carthauler <- qin_cart*6
  han_n_carthauler <- han_n_cart*6
  han_s_carthauler <- han_s_cart*6
  
  han_ns_labourforce_total <- han_n_labourforce_total + han_s_labourforce_total
  han_ns_carthauler <- han_n_carthauler + han_s_carthauler
  
  qin_carthauler_personday <- qin_carthauler * (800*2/25)
  
  han_carthauler_personday <- han_ns_carthauler * (100*2/25)
  
  return(list(
    qin_personday_total = qin_personday_total,
    han_personday_total = han_personday_total,
    
    qin_labourforce_total = qin_labourforce_total,
    han_n_labourforce_total = han_n_labourforce_total,
    han_s_labourforce_total = han_s_labourforce_total,
    han_ns_labourforce_total = han_ns_labourforce_total,
    
    qin_carthauler = qin_carthauler,
    immigration_carthauler = immigration_carthauler,
    han_n_carthauler = han_n_carthauler,
    han_s_carthauler = han_s_carthauler,
    han_ns_carthauler = han_ns_carthauler,
    
    qin_carthauler_personday = qin_carthauler_personday,
    han_carthauler_personday = han_carthauler_personday
  ))  
  
  
}


###############################

# Creating empty lists
qin_personday_list <- list()
han_personday_list <- list()

qin_workforce_list <- list()
han_n_workforce_list <- list()
han_s_workforce_list <- list()
han_ns_workforce_list <- list()

qin_carthauler_list <- list()
han_n_carthauler_list <- list()
han_s_carthauler_list <- list()
han_ns_carthauler_list <- list()

qin_carthauler_personday_list <- list()
han_carthauler_personday_list <- list()

pb <- txtProgressBar(min = 0, max = 1000, style = 3)

for (i in 1:1000) {
  # Choose radonmly
  quarrywall <- sample(quarrywall_list, 1)
  leveling <- sample(leveling_list, 1)
  upto0.02 <- sample(in_buildstoneupto0.02_list, 1)
  from0.02to0.05 <- sample(in_buildstone0.02to0.05_list, 1)
  over0.05 <- sample(in_buildstoneover0.05_list, 1)
  earth <- sample(earthrate_list, 1)
  
  results <- calc_indepentrates(quarrywall,leveling,upto0.02, from0.02to0.05,over0.05,earth)
  
  qin_personday_list <- append(qin_personday_list, results$qin_personday_total)
  han_personday_list <- append(han_personday_list, results$han_personday_total)
  
  qin_workforce_list <- append(qin_workforce_list, results$qin_labourforce_total)
  han_n_workforce_list <- append(han_n_workforce_list, results$han_n_labourforce_total)
  han_s_workforce_list <- append(han_s_workforce_list, results$han_s_labourforce_total)
  han_ns_workforce_list <- append(han_ns_workforce_list, results$han_ns_labourforce_total)
  
  qin_carthauler_list <- append(qin_carthauler_list, results$qin_carthauler)
  han_n_carthauler_list <- append(han_n_carthauler_list, results$han_n_carthauler)
  han_s_carthauler_list <- append(han_s_carthauler_list, results$han_s_carthauler)
  han_ns_carthauler_list <- append(han_ns_carthauler_list, results$han_ns_carthauler)
  
  qin_carthauler_personday_list <- append(qin_carthauler_personday_list, results$qin_carthauler_personday)
  han_carthauler_personday_list <- append(han_carthauler_personday_list, results$han_carthauler_personday)
  
  setTxtProgressBar(pb, i)
}
close(pb)

################直方图#####################
# 创建一个包含所有数据列的数据框
data_df_10h <- data.frame(
  qin_personday = unlist(qin_personday_list),
  han_personday = unlist(han_personday_list),
  
  qin_workforce = unlist(qin_workforce_list),
  han_n_workforce = unlist(han_n_workforce_list),
  han_s_workforce = unlist(han_s_workforce_list),
  han_ns_workforce = unlist(han_ns_workforce_list),
  
  qin_carthauler = unlist(qin_carthauler_list),
  han_n_carthauler = unlist(han_n_carthauler_list),
  han_s_carthauler = unlist(han_s_carthauler_list),
  han_ns_carthauler = unlist(han_ns_carthauler_list),
  
  qin_carthauler_personday = unlist(qin_carthauler_personday_list),
  han_carthauler_personday = unlist(han_carthauler_personday_list)
)

density_qin_personday_10h <- density(data_df_10h$qin_personday, na.rm = TRUE)
density_han_personday_10h <- density(data_df_10h$han_personday, na.rm = TRUE)

density_qin_workforce_10h <- density(data_df_10h$qin_workforce, na.rm = TRUE)
density_han_n_workforce_10h <- density(data_df_10h$han_n_workforce, na.rm = TRUE)
density_han_s_workforce_10h <- density(data_df_10h$han_s_workforce, na.rm = TRUE)
density_han_ns_workforce_10h <- density(data_df_10h$han_ns_workforce, na.rm = TRUE)

density_qin_carthauler_10h <- density(data_df_10h$qin_carthauler, na.rm = TRUE)
density_han_n_carthauler_10h <- density(data_df_10h$han_n_carthauler, na.rm = TRUE)
density_han_s_carthauler_10h <- density(data_df_10h$han_s_carthauler, na.rm = TRUE)
density_han_ns_carthauler_10h <- density(data_df_10h$han_ns_carthauler, na.rm = TRUE)

density_qin_carthauler_personday_10h <- density(data_df_10h$qin_carthauler_personday, na.rm = TRUE)
density_han_carthauler_personday_10h <- density(data_df_10h$han_carthauler_personday, na.rm = TRUE)


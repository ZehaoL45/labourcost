################################################################################################
#
# CODE NAME:       	plot_uncertainty
# 
# AUTHOR(S):		Zehao Li, University College London (zehao.li@ucl.ac.uk) 
#                       
# PURPOSE:		After the Monte-Carlo simulation for the case of 5 and 10 hours, run this code to 
#             plot the results.
#
# ACKNOWLEDGEMENTS:	Giacomo Fontana, Andrew Bevan
# COPYRIGHT:		(C) 2024 by Zehao Li
# LICENCE：		MIT LICENCE
#			
################################################################################################

# Set the layout to display 4 plots in one column and configure margins for each plot.
par(mfrow=c(4, 1), mar=c(4, 4, 2, 1), oma=c(0,0,0,0), xpd=FALSE)

# The intention here seems to set plot border color, but `par(col=NA)` does not affect border colors. This line might not have the intended effect.
par(col=NA)

# Set options to prevent scientific notation in plots, aiming for readability.
options(scipen = 999)

# Combine all x values from various density datasets to determine a consistent x-axis range for all plots.
all_x_values <- c(density_qin_personday_5h$x, density_qin_personday_10h$x, 
                  density_qin_carthauler_personday_5h$x, density_qin_carthauler_personday_10h$x,
                  density_han_personday_5h$x, density_han_personday_10h$x,
                  density_han_carthauler_personday_5h$x, density_han_carthauler_personday_10h$x)

# Calculate the minimum and maximum x values for setting consistent x-axis limits.
min_x <- min(all_x_values)
max_x <- max(all_x_values)

# Find the maximum y values (density) among all datasets for Qin and Han, both for personday and carthauler scenarios, to set consistent y-axis limits.
max_density_qin_personday <- max(c(density_qin_personday_5h$y, density_qin_personday_10h$y))
max_density_qin_carthauler <- max(c(density_qin_carthauler_personday_5h$y, density_qin_carthauler_personday_10h$y))
max_density_han_personday <- max(c(density_han_personday_5h$y, density_han_personday_10h$y))
max_density_han_carthauler <- max(c(density_han_carthauler_personday_5h$y, density_han_carthauler_personday_10h$y))

# Plot density graphs for the Qin wall labor force with uniform x and y axes. Use `polygon` for area fill and `lines` for overlay curves.
plot(density_qin_personday_5h, xlab="", ylab="Density", main="", xlim=c(min_x, max_x), ylim=c(0, max_density_qin_personday), col="#BEB8DC", lwd=2)
polygon(density_qin_personday_5h, col="#BEB8DC", border=NA)
lines(density_qin_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_qin_personday_10h, col="#FA7F6F", border=NA)

# Repeat the plotting process for Qin wall transportation requirements.
plot(density_qin_carthauler_personday_5h, main="", xlab="", ylab="Density", ylim=c(0, max_density_qin_carthauler), xlim=c(min_x, max_x), col="#BEB8DC", lwd=2)
polygon(density_qin_carthauler_personday_5h, col="#BEB8DC", border=NA)
lines(density_qin_carthauler_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_qin_carthauler_personday_10h, col="#FA7F6F", border=NA)

# Plot density graphs for the Han wall labor force, following the same steps as for the Qin plots.
plot(density_han_personday_5h, main="", xlab="", ylab="Density", xlim=c(min_x, max_x), ylim=c(0, max_density_han_personday), col="#BEB8DC", lwd=2)
polygon(density_han_personday_5h, col="#BEB8DC", border=NA)
lines(density_han_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_han_personday_10h, col="#FA7F6F", border=NA)

# Repeat the plotting process for Han wall transportation requirements.
plot(density_han_carthauler_personday_5h, main="", xlab="", ylab="Density", xlim=c(min_x, max_x), ylim=c(0, max_density_han_carthauler), col="#BEB8DC", lwd=2)
polygon(density_han_carthauler_personday_5h, col="#BEB8DC", border=NA)
lines(density_han_carthauler_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_han_carthauler_personday_10h, col="#FA7F6F", border=NA)

# Reset the plot layout parameters to default after finishing the multi-panel plot.
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1, col="#000000")

# These additional plot and par settings appear to be incomplete or redundant since the main plotting sequence is already finished.
par(col=NA)

# Plot density graphs for the Han wall labor force and carthauler
min_x_han <- min(density_han_personday_5h$x,density_han_personday_10h$x)
max_x_han <- max(density_han_personday_5h$x,density_han_personday_10h$x)
min_x_han_cart <- min(density_han_carthauler_personday_5h$x,density_han_carthauler_personday_10h$x)
max_x_han_cart <- max(density_han_carthauler_personday_5h$x,density_han_carthauler_personday_10h$x)
options(scipen = 999)
plot(density_han_personday_5h, main="", xlab="", ylab="Density", xlim=c(min_x_han, 6000000), ylim=c(0, max_density_han_personday), col="#BEB8DC", lwd=2)
polygon(density_han_personday_5h, col="#BEB8DC", border=NA)
lines(density_han_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_han_personday_10h, col="#FA7F6F", border=NA)

plot(density_han_carthauler_personday_5h, main="", xlab="", ylab="Density", xlim=c(min_x_han_cart, max_x_han_cart), ylim=c(0, max_density_han_carthauler), col="#BEB8DC", lwd=2)
polygon(density_han_carthauler_personday_5h, col="#BEB8DC", border=NA)
lines(density_han_carthauler_personday_10h, col="#FA7F6F", lwd=2)
polygon(density_han_carthauler_personday_10h, col="#FA7F6F", border=NA)



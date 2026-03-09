# Load whole dataset
data(mtcars)
# Get the library.
library(plotrix)
# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon",
        main = "Mileage Data",
        notch = TRUE,
        varwidth = TRUE,
        col = c("green","yellow","purple"),
        names = c("High","Medium","Low")
)
# Get the input values.
input <- mtcars[,c('wt','mpg')]
# Plot the chart
plot(x = input$wt,y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),
     main = "Weight vs Milage")

# Average Horsepower by Number of Cylinders
avg_hp <- tapply(mtcars$hp, mtcars$cyl, mean)
cyl_labels <- paste0(names(avg_hp), "-cyl")

barplot(avg_hp,
        names.arg = cyl_labels,
        xlab      = "Number of Cylinders",
        ylab      = "Average Horsepower (hp)",
        main      = "Average Horsepower by Cylinder Count",
        col       = c("steelblue", "orange", "firebrick"),
        border    = "black",
        ylim      = c(0, max(avg_hp) * 1.2))

# Add value labels on top of each bar
text(x      = c(0.7, 1.9, 3.1),
     y      = avg_hp + 5,
     labels = paste0(round(avg_hp, 1), " hp"),
     cex    = 1.0,
     font   = 2)

# Engine Displacement vs Gross Horsepower (sorted by disp)
mtcars_sorted <- mtcars[order(mtcars$disp), ]

plot(x    = mtcars_sorted$disp,
     y    = mtcars_sorted$hp,
     type = "o",
     col  = "darkorange",
     pch  = 16,
     lwd  = 1.8,
     xlab = "Engine Displacement (cu. in.)",
     ylab = "Gross Horsepower (hp)",
     main = "Engine Displacement vs Horsepower Trend")

# Overlay a smooth trend line for clarity
lines(lowess(mtcars_sorted$disp, mtcars_sorted$hp),
      col = "darkred",
      lwd = 2.5,
      lty = 2)

legend("topleft",
       legend = c("Individual Cars", "Trend (LOWESS)"),
       col    = c("darkorange", "darkred"),
       pch    = c(16, NA),
       lty    = c(1, 2),
       lwd    = c(1.8, 2.5),
       cex    = 0.9)

# Proportion of Cars by Transmission Type & Cylinder Group
# Create 6 groups: Auto/Manual × 4/6/8 cyl
mtcars$group <- paste0(ifelse(mtcars$am == 0, "Auto", "Manual"),
                       "-", mtcars$cyl, "cyl")

group_counts <- sort(table(mtcars$group), decreasing = TRUE)
group_pct    <- round(100 * group_counts / sum(group_counts), 1)
group_labels <- paste0(names(group_counts), "\n(", group_pct, "%)")

slice_colors <- c("steelblue", "firebrick", "gold",
                  "mediumseagreen", "orchid", "darkorange")

pie3D(as.numeric(group_counts),
      labels   = group_labels,
      explode  = 0.08,
      col      = slice_colors,
      main     = "Fleet Breakdown: Transmission Type × Cylinder Count",
      labelcex = 0.78,
      theta    = 0.8)

DataFrame <- read.csv("supermarket.csv")

par(mfrow = c(1, 1))  

# Plot 1
plot(DataFrame$unit_cost, DataFrame$satisfaction_score,
     main = "Unit Cost vs Satisfaction",
     xlab = "Unit Cost",
     ylab = "Satisfaction Score",
     ylim = c(0, 6),
     pch=19,
     col = c("blue","red"))
legend("topleft",legend=c("Unit Cost","Satistfaction Score"),col=c("blue","red"),pch=19)
# Plot 2
dept_items <- tapply(DataFrame$items_count, DataFrame$department, sum)
barplot(dept_items,
        main = "Total Items Count by Department",
        xlab = "Department",
        ylab = "Total Items Count",
        col = "lightblue",
        ylim=c(0,2000),
        las = 2)   
# Plot 3
electronics <- DataFrame[DataFrame$department == "Electronics",
                         c("department", "city_branch")]
counts <- table(electronics$city_branch)
percentages <- round(100 * counts / sum(counts), 1)
labels <- paste(names(counts), " ", percentages, "%")

pie(counts,
    main = "Electronics Sales by City",
    col = rainbow(length(counts)),
    labels = labels)
legend("topleft",legend = names(counts),col= rainbow(length(counts)),pch=19)

# Plot 4
boxplot(DataFrame$unit_cost,
        main = "Boxplot of Unit Cost",
        xlab = "Unit Cost",
        ylab = "Value",
        col = "lightblue",
        outline = TRUE,        
        pch = 19,  
        horizontal = FALSE)

# Plot 5
hist(DataFrame$satisfaction_score,
     main = "Satisfaction Score Histogram",
     xlab = "Satisfaction Score",
     ylim=c(0,20),
     col = "lightblue")

# Plot 6
avg_scores <- tapply(DataFrame$satisfaction_score,
                     DataFrame$department,
                     mean,
                     na.rm = TRUE)

dotchart(avg_scores,
         main = "Avg Satisfaction by Dept",
         xlab = "Average Score",
         ylab = "Department",
         col = "blue",
         pch = 19)

# Plot 7
high_sat <- DataFrame[DataFrame$delivery_time >= 90, ]
summary_table <- table(high_sat$department)

barplot(summary_table,
        main = "High Delivery Time by Dept",
        xlab = "Department",
        ylab = "Number of Orders",
        col = "blue",
        las = 2)

# Plot 8
plot(density(DataFrame$satisfaction_score, na.rm = TRUE),
     main = "Density of Satisfaction",
     xlab = "Satisfaction Score",
     ylab = "Density",
     col = "blue",
     lwd = 2)

# Plot 9 
vars <- DataFrame[, c("delivery_time", "satisfaction_score", "unit_cost")]
pairs(vars,
      main = "Pairwise Relationships",
      pch = 19,
      col =c("red", "green", "blue"))

legend("topright",
       legend = colnames(vars),
       col = c("red", "green", "blue"),pch=19)
#Question 10 combine from 1-8
#For Better Visibility I did each one alone above

par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
plot(DataFrame$unit_cost, DataFrame$satisfaction_score,
     main = "Unit Cost vs Satisfaction",
     xlab = "Unit Cost",
     ylab = "Satisfaction Score",
     ylim = c(0, 8),
     pch=19,
     col = c("red", "blue"))
dept_items <- tapply(DataFrame$items_count,
                     DataFrame$department,
                     sum,
                     na.rm = TRUE)
dept_items <- tapply(DataFrame$items_count,
                     DataFrame$department,
                     sum,
                     na.rm = TRUE)
barplot(dept_items,
        main = "Total Items Count by Department",
        xlab = "Department",
        ylab = "Total Items Count",
        col = "lightblue",
        ylim=c(0,2000),
        las = 2)   
pie(counts,
    main = "Electronics Sales by City",
    col = rainbow(length(counts)),
    labels = labels)
boxplot(DataFrame$unit_cost,
        main = "Boxplot of Unit Cost",
        xlab = "Unit Cost",
        ylab = "Value",
        col = "lightblue",
        horizontal = FALSE)
hist(DataFrame$satisfaction_score,
     main = "Satisfaction Score Histogram",
     xlab = "Satisfaction Score",
     col = "lightblue")
dotchart(avg_scores,
         main = "Avg Satisfaction by Dept",
         xlab = "Average Score",
         ylab = "Department",
         col = "blue",
         pch = 19)
barplot(summary_table,
        main = "High Delivery Time by Dept",
        xlab = "Department",
        ylab = "Number of Orders",
        col = "blue",
        las = 2)
plot(density(DataFrame$satisfaction_score, na.rm = TRUE),
     main = "Density of Satisfaction",
     xlab = "Satisfaction Score",
     ylab = "Density",
     col = "blue",
     lwd = 2)
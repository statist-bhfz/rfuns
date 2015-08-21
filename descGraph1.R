descGraph1 <- function(x, 
                       statistics = c(2:5, 8, 9), 
                       plot_type = "boxplot", 
                       height = 25, 
                       width = 100,
                       lang = "en") {
    
    # Created by Andrey Ogurtsov, 02.08.2015
    
    require(psych)

    # Filename
    fname <- tempfile(pattern = "graph", 
                      tmpdir = paste(getwd(), "figures", sep = "/"))
    fname <- unlist(strsplit(fname, "[\\]"))[2]
    fname <- paste("figures/", fname, ".png", sep = "")
    
    # Plot
    png(filename = fname, width = width, height = height)
    par(oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), plt=c(0, 1, 0, 1), bty="n")
    if (plot_type == "boxplot") {
        boxplot(x, xaxt = "n", horizontal=TRUE) 
    } 
    if (plot_type == "hist") {
        hist(x, main="", yaxt = "n", breaks="FD") # Freedman-Diaconis rule
    }
    if (plot_type == "stripchart") {
    stripchart(x, xaxt = "n", method = "jitter", 
               jitter = 0.2, pch = 1, cex = 0.7)
    }
    dev.off()
    
    # Creating image link in markdown format
    imglink <- paste("![alt text](", fname, ")", sep="")
    
    # Final table
    tabl <- describe(x)[, statistics]
    
    # Rename columns
    if (lang == "ru") {
        ru_colnames <- c("Переменные", "n", "Среднее", "Станд. отклон.",
                         "Медиана", "Усеч. ср.", "Мед. абс. откл.",
                         "Мин.", "Макс.", "Размах", "Асимм.", "Эксцесс", 
                         "Станд. ош.")
        colnames(tabl) <- ru_colnames[statistics]
    }
    
    if (lang == "ua") {
        ua_colnames <- c("Змінні", "n", "Середнє", "Станд. відх.",
                         "Медіана", "Усіч. сер.", "Мед. абс. відх.",
                         "Мін.", "Макс.", "Розмах", "Асимм.", "Ексцесс", 
                         "Станд. пох.")
        colnames(tabl) <- ua_colnames[statistics]
    }
    
    tabl$graph <- imglink
    colnames(tabl)[colnames(tabl) == "graph"] <- ""
    
    # unlink will not work with apply
    
    return(tabl)
}

# Examples
# kable(descGraph1(x=cars$speed, plot_type = "stripchart"))
# kable(descGraph1(x=cars$speed, plot_type = "hist"))

# result <- apply(cars, 2, descGraph1, lang="ru")
# kable(do.call(rbind, result))
# ___ covariance tests between each bloc's recordings

df_covmat <- data.frame(Date = Date)

for (i in 1:length(valid)){
    df_covmat[ , i +1 ] <- df_difftilts[ , i + 1 ]
}

for (i in 1:length(valid)){
    var <- xts::xts(
        x = df_covmat[, i+1],
        order.by = df_covmat[, 1]
    )
    var <- xts::apply.weekly(
        var,
        sum,
        na.rm = TRUE
    )
    if (i == 1){
        df_wcovmat <- data.frame(Date = zoo::index(var))
    }
    var <- as.numeric(var)
    var[var == 0] <- NA
    df_wcovmat[,i+1] <- var
}

df_covmat 	<- df_covmat[,-1]
df_wcovmat 	<- df_wcovmat[,-1]

for (i in 1:length(valid)){
    colnames(df_covmat)[i] 		<- paste0("b_", valid[i])
    colnames(df_wcovmat)[i]		<- paste0("b_", valid[i])
}

# coefficients

coef_n 	<-  Hmisc::rcorr(as.matrix(df_covmat), type = "spearman")$n
coef_r 	<-  round(Hmisc::rcorr(as.matrix(df_covmat), type = "spearman")$r, 7)
coef_p 	<-  round( Hmisc::rcorr(as.matrix(df_covmat), type = "spearman")$P, 3)

# shape

get_tri	 <-function(matrix, type = c("lower", "upper")){
    if (type == "lower"){
        matrix[lower.tri(matrix)]	<- NA
    } else if (type == "upper"){
        matrix[upper.tri(matrix)]	<- NA
    }
    return(matrix)
}

reorder <- function(cormat){
    dd	<- dist((1-(cormat)) /2, method = "minkowski" )
    hc	<- hclust(dd, method = "complete")
    cormat <-cormat[hc$order, hc$order]
}

coef_r 	<- get_tri(coef_r, "upper")
coef_p 	<- get_tri(coef_p, "upper")
coef_n 	<- get_tri(coef_n, "upper")

for (i in 1:nrow(coef_n)){
    coef_n[ i, i ] 	<- NA
}
for (i in 1:nrow(coef_r)){
    coef_r[ i, i ] 	<- NA
}
for (i in 1:nrow(coef_p)){
    coef_p[ i, i ] 	<- NA
}

# melt

coef <- reshape2::melt(
    coef_r,
    na.rm = FALSE,
    value.name = "r"
)

# class

coef$c 	<- base::cut(
    coef$r,
    breaks = c( -1, -.5, -.25, .25, .5, 1 ),
    include.lowest = TRUE,
    label = c(
        "[ -1.00 ; -0.50 [",
        "[ -0.50 ; -0.25 [",
        "[ -0.25 ;  0.25 [",
        "[  0.25 ;  0.50 [",
        "[  0.50 ;  1.00 ]"
    ),
    right = FALSE
)

class_p <- function(array_pval, alpha){
    alpha <- as.numeric(alpha)
    p_ <- data.frame(
        p = array_pval,
        class = NA
    )
    for (i in 1:length(p_$p)){
        if (!is.na(p_$p[i])){
            if (p_$p[ i ] > alpha){
                p_$class[ i ] <- "–"
            }
            if (p_$p[ i ] <= alpha){
                p_$class[ i ] <- ""
            }
        } else {
            p_$class[ i ] <- NA
        }
    }
    return(p_$class)
}

coef$n 			<- reshape2::melt(coef_n, na.rm = FALSE)$value
coef$p 			<- reshape2::melt(coef_p, na.rm = FALSE)$value
coef$p_class	<- class_p(coef$p, .05)

coef <- na.omit(coef)

# plot 

quartz(
    "covariances",
    height = 5, width = 5,
    pointsize = 12
)
par(oma=c(rep(0, 4)))

require(ggplot2)

ggplot2::ggplot(
    data = coef,
    ggplot2::aes(x = Var1, y = Var2)
) +
    ggplot2::geom_tile(
        ggplot2::aes(fill = c),
        color = "white",
    ) +
    ggplot2::scale_fill_brewer(
        palette = "RdBu",
        name = "Correlation coefficient",
        drop = FALSE
    ) +
    ggplot2::geom_text(
        ggplot2::aes(label = round(r, 1)),
        size = 0
    ) +
    ggplot2::geom_text(
        ggplot2::aes(label = p_class),
        size = 4
    ) +
    ggplot2::theme_minimal(
        base_size = 11
    ) +
    ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 90, vjust = 0.4),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(
            fill = rgb( .5, .5, .5, .15),
            color = "grey90",
        ),
        legend.position = c(0.2, 0.77),
        legend.direction = "vertical",
        plot.margin = unit(rep(0, 4), units = "cm" ),
        complete = T
    ) +
    ggplot2::coord_fixed()

# stats

mean_cor 		<- round(mean((coef$r), na.rm=T), 2)
mean_abs_cor 	<- round(mean(abs(coef$r), na.rm=T), 2)

sum <- as.data.frame(summary(coef$c))
sum <- data.frame(classes=rownames(sum), n = sum[, 1])

mod_str_neg 	<- round(sum(sum[c(1,2) ,2]), 0)
mod_str_pos 	<- round(sum(sum[c(4,5) ,2]), 0)
mod_null 		<- sum[3, 2]

print(sum)

stats <- matrix(
    data = c(
        round(mod_str_neg, 0),
        round(mod_str_pos, 0),
        mod_null,
        mean_cor,
        mean_abs_cor
    ),
    ncol = 1,
    nrow = 5,
)
stats <- as.data.frame(stats)
colnames(stats) <- NULL
rownames(stats) <- c(
    "negative correlations",
    "positive correlations",
    "no associations",
    "mean correlation coefficient",
    "mean absolute correlation coefficient"
)

print("blocs' correlations")
print(stats)

# save plot

quartz.save(
    file = file.path(dir_plt, "models", "mdl_11.png"),
    type = "png",
    dpi = 300
)

dev.off()
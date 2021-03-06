#' Create a non-linear Regression for a Michaelis-Menten type Enzyme with a little knowlege of its Km and Vmax
#'
#' @param sub substrate concentration
#' @param vel enzymatic velocity
#' @param xlab lable of the abscissa
#' @param ylab lable of the ordinate
#' @param title title of the plot
#'


plot_MM_direct <- function(sub, velo, title = "title", xlab = "abscisaa", ylab = "ordinate"){

    # Fitten des SSmicmen-Modell

    fit <- nls(velo ~ SSmicmen(sub, Vm, K)) %>%
        print()

    # plotten des Fits

    ggplot2::ggplot(mapping = ggplot2::aes(x = sub, y = velo))+
       ggplot2::geom_point()+
       ggplot2::stat_function(fun = function(sub){ (coef(fit)[[1]] * sub) / ( coef(fit)[[2]] + sub)}, color = "blue")+ # einzeichnen des Fitting
       ggplot2::geom_hline(yintercept = coef(fit)[[1]])+ # Vmax aus coefficents eintragen
       ggplot2::geom_text(ggplot2::aes(0,coef(fit)[[1]],label = round(coef(fit)[[1]], digits = 3), vjust = 1.4), hjust = .3 )+ # Vmax aus coefficents eintragen
       ggplot2::geom_hline(yintercept =( coef(fit)[[1]])/2,)+ # Vmax/2 aus coefficents
       ggplot2::geom_text(ggplot2::aes(0,(coef(fit)[[1]])/2,label = "Vmax/2", vjust = 1.4), hjust = .3)+ # Vmax/2 aus coefficents eintragen
       ggplot2::geom_vline(xintercept = coef(fit)[[2]])+ # Km aus coefficents
       ggplot2::geom_text(ggplot2::aes(0,coef(fit)[[2]],label = round(coef(fit)[[2]], digits = 3), vjust = 1.41), hjust = -2.3)+ # Km aus coefficents eintragen
       ggplot2::xlab(xlab)+
       ggplot2::ylab(ylab)+
       ggplot2::ggtitle(title)
    }


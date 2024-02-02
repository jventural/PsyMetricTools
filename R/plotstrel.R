plotstrel <- function(x, estimate, blackwhite = FALSE, criteria = TRUE, cuts = c(.70, .80), twopie = FALSE){

  posi <- grep(estimate, x$estimates, ignore.case = TRUE)
  samp <- coda::as.mcmc(unlist(x$Bayes$samp[posi]))
  n.item <- x$n.item

  # Verifica si prior está definido, si no, usa los parámetros para generar una distribución a priori
  prior <- unlist(x$priors[posi])
  if (is.null(prior) || length(prior) < 2) {
    if (is.null(x$priors) || is.null(x$priors$a0) || is.null(x$priors$b0)) {
      stop("No se pueden generar datos a priori: faltan parámetros.")
    }
    n <- 1000  # Define el número de muestras para la distribución a priori
    prior <- MCMCpack::rinvgamma(n, shape = x$priors$a0, scale = x$priors$b0)
  }

  # Configuración de parámetros gráficos
  par(cex.main = 1.5, mar = c(4, 4,  1, 1), mgp = c(2, .6, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)

  hdi <- coda::HPDinterval(samp)
  med <- median(samp)
  peak <- max(density(samp)$y)
  rad <- .04

  colos <- c("firebrick", "cornflowerblue","navy")
  if (blackwhite){
    colos <- c("gray100", "gray70", "gray10")
  }
  if (length(prior) < 2) {
    stop("El vector 'prior' no tiene suficientes datos.")
  }

  dens.prior <- density(prior, from = 0, to = 1, n = 2e3)
  options(warn = -1)
  xx0 <- min(which(dens.prior$x <= cuts[1]))
  xx1 <- max(which(dens.prior$x <= cuts[1]))
  xx2 <- max(which(dens.prior$x <= cuts[2]))
  xx3 <- max(which(dens.prior$x <= 1))

  if (!is.integer(xx0)) xx0 <- 1
  if (!is.integer(xx1)) xx1 <- 1
  if (!is.integer(xx2)) xx2 <- 1
  if (!is.integer(xx3)) xx3 <- 1

  dens.post <- density(samp, adjust = 1.75, n = 2e3)
  x0 <- min(which(dens.post$x <= cuts[1]))
  x1 <- max(which(dens.post$x <= cuts[1]))
  x2 <- max(which(dens.post$x <= cuts[2]))
  x3 <- max(which(dens.post$x <= 1))
  options(warn=0)
  if (!is.integer(x0)) x0 <- 1
  if (!is.integer(x1)) x1 <- 1
  if (!is.integer(x2)) x2 <- 1
  if (!is.integer(x3)) x3 <- 1

  y1 <- sum(prior <= cuts[1])
  y2 <- sum(prior <= cuts[2])
  y3 <- sum(prior <= 1)

  z1 <- sum(samp <= cuts[1])
  z2 <- sum(samp <= cuts[2])
  z3 <- sum(samp <= 1)

  pie.prior <- c(y1, y2-y1, y3-y2)
  pie.prior[pie.prior == 0] <- 1e-20
  pie.prior.labels <- as.character(round(pie.prior/(length(prior)*1e-2), 1))

  pie.post <- c(z1, z2-z1, z3-z2)
  pie.post[pie.post == 0] <- 1e-20
  pie.post.labels <- as.character(round(pie.post/(length(samp)*1e-2), 1))

  for (i in 1:3){
    pie.prior.labels[i] <- paste(pie.prior.labels[i], "%")
    pie.post.labels[i] <- paste(pie.post.labels[i], "%")
  }

  # ------------------------------- plotting --------------------------------------

  plotShadePost <- function(dens, xx, cols, criteria, blackwhite){
    if (criteria){
      color_transp <- adjustcolor(cols, alpha.f = .7)
      if (blackwhite) {color_transp <- adjustcolor(cols, alpha.f = .8)}
      with(dens, polygon(x[c(xx[1],xx[1]:xx[2],xx[2])], c(0, y[xx[1]:xx[2]], 0), col = color_transp[1]))
      with(dens, polygon(x[c(xx[2],xx[2]:xx[3],xx[3])], c(0, y[xx[2]:xx[3]], 0), col = color_transp[2]))
      with(dens, polygon(x[c(xx[3],xx[3]:xx[4],xx[4])], c(0, y[xx[3]:xx[4]], 0), col = color_transp[3]))
    }
    else {
      color_transp <- adjustcolor(cols[3], alpha.f = .7)
      if (blackwhite) {color_transp <- adjustcolor(cols[3], alpha.f = .8)}
      with(dens, polygon(x[c(xx[1],xx[1]:xx[4],xx[4])], c(0, y[xx[1]:xx[4]], 0), col = color_transp))
    }

  }

  plotShadePrior <- function(dens, xx, cols, criteria, blackwhite){
    if (criteria){
      color_transp <- adjustcolor(cols, alpha.f = .5)
      if (blackwhite){color_transp <- adjustcolor(cols, alpha.f = .7)}
      with(dens, polygon(x[c(xx[1],xx[1]:xx[2],xx[2])], c(0, y[xx[1]:xx[2]], 0), col = color_transp[1]))
      with(dens, polygon(x[c(xx[2],xx[2]:xx[3],xx[3])], c(0, y[xx[2]:xx[3]], 0), col = color_transp[2]))
      with(dens, polygon(x[c(xx[3],xx[3]:xx[4],xx[4])], c(0, y[xx[3]:xx[4]], 0), col = color_transp[3]))
    }
    else {
      color_transp <- adjustcolor(cols[2], alpha.f = .5)
      if (blackwhite) {color_transp <- adjustcolor(cols[2], alpha.f = .7)}
      with(dens, polygon(x[c(xx[1],xx[1]:xx[4],xx[4])], c(0, y[xx[1]:xx[4]], 0), col = color_transp))
    }
  }

  if (criteria){
    if (twopie){
      plot(density(samp, adjust = 1.75), type = "l", axes = F, xlab = "Reliability", ylab = NA,
           xlim = c(0, 1), ylim = c(-.1,  peak * 1.55),
           lwd = 3, main = "")
      plotShadePrior(dens.prior, xx = c(xx0, xx1, xx2, xx3), cols = colos, criteria = criteria, blackwhite = blackwhite)
      plotShadePost(dens.post, xx = c(x0, x1, x2, x3), cols = colos, criteria = criteria, blackwhite = blackwhite)

      lines(density(prior, from = 0, to = 1), lty = 2, lwd = 3)
      axis(side = 1, at = seq(0, 1, by = .2), labels = seq(0, 1, by = .2), cex.axis = 1.2, lwd = 1.5)
      axis(side = 2, at = seq(0, peak, by = peak/5), labels = NA, cex.axis = 1.2, lwd = 1.5)

      title(ylab = "Density", mgp = c(1, 1, 0), adj = 0.31)
      arrows(x0 = hdi[1], y0 = peak, x1 = hdi[2], y1 = peak, angle = 90, length = 0.05,
             code = 3, lwd = 2)

      t1 <- legend(x = .95, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
      text(t1$rect$left + t1$rect$w, t1$text$y*.99,
           c("", paste("median = ", round(med, 3), sep =""),
             paste("95% HDI: [", round(hdi[1], 3), ", ", round(hdi[2], 3),"]", sep ="")),
           cex = 1.2, pos = 2)

      legend(x = 0, y = peak, lty = c(1, 3), lwd = 2, c("Posterior", "Prior"), bty = "n", cex = 1.2)

      text("insufficient", x = cuts[1]/2, y = peak*-.03, adj = 0.5, cex = 1.2)
      text("sufficient", x = (cuts[1] + cuts[2])/2, y = peak*-.03, adj = .5, cex = 1.2)
      text("good", x = (cuts[2] + 1)/2, y = peak*-.03, adj = .5, cex = 1.2)
      legend(x = 0, y = peak*1.33,  fill=colos, horiz=F, cex=1.2, bty = "n",
             c("insufficient:", "sufficient:", "good:"))

      f.prior <- plotrix::floating.pie(xpos = .3, ypos = peak*1.4, x = pie.prior, radius = rad,
                                       col = colos, startpos = 0)
      f.post <- plotrix::floating.pie(xpos = .45, ypos = peak*1.4, x = pie.post, radius = rad,
                                      col = colos, startpos = 0)
      text("prior", x = .3, y = peak*1.53, cex = 1.2)
      text("posterior", x = .45, y = peak*1.53, cex = 1.2)

      l1 <- legend(x = .29, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
      l2 <- legend(x = .44, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
      text(l1$rect$left + l1$rect$w, l1$text$y*.99, c(pie.prior.labels), pos = 2, cex = 1.2)
      text(l2$rect$left + l2$rect$w, l2$text$y*.99, c(pie.post.labels), pos = 2, cex = 1.2)

    } else {
      plot(density(samp, adjust = 1.75), type = "l", axes = F, xlab = "Reliability", ylab = NA,
           xlim = c(0, 1), ylim = c(-.1,  peak * 1.33),
           lwd = 3, main = "")
      plotShadePrior(dens.prior, xx = c(xx0, xx1, xx2, xx3), cols = colos, criteria = criteria, blackwhite = blackwhite)
      plotShadePost(dens.post, xx = c(x0, x1, x2, x3), cols = colos, criteria = criteria, blackwhite = blackwhite)

      lines(density(prior, from = 0, to = 1), lty = 3, col = "firebrick", lwd = 3)
      axis(side = 1, at = seq(0, 1, by = .2), labels = seq(0, 1, by = .2), cex.axis = 1.2, lwd = 1.5)
      axis(side = 2, at = seq(0, peak, by = peak/5), labels = NA, cex.axis = 1.2, lwd = 1.5)
      title(ylab = "Density", mgp = c(1, 1, 0), adj = 0.31)
      arrows(x0 = hdi[1], y0 = peak, x1 = hdi[2], y1 = peak, angle = 90, length = 0.05,
             code = 3, lwd = 2)

      t1 <- legend(x = .95, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
      text(t1$rect$left + t1$rect$w, t1$text$y*.99,
           c("", paste("median = ", round(med, 3), sep =""),
             paste("95% HDI: [", round(hdi[1], 3), ", ", round(hdi[2], 3),"]", sep ="")),
           cex = 1.2, pos = 2)

      legend(x = 0, y = peak, lty = c(1, 3), lwd = 2, c("Posterior", "Prior"), col = c("navy", "firebrick"), bty = "n", cex = 1.2)

      text("insufficient", x = cuts[1]/2, y = peak*-.03, adj = 0.5, cex = 1.2)
      text("sufficient", x = (cuts[1] + cuts[2])/2, y = peak*-.03, adj = .5, cex = 1.2)
      text("good", x = (cuts[2] + 1)/2, y = peak*-.03, adj = .5, cex = 1.2)
      legend(x = 0, y = peak*1.33,  fill=colos, horiz=F, cex=1.2, bty = "n",
             c("insufficient:", "sufficient:", "good:"))
      f.post <- plotrix::floating.pie(xpos = .42, ypos = peak*1.2, x = pie.post, radius = rad+.02,
                                      col = colos, startpos = 0)
      l2 <- legend(x = .275, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
      text(l2$rect$left + l2$rect$w, l2$text$y*.993, c(pie.post.labels), pos = 2, cex = 1.2)
    }

  } else {
    plot(density(samp, adjust = 1.75), type = "l", axes = F, xlab = "Reliability", ylab = NA,
         xlim = c(0, 1), ylim = c(0,  peak * 1.25),
         lwd = 3, main = "")
    plotShadePrior(dens.prior, xx = c(xx0, xx1, xx2, xx3), cols = colos, criteria = criteria, blackwhite = blackwhite)
    plotShadePost(dens.post, xx = c(x0, x1, x2, x3), cols = colos, criteria = criteria, blackwhite = blackwhite)

    lines(density(prior, from = 0, to = 1), lty = 3, lwd = 3)

    axis(side = 1, at = seq(0, 1, by = .2), labels = seq(0, 1, by = .2), cex.axis = 1.2, lwd = 1.5)
    axis(side = 2, at = seq(0, peak, by = peak/5), labels = NA, cex.axis = 1.2, lwd = 1.5)
    title(ylab = "Density", mgp = c(1, 1, 0), adj = 0.31)
    arrows(x0 = hdi[1], y0 = peak, x1 = hdi[2], y1 = peak, angle = 90, length = 0.05,
           code = 3, lwd = 2)

    t1 <- legend(x = .95, y = peak*1.33, legend=c("", "", ""), cex = 1.2, bty ="n", xjust = 0, yjust = 1)
    text(t1$rect$left + t1$rect$w, t1$text$y*.99,
         c("", paste("median = ", round(med, 3), sep =""),
           paste("95% HDI: [", round(hdi[1], 3), ", ", round(hdi[2], 3),"]", sep ="")),
         cex = 1.2, pos = 2)

    legend(x = 0, y = peak, lty = c(1, 3), lwd = 2, c("Posterior", "Prior"), bty = "n", cex = 1.2)

  }

  options(warn = 0)
}

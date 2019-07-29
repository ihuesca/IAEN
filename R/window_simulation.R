#Simulation window######################

#' Simulations for networks
#' @keywords internal

simulation_func <- function(h, ...) {

    # Percentage of herbivores

    Fraction_Herv <- function(dat, ...) {
        rownames(dat) = colnames(dat)
        Basal <- rownames(dat)[apply(dat, 2, sum) == 0]
        dif <- rownames(dat)[!rownames(dat) %in% Basal]
        dat2 <- dat[dif, dif]
        Nod2 <- colnames(dat2)[apply(dat2, 2, sum) == 0]
        if (length(Nod2) > 0) {
            dat3 <- matrix(dat[Basal, Nod2], nrow = length(Basal), ncol = length(Nod2))
            dimnames(dat3) <- list(Basal, Nod2)
            Hervivores <- rownames(dat3)[apply(dat3, 2, sum) > 0]
            n.Hervivores <- length(Hervivores)
            fraction <- (n.Hervivores)/dim(dat)[1]
        } else {
            fraction <- 0
        }
        fraction
    }

    # Percentage of omnivores

    Fraction_Omni <- function(mat, TL, ...) {
        if (is.numeric(TL)) {
            WebByTL <- aggregate(mat, by = list(as.character(TL)), FUN = sum)
            WebByTL <- WebByTL[, -1]
            WebByTL <- (WebByTL > 0) * 1
            different.TL <- apply(WebByTL, 2, sum)
            n.Omnivores <- length(different.TL[different.TL > 1])
            Fraction <- n.Omnivores/dim(mat)[1]
        } else {
            Fraction <- 0
        }
        Fraction
    }

    # Percentage of Species in Loops

    Fraction_Loops <- function(mat) {
        mat <- dat <- as.matrix(mat)
        loops <- colnames(mat)[diag(mat) == 1]
        for (i in 1:10) {
            mat <- mat %*% dat
            loops <- c(loops, colnames(mat)[diag(mat) > 1])
        }
        dup <- duplicated(loops)
        loops <- loops[dup == FALSE]
        loops <- list(Loops = loops, per = (length(loops)/dim(mat)[1]))
        loops
    }

    # Attributes

    attri_binary <- function(COM1, mat, gr, ...) {
        S <- NumberOfNodes(COM1) #Packages:cheddar
        L <- NumberOfTrophicLinks(COM1)
        C <- DirectedConnectance(COM1)
        Top <- FractionTopLevelNodes(COM1)
        Int <- FractionIntermediateNodes(COM1)
        Bas <- FractionBasalNodes(COM1)
        Herb <- Fraction_Herv(mat)
        Can <- FractionCannibalistic(COM1)
        Loop <- Fraction_Loops(mat)$per
        if (Bas > 0) {
            SW.TL <- ShortWeightedTrophicLevel(COM1, include.isolated = TRUE) #Packages:cheddar
            if (!any(is.na(SW.TL))) {
                Omn <- Fraction_Omni(mat, TL = SW.TL)
                TL <- mean(SW.TL)
            } else {
                Omn <- 0
                TL <- 0
            }
            chains.length <- TrophicChainsStats(COM1)$chain.lengths #Packages:cheddar
            ChLg <- mean(chains.length)
            ChSD <- sd(chains.length)
        } else {
            Omn <- 0
            TL <- 0
            ChLg <- 0
            ChSD <- 0
        }
        GenSD <- sd(TrophicGenerality(COM1))
        VulSD <- sd(TrophicVulnerability(COM1))
        MxSim <- MeanMaximumTrophicSimilarity(COM1)
        Path <- mean_distance(gr, directed = FALSE) #Packages:igraph
        Clust <- transitivity(gr)
        att.table <- data.frame(c(S, L, C, Top, Int, Bas, Herb, Can, Loop,
            Omn, GenSD, VulSD, MxSim, TL, ChLg, ChSD, Path, Clust))
        rownames(att.table) <- c("S", "L", "C", "Top", "Int", "Bas", "Herb",
            "Can", "Loop", "Omn", "GenSD", "VulSD", "MxSim", "TL", "ChLg",
            "ChSD", "Path", "Clust")
        colnames(att.table) <- c("Value")
        att.table
    }

    # Centrality

    Cent_webs <- function(gr, ...) {

        # Betweenness Centrality

        cent_Bet <- centr_betw(gr, normalized = FALSE)$res #Packages:igraph
        cent_BetNorm <- igraph::betweenness(gr, normalized = TRUE)
        cent_Bet1 <- centr_betw(gr, normalized = FALSE)
        cent_Bet2 <- centr_betw(gr, normalized = TRUE)

        centBet <- c(cent_Bet, Centralization = cent_Bet1$centralization,
            Theoretical.max = cent_Bet1$theoretical_max)
        centBetNorm <- c(cent_BetNorm, Centralization = cent_Bet2$centralization,
            Theoretical.max = cent_Bet2$theoretical_max)

        Betweenness_cent <- data.frame(centBet, centBetNorm)
        colnames(Betweenness_cent) <- c("Betw", "BetwNor")

        # Closeness Centrality

        Clo1_tot <- igraph::closeness(gr, mode = "total") #Packages:igraph
        Clo1_tot.n <- igraph::closeness(gr, mode = "total", normalized = TRUE)

        Clo2_tot <- centr_clo(gr, mode = "all", normalized = FALSE)
        Clo2_tot.n <- centr_clo(gr, mode = "all", normalized = TRUE)

        centClo <- data.frame(Clo1_tot, Clo1_tot.n)
        gcent <- c(Clo2_tot$centralization, Clo2_tot.n$centralization)
        tmax <- c(Clo2_tot$theoretical_max, Clo2_tot.n$theoretical_max)

        Closeness_cent <- round(data.frame(rbind(centClo, Centralization = gcent,
            Theoretical.max = tmax)), digits = 6)
        names(Closeness_cent) <- c("TotClos", "TotClosNor")

        # Degree Centrality

        Deg1_tot <- igraph::degree(gr, mode = "total") #Packages:igraph
        Deg1_tot.n <- igraph::degree(gr, mode = "total", normalized = TRUE)

        Deg2_tot <- centr_degree(gr, mode = "all", normalized = FALSE)
        Deg2_tot.n <- centr_degree(gr, mode = "all", normalized = TRUE)

        centDeg <- data.frame(cbind(Deg1_tot, Deg1_tot.n))
        gcent <- c(Deg2_tot$centralization, Deg2_tot.n$centralization)
        tmax <- c(Deg2_tot$theoretical_max, Deg2_tot.n$theoretical_max)

        Degree_cen <- data.frame(rbind(centDeg, Centralization = gcent,
            Theoretical.max = tmax))
        colnames(Degree_cen) <- c("TotDeg", "TotDegNorm")

        Cent <- cbind(Betweenness_cent, Closeness_cent, Degree_cen)
        Cent

    }

    # Niche Model

    Web.NicheModel <- function(S, C, tol, N, ...) {

        # Connectance interval
        C.min <- C - tol
        C.max <- C + tol

        WebNicheModel <- list()

        for (k in 1:N) {
            iter <- 0

            while (iter < 1000) {
                iter <- iter + 1

                Web.Niche <- matrix(0, S, S, dimnames = list(paste("S",
                  1:S, sep = ""), paste("S", 1:S, sep = "")))
                n <- sort(runif(S))
                b <- 1/(2 * C) - 1
                x <- 1 - (1 - runif(S))^(1/b)
                r <- n * x
                r[1] <- 0
                center <- numeric()
                for (i in 1:S) {
                  center[i] <- runif(1, r[i]/2, min(n[i], 1 - r[i]/2))
                }
                getMin <- center - r/2
                getMax <- center + r/2

                # Adjacency matrix
                for (i in 1:S) {
                  Web.Niche[c(1:S)[n > getMin[i] & n < getMax[i]], i] <- 1
                }

                # Links
                L <- sum(Web.Niche)

                # Connectance
                C.value = L/S^2

                # Connected
                connected.nodes <- sna::is.connected(Web.Niche, connected = "weak")

                # Loops
                net <- network(Web.Niche)
                loop.value <- has.loops(net)

                if (C.value > C.min & C.value < C.max & loop.value == FALSE &
                  connected.nodes == TRUE) {
                  iter = 1000
                }
            }
            WebNicheModel[[k]] <- Web.Niche
        }
        WebNicheModel
    }

    # Cascade Model

    Web.CascadeModel <- function(S, C, tol, N, ...) {

        # Connectance interval
        C.min <- C - tol
        C.max <- C + tol

        WebCascadeModel <- list()

        for (k in 1:N) {
            iter <- 0

            while (iter < 1000) {
                iter <- iter + 1

                Web.Cascade <- matrix(0, S, S, dimnames = list(paste("S",
                  1:S, sep = ""), paste("S", 1:S, sep = "")))
                P <- 2 * C * S/(S - 1)
                r.value <- runif((S^2 - S)/2)

                # Adjacency matrix
                Web.Cascade[upper.tri(Web.Cascade)][r.value < P] = 1

                # Links
                L <- sum(Web.Cascade)

                # Connectance
                C.value = L/S^2

                # Connected
                connected.nodes <- sna::is.connected(Web.Cascade, connected = "weak") #Packages:sna

                # Loops
                net <- network(Web.Cascade) #Packages:network
                loop.value <- has.loops(net)

                if (C.value > C.min & C.value < C.max & loop.value == FALSE &
                  connected.nodes == TRUE) {
                  iter = 1000
                }
            }
            WebCascadeModel[[k]] <- Web.Cascade
        }
        WebCascadeModel
    }

    # Random Model

    Web.RandomModel <- function(S, C, tol, N, ...) {

        # Connectance interval
        C.min <- C - tol
        C.max <- C + tol

        WebRandomModel <- list()

        for (k in 1:N) {
            iter <- 0

            while (iter < 1000) {
                iter <- iter + 1

                # Adjacency matrix
                Web.Random <- matrix(0, S, S, dimnames = list(paste("S",
                  1:S, sep = ""), paste("S", 1:S, sep = "")))
                r.value <- runif(S^2)
                Web.Random[r.value < C] = 1

                # Links
                L <- sum(Web.Random)

                # Connectance
                C.value = L/S^2

                # Connected
                connected.nodes <- sna::is.connected(Web.Random, connected = "weak") #Packages:sna

                # Loops
                net <- network(Web.Random) #Packages:network
                loop.value <- has.loops(net)

                if (C.value > C.min & C.value < C.max & loop.value == FALSE &
                  connected.nodes == TRUE) {
                  iter = 1000
                }
            }
            WebRandomModel[[k]] <- Web.Random
        }
        WebRandomModel
    }

    SmallWorld.random <- function(n, m, rep) {
        if (m > ((n * n) - n)) {
            m = ((n * n) - n)
        }
        gr.random <- list()
        x = 1
        while (x <= rep) {
            gr.rand <- sample_gnm(n, m, directed = TRUE) #Packages:igraph
            mat <- as.matrix(as_adjacency_matrix(gr.rand))
            iso <- isolates(mat, diag = FALSE)
            if (length(iso) == 0) {
                gr.random[[x]] <- gr.rand
                x <- x + 1
            }
        }
        L.rand <- unlist(lapply(gr.random, mean_distance))
        C.rand <- unlist(lapply(gr.random, transitivity))
        L.random <- round(c(L.rand, mean(L.rand)), 4)
        C.random <- round(c(C.rand, mean(C.rand)), 4)
        Repetition <- c(seq(1, rep), "Average")
        Parameters <- data.frame(Repetition, L.random, C.random)
        colnames(Parameters) <- c("Repetition", "Path(L random)", "Clust(C random)")
        Parameters
    }

    #################################################

    simulation_window <- gtkWindow(show = FALSE)
    simulation_window$setTransientFor(Window)
    simulation_window$setPosition("center-on-parent")
    simulation_window["title"] <- "Simulation"
    simulation_window$setSizeRequest(300, 390)
    simulation_window["border-width"] <- 2
    simulation_window$setModal(TRUE)
    simulation_window$setResizable(FALSE)

    vbox_sim <- gtkVBox(FALSE, 5)
    simulation_window$add(vbox_sim)

    select_label <- gtkLabel("Select the options to perform:")
    select_label["xalign"] <- 0
    select_label$modifyFg(GtkStateType["normal"], "blue")
    vbox_sim$packStart(select_label, expand = FALSE, fill = FALSE)

    #################################################

    rad_sim_cfw <- gtkRadioButton(label = "Create Food Web")
    rad_sim_sw <- gtkRadioButtonNewWithLabelFromWidget(group = rad_sim_cfw,
        label = "Small World")
    vbox_sim$packStart(rad_sim_cfw, expand = FALSE, fill = FALSE)

    tab1 <- gtkTable(rows = 2, columns = 2, homogeneous = TRUE)
    vbox_sim$packStart(tab1, expand = TRUE, fill = TRUE)
    gtkTableSetRowSpacings(tab1, 2)
    gtkTableSetColSpacings(tab1, 2)

    #################################################

    mod_frame <- gtkFrame("Model")
    mod_vbox <- gtkVBox(FALSE, 5)
    rad_random <- gtkRadioButton(label = "Random")
    rad_cascade <- gtkRadioButtonNewWithLabelFromWidget(group = rad_random,
        label = "Cascade")
    rad_niche <- gtkRadioButtonNewWithLabelFromWidget(group = rad_cascade,
        label = "Niche")
    mod_vbox$add(rad_random)
    mod_vbox$add(rad_cascade)
    mod_vbox$add(rad_niche)
    mod_frame$add(mod_vbox)

    #################################################

    par_frame <- gtkFrame("Parameters")
    par_vbox <- gtkVBox(FALSE, 5)
    par_frame$add(par_vbox)
    spe_label <- gtkLabel("Species:")
    spe_ent <- gtkEntry()
    spe_ent$setSizeRequest(40, 20)
    spe_ent$insertText(20)
    gSignalConnect(spe_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })
    spe_hbox <- gtkHBox(FALSE, 39)
    spe_hbox$packStart(spe_label, FALSE, FALSE)
    spe_hbox$packStart(spe_ent, FALSE, FALSE)

    con_label <- gtkLabel("Connectance:")
    con_adjustment <- gtkAdjustment(lower = 0, upper = 1, step.incr = 0.01)
    con_spinbutton <- gtkSpinButton(adjustment = con_adjustment, digits = 2)
    con_spinbutton["value"] <- 0.2
    con_spinbutton$setSizeRequest(40, 22)

    con_hbox <- gtkHBox(FALSE, 8)
    con_hbox$packStart(con_label, FALSE, FALSE)
    con_hbox$packStart(con_spinbutton, FALSE, FALSE)

    tol_label <- gtkLabel("Tolerance (%):")
    tol_adjustment <- gtkAdjustment(lower = 1, upper = 100, step.incr = 1)
    tol_spinbutton <- gtkSpinButton(adjustment = tol_adjustment)
    tol_spinbutton["value"] <- 3
    tol_spinbutton$setSizeRequest(40, 22)

    tol_hbox <- gtkHBox(FALSE, 5)
    tol_hbox$packStart(tol_label, FALSE, FALSE)
    tol_hbox$packStart(tol_spinbutton, FALSE, FALSE)

    par_vbox$add(spe_hbox)
    par_vbox$add(con_hbox)
    par_vbox$add(tol_hbox)

    #################################################

    cal_frame <- gtkFrame("Calculate")

    cal_cb <- list()
    cal_cb$Att <- gtkCheckButton(label = "Attributes")
    cal_cb$Cent <- gtkCheckButton(label = "Centralities")

    iter_label <- gtkLabel("Number of iterations:")
    iter_ent <- gtkEntry()
    iter_ent$setSizeRequest(35, 20)
    iter_ent$insertText(1)
    gSignalConnect(iter_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })
    iter_hbox <- gtkHBox(FALSE, 7)
    iter_hbox$packStart(iter_label, FALSE, FALSE)
    iter_hbox$packStart(iter_ent, FALSE, FALSE)

    cal_vbox <- gtkVBox(FALSE, 5)
    cal_frame$add(cal_vbox)
    sapply(cal_cb, cal_vbox$packStart)
    cal_vbox$add(iter_hbox)

    #################################################
    vbox_sim$packStart(gtkHSeparatorNew(), FALSE, FALSE)
    vbox_sim$packStart(rad_sim_sw, expand = FALSE, fill = FALSE)

    #################################################

    par_sw_frame <- gtkFrame("Parameters")
    vbox_sim$packStart(par_sw_frame, TRUE, TRUE)
    sw_vbox <- gtkVBox(FALSE, 5)
    par_sw_frame$add(sw_vbox)

    #################################################

    ln_sw_label <- gtkLabel("Number of nodes:")
    ln_sw_ent <- gtkEntry()
    ln_sw_ent$setSizeRequest(40, 20)
    ln_sw_ent$insertText(20)
    gSignalConnect(ln_sw_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })
    ln_sw_hbox <- gtkHBox(FALSE, 9)
    ln_sw_hbox$packStart(ln_sw_label, FALSE, FALSE)
    ln_sw_hbox$packStart(ln_sw_ent, FALSE, FALSE)

    nl_sw_label <- gtkLabel("Number of links:")
    nl_sw_ent <- gtkEntry()
    nl_sw_ent$setSizeRequest(40, 20)
    nl_sw_ent$insertText(20)
    gSignalConnect(nl_sw_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })
    nl_sw_hbox <- gtkHBox(FALSE, 17)
    nl_sw_hbox$packStart(nl_sw_label, FALSE, FALSE)
    nl_sw_hbox$packStart(nl_sw_ent, FALSE, FALSE)

    rep_sw_label <- gtkLabel("Repetitions:")
    rep_sw_ent <- gtkEntry()
    rep_sw_ent$setSizeRequest(40, 20)
    rep_sw_ent$insertText(100)
    gSignalConnect(rep_sw_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })
    rep_sw_hbox <- gtkHBox(FALSE, 43)
    rep_sw_hbox$packStart(rep_sw_label, FALSE, FALSE)
    rep_sw_hbox$packStart(rep_sw_ent, FALSE, FALSE)

    sw_vbox$add(ln_sw_hbox)
    sw_vbox$add(nl_sw_hbox)
    sw_vbox$add(rep_sw_hbox)

    #################################################

    StatFWok <- function(h, ...) {

        ##### SPINNER START#####

        SpinStart()
        e$run.spin <- c(e$run.spin, TRUE)

        if (rad_sim_cfw["active"]) {

            ##### MODEL#####

            if (rad_random["active"]) {
                Model <- "Random"
            } else if (rad_cascade["active"]) {
                Model <- "Cascade"
            } else if (rad_niche["active"]) {
                Model <- "Niche"
            }

            ##### PARAMETERS#####

            n_spe_ent <- nzchar(spe_ent$getText())
            if (n_spe_ent == FALSE) {
                n_Species <- 0
            } else {
                n_Species <- as.numeric(spe_ent$getText())
            }

            value_Conectance <- con_spinbutton["value"]
            value_Tolerance <- tol_spinbutton["value"]

            n_iter_ent <- nzchar(iter_ent$getText())
            if (n_iter_ent == FALSE) {
                n_Iter <- 0
            } else {
                n_Iter <- as.numeric(iter_ent$getText())
            }

            #################################################

            if (Model == "Random") {
                FoodWeb <- Web.RandomModel(S = n_Species, C = value_Conectance,
                  tol = value_Tolerance, N = n_Iter)
            } else if (Model == "Cascade") {
                FoodWeb <- Web.CascadeModel(S = n_Species, C = value_Conectance,
                  tol = value_Tolerance, N = n_Iter)
            } else if (Model == "Niche") {
                FoodWeb <- Web.NicheModel(S = n_Species, C = value_Conectance,
                  tol = value_Tolerance, N = n_Iter)
            }

            #################################################

            ##### RESULT GNOTEBOOK#####

            e$cont = e$cont + 1
            name <- paste(e$cont, ".", Model, " Model", sep = "")

            .GlobalEnv$gn1 <- gtkNotebook()
            gn1["scrollable"] <- TRUE
            result.note$insertPageWithCloseButton(gn1, label.text = name)
            t.store$set(t.store$append()$iter, 0, name)
            model <- tree_view$getModel()
            nrows <- model$iterNChildren()
            selection <- tree_view$getSelection()
            miter <- t.store$getIterFromString(as.character(nrows - 1))$iter
            gtkTreeSelectionSelectIter(selection, miter)
            Notebook["page"] <- 2
            simulation_window["visible"] <- FALSE

            ##### COMMUNITY#####

            NODE <- colnames(FoodWeb[[1]])
            COM1 <- Community(nodes = data.frame(node = NODE), trophic.links = PredationMatrixToLinks(FoodWeb[[1]]),
                properties = list(title = "Test community")) #Packages:cheddar

            ##### NETWORK#####

            net <- network(FoodWeb[[1]]) #Packages:network

            ##### IGRAPH#####

            gr <- graph_from_adjacency_matrix(FoodWeb[[1]]) #Packages:igraph

            #################################################

            Model.FoodWeb <- FoodWeb[[1]]
            Model.FoodWeb <- data.frame(cbind(Nodes = rownames(Model.FoodWeb),
                Model.FoodWeb), row.names = NULL)

            rgDAT <- rGtkDataFrame(Model.FoodWeb)
            view <- gtkTreeView(rgDAT)

            crt1 <- gtkCellRendererText()
            crt1["background"] <- "lightblue3"
            crt1["font"] <- "sans bold 8"
            crt1["style"] <- "normal"
            view$insertColumnWithAttributes(position = -1, title = colnames(rgDAT)[1],
                cell = crt1, text = 0)

            crt2 <- gtkCellRendererText()
            crt2["foreground"] <- "gray17"
            crt2["font"] <- "sans 8"
            crt2["background"] <- "gray96"

            mapply(view$insertColumnWithAttributes, -1, colnames(rgDAT)[-1],
                list(crt2), text = seq_len(ncol(rgDAT) - 1))
            gsw <- gtkScrolledWindow()
            gsw$add(view)
            gn1$appendPage(gsw, gtkLabel("Food Web"))

            e$Result$A$FoodWeb <- Model.FoodWeb

            #################################################

            if (cal_cb$Att["active"] | cal_cb$Cent["active"]) {

                result1 <- list()
                if (cal_cb$Att["active"]) {
                  graphs <- lapply(FoodWeb, function(x) graph_from_adjacency_matrix(x))
                  communities <- lapply(FoodWeb, function(x) {
                    NODE <- colnames(x)
                    COM1 <- Community(nodes = data.frame(node = NODE),
                      trophic.links = PredationMatrixToLinks(x), properties = list(title = "Test community"))
                  })
                  attributes1 <- list()
                  for (i in 1:length(FoodWeb)) {
                    attributes1[[i]] <- attri_binary(COM1 = communities[[i]],
                      mat = FoodWeb[[i]], gr = graphs[[i]])
                  }
                  Attri.mean <- Reduce("+", lapply(attributes1, function(x) replace(x,
                    is.na(x), 0)))/length(attributes1)
                  Attri.mean <- data.frame(rownames(Attri.mean), Attri.mean,
                    row.names = NULL)
                  colnames(Attri.mean) <- c("Attribite", "Value")
                  e$Result$A$Attributes <- Attri.mean
                  result1$Attributes <- format(Attri.mean, digits = 3,
                    nsmall = 4)
                }

                if (cal_cb$Cent["active"]) {
                  graphs <- lapply(FoodWeb, function(x) graph_from_adjacency_matrix(x))
                  centr <- lapply(graphs, Cent_webs)
                  sum_centr <- Reduce("+", centr)
                  average_centr <- sum_centr/length(FoodWeb)
                  average_centr <- data.frame(cbind(Nodes = rownames(average_centr),
                    average_centr), row.names = NULL)
                  average_centr1 <- format(average_centr, digits = 3, nsmall = 4)
                  result1$Average.Centrality <- average_centr1
                  e$Result$A$Average.Centrality <- average_centr
                }

                if (n_Iter > 1) {
                  Frequency <- Reduce("+", FoodWeb)
                  Frequency <- data.frame(cbind(Nodes = rownames(Frequency),
                    Frequency), row.names = NULL)
                  result1$Frequency <- Frequency
                  e$Result$A$Frecuency <- Frequency
                }

                for (i in 1:length(result1)) {

                  rgDAT <- rGtkDataFrame(result1[[i]])
                  view <- gtkTreeView(rgDAT)

                  crt1 <- gtkCellRendererText()
                  crt1["background"] <- "lightblue3"
                  crt1["font"] <- "sans bold 8"
                  crt1["style"] <- "normal"
                  view$insertColumnWithAttributes(position = -1, title = colnames(rgDAT)[1],
                    cell = crt1, text = 0)

                  crt2 <- gtkCellRendererText()
                  crt2["foreground"] <- "gray17"
                  crt2["font"] <- "sans 8"
                  crt2["background"] <- "gray96"

                  mapply(view$insertColumnWithAttributes, -1, colnames(rgDAT)[-1],
                    list(crt2), text = seq_len(ncol(rgDAT) - 1))
                  gsw <- gtkScrolledWindow()
                  gsw$add(view)

                  gn1$appendPage(gsw, gtkLabel(names(result1)[i]))

                }
            }
        }

        if (rad_sim_sw["active"]) {

            ##### PARAMETERS#####

            n_nod_ent <- nzchar(ln_sw_ent$getText())
            if (n_nod_ent == FALSE) {
                n_Nodes <- 0
            } else {
                n_Nodes <- as.numeric(ln_sw_ent$getText())
            }

            n_links_ent <- nzchar(nl_sw_ent$getText())
            if (n_links_ent == FALSE) {
                n_Links <- 0
            } else {
                n_Links <- as.numeric(nl_sw_ent$getText())
            }

            n_rep_ent <- nzchar(rep_sw_ent$getText())
            if (n_rep_ent == FALSE) {
                n_Rep <- 0
            } else {
                n_Rep <- as.numeric(rep_sw_ent$getText())
            }

            #################################################

            ##### RESULT GNOTEBOOK#####

            e$cont = e$cont + 1
            name <- paste(e$cont, ".", " Small World", sep = "")

            .GlobalEnv$gn1 <- gtkNotebook()
            gn1["scrollable"] <- TRUE
            result.note$insertPageWithCloseButton(gn1, label.text = name)
            t.store$set(t.store$append()$iter, 0, name)
            model <- tree_view$getModel()
            nrows <- model$iterNChildren()
            selection <- tree_view$getSelection()
            miter <- t.store$getIterFromString(as.character(nrows - 1))$iter
            gtkTreeSelectionSelectIter(selection, miter)
            Notebook["page"] <- 2
            simulation_window["visible"] <- FALSE

            #################################################

            SmallWorld.par <- SmallWorld.random(n = n_Nodes, m = n_Links,
                rep = n_Rep)

            rgDAT <- rGtkDataFrame(SmallWorld.par)
            view <- gtkTreeView(rgDAT)

            crt1 <- gtkCellRendererText()
            crt1["background"] <- "lightblue3"
            crt1["font"] <- "sans bold 8"
            crt1["style"] <- "normal"
            view$insertColumnWithAttributes(position = -1, title = colnames(rgDAT)[1],
                cell = crt1, text = 0)

            crt2 <- gtkCellRendererText()
            crt2["foreground"] <- "gray17"
            crt2["font"] <- "sans 8"
            crt2["background"] <- "gray96"

            mapply(view$insertColumnWithAttributes, -1, colnames(rgDAT)[-1],
                list(crt2), text = seq_len(ncol(rgDAT) - 1))
            gsw <- gtkScrolledWindow()
            gsw$add(view)
            gn1$appendPage(gsw, gtkLabel("Small World"))

            e$Result$A$SmallWorld <- SmallWorld.par

        }

        #################################################

        simulation_window$destroy()
        if (any(names(e$Result) == name)) {
            e$Result[name] = NULL
        }
        names(e$Result)[length(e$Result)] <- name
        SpinStop()

    }

    #################################################

    buttons_hbox <- gtkHBox(FALSE, 5)

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = StatFWok)
    ok_button$setSizeRequest(70, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) simulation_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    buttons_hbox$packEnd(cancel_button, FALSE, FALSE)
    buttons_hbox$packEnd(ok_button, FALSE, FALSE)

    tab1$attach(mod_frame, left.attach = 0, 1, top.attach = 0, 1)
    tab1$attach(par_frame, left.attach = 1, 2, top.attach = 0, 1)
    tab1$attach(cal_frame, left.attach = 0, 2, top.attach = 1, 2)
    vbox_sim$packEnd(buttons_hbox, FALSE, FALSE)

    #################################################

    simulation_window$setModal(TRUE)
    simulation_window$showAll()
    simulation_window$setResizable(FALSE)

}

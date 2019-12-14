#################################################

#' Bipartite Network
#' @keywords internal

statB_func <- function(h, ...) {

    ##### Network Attributes#####

    net.lev <- function(dat, weight, ...) {
        index <- if (weight == TRUE) {
            "ALLBUTDD"
        } else {
            "binary"
        }
        dist <- if (weight == TRUE) {
            "horn"
        } else {
            "jaccard"
        }
        res <- networklevel(dat, index = index, dist = dist) #Package:bipartite
        res <- data.frame(Attribute = names(res), Value = format(res, digits = 2,
            nsmall = 2), row.names = NULL)
        return(res)
    }

    ##### Attributes by level#####

    g.lev <- function(dat, weight) {
        index <- if (weight == TRUE) {
            "ALLBUTDD"
        } else {
            "binary"
        }
        dist <- if (weight == TRUE) {
            "horn"
        } else {
            "jaccard"
        }
        res <- grouplevel(dat) #Package:bipartite
        res <- data.frame(Attribute = names(res), Value = format(res, digits = 3,
            nsmall = 4, scientific = FALSE), row.names = NULL)
        return(res)
    }

    ##### Network properties at the species level####

    sp.lev <- function(dat, weight, ...) {
        index <- if (weight == TRUE) {
            "ALLBUTDD"
        } else {
            "binary"
        }
        nested.method <- if (weight == TRUE) {
            "NODF"
        } else {
            "binmatnest"
        }
        res <- specieslevel(dat, nested.method = nested.method) #Package:bipartite
        res1 <- data.frame(Nodes = rownames(res$"lower level"), format(res$"lower level",
            digits = 3, nsmall = 4), row.names = NULL)
        res2 <- data.frame(Nodes = rownames(res$"higher level"), format(res$"higher level",
            digits = 3, nsmall = 4), row.names = NULL)
        return(list(Properties.lower = res1, Properties.higher = res2))
    }

    ##### Nestedness#####

    nest <- function(dat, weight, ...) {
        method <- if (weight == TRUE) {
            "NODF"
        } else {
            "binmatnest"
        }
        res <- nestedrank(dat, method = method) #Package:bipartite
        res1 <- data.frame(Nodes = names(res$"lower level"), Value = format(res$"lower level",
            digits = 3, nsmall = 4), row.names = NULL)
        res2 <- data.frame(Nodes = names(res$"higher level"), Value = format(res$"higher level",
            digits = 3, nsmall = 4), row.names = NULL)
        return(list(Nestedness.lower = res1, Nestedness.higher = res2))
    }

    ##### Contribution per-species to nestedness#####

    nest.cont <- function(dat, ...) {
        res <- nestedcontribution(dat) #Package:bipartite
        res1 <- data.frame(Nodes = row.names(res$"lower level"), Value = format(res$"lower level"$nestedcontribution,
            digits = 3, nsmall = 4), row.names = NULL)
        res2 <- data.frame(Nodes = row.names(res$"higher level"), Value = format(res$"higher level"$nestedcontribution,
            digits = 3, nsmall = 4), row.names = NULL)
        return(list(Contribution.lower = res1, Contribution.higher = res2))
    }

    ##### Specialization#####

    spe.lev <- function(dat, ...) {
        res <- H2fun(dat, H2_integer = FALSE) #Package:bipartite
        res <- data.frame(Attribute = names(res), Value = format(res, digits = 3,
            nsmall = 4), row.names = NULL)
        return(res)
    }

    ##### Specialization by level#####

    spe.by.lev <- function(dat, ...) {
        res <- dfun(dat)
        res2 <- dfun(t(dat)) #Package:bipartite
        res <- data.frame(Nodes = rownames(dat), format(cbind(dprime = res$dprime,
            d = res$d, dmin = res$dmin, dmax = res$dmax), digits = 3, nsmall = 4),
            row.names = NULL)
        res2 <- data.frame(Nodes = colnames(dat), format(cbind(dprime = res2$dprime,
            d = res2$d, dmin = res2$dmin, dmax = res2$dmax), digits = 3,
            nsmall = 4), row.names = NULL)
        return(list(Specialization.lower = res, Specialization.higher = res2))
    }

    ##### Node overlap and separation#####

    nos <- function(dat, ...) {
        nos <- NOS(dat, keep.Nij = FALSE, keep.diag = FALSE) #Package:bipartite
        v1 <- nos$Nbar
        v2 <- nos$mod
        v3 <- nos$Nbar_higher
        v4 <- nos$Nbar_lower
        v5 <- nos$mod_lower
        v6 <- nos$mod_higher
        Measure <- c("Nbar", "mod", "Nbar_higher", "Nbar_lower", "mod_lower",
            "mod_higher")
        res <- data.frame(Measure, Value = format(c(v1, v2, v3, v4, v5,
            v6), digits = 3, nsmall = 4))
        res
    }

    ##### Modularity#####

    Modularity.Bip <- function(Bip, method, ...) {
        col.zero <- c(1:ncol(Bip))[apply(Bip, 2, sum) == 0]
        row.zero <- c(1:nrow(Bip))[apply(Bip, 1, sum) == 0]
        if (length(col.zero) > 0) {
            Bip <- Bip[, -c(col.zero)]
        }
        if (length(row.zero) > 0) {
            Bip <- Bip[-c(row.zero), ]
        }
        res <- computeModules(Bip, method = method) #Package:bipartite

        Modules <- res@modules
        Modules <- Modules[-1, -c(1, 2)]
        OrderA <- rownames(Bip)[res@orderA]
        Modules.T <- apply(Modules, 2, function(x) {
            c(1:length(x))[x > 0]
        })
        Modules.Row <- Modules.T[1:nrow(Bip)][res@orderA]
        Modules.Col <- Modules.T[nrow(Bip) + 1:ncol(Bip)][res@orderB]
        Modules.Row <- data.frame(rownames(Bip)[res@orderA], Modules.Row)
        Modules.Col <- data.frame(colnames(Bip)[res@orderB], Modules.Col)
        colnames(Modules.Row) <- c("Nodes", "Module")
        colnames(Modules.Col) <- c("Nodes", "Module")
        like <- data.frame(c("Likelihood"), c(res@likelihood))
        colnames(like) <- list("Statistic", "Value")
        OrderA <- res@orderA
        OrderB <- res@orderB

        return(list(Modules.higher = Modules.Row, Modules.lower = Modules.Col,
            Likelihood = like, OrderA = OrderA, OrderB = OrderB))
    }

    ###################################################################

    StatBok <- function(h, ...) {

        ##### ITEM#####

        iter <- gtkComboBoxGetActiveIter(combobox)$iter
        item <- model$GetValue(iter, 0)$value

        ##### DATA#####

        dat <- as.matrix(e$datB[[item]])

        ##### Weighted/Binary#####

        if (any(dat != 1 & dat != 0) == TRUE) {
            weight <- TRUE
        } else {
            weight <- FALSE
        }

        ##### RESULT GNOTEBOOK#####

        e$cont <- e$cont + 1
        name <- paste(e$cont, ".", item, sep = "")

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
        statB_window["visible"] <- FALSE

        ##### SPINNER START#####

        SpinStart()
        e$run.spin <- c(e$run.spin, TRUE)

        ##### ATTRIBUTES#####

        if (any(sapply(gen_cb, "[", "active") == TRUE)) {

            ##### Network Attributes#####

            if (gen_cb$Net["active"]) {
                Attributes <- net.lev(dat, weight)
                rgDAT <- rGtkDataFrame(Attributes)
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

                gn1$appendPage(gsw, gtkLabel("Attributes"))
                At <- transform(Attributes, Value = as.character(Value))
                At <- transform(At, Value = as.numeric(Value))
                e$Result$A$Attributes <- At
            }

            ##### Attributes by level#####

            if (gen_cb$Att["active"]) {
                Attributes.level <- g.lev(dat, weight)
                rgDAT <- rGtkDataFrame(Attributes.level)
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
                gn1$appendPage(gsw, gtkLabel("Attributes.level"))
                At2 <- transform(Attributes.level, Value = as.character(Value))
                At2 <- transform(At2, Value = as.numeric(Value))
                e$Result$A$Attributes.level <- At2

            }
        }

        ##### TOPOLOGY#####

        if (any(sapply(top_cb, "[", "active") == TRUE)) {

            ##### Network properties at the species level#####

            if (top_cb$Pro["active"]) {
                Properties <- sp.lev(dat, weight)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(Properties[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste(names(Properties)[[i]])))
                }
                Properties[[1]][2:ncol(Properties[[1]])] <- apply(data.frame(Properties[[1]][2:ncol(Properties[[1]])]),
                  2, as.numeric)
                Properties[[2]][2:ncol(Properties[[2]])] <- apply(data.frame(Properties[[2]][2:ncol(Properties[[2]])]),
                  2, as.numeric)
                e$Result$A$Properties.lower <- Properties[[1]]
                e$Result$A$Properties.higher <- Properties[[2]]
            }

            ##### Nestedness#####

            if (top_cb$Nes["active"]) {
                Nest <- nest(dat, weight)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(Nest[[i]])
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
                }
                Nest1 <- transform(Nest[[1]], Value = as.character(Value))
                Nest1 <- transform(Nest1, Value = as.numeric(Value))
                Nest2 <- transform(Nest[[2]], Value = as.character(Value))
                Nest2 <- transform(Nest2, Value = as.numeric(Value))
                e$Result$A$Nestedness.lower <- Nest1
                e$Result$A$Nestedness.higher <- Nest2
            }

            ##### Contribution per-species to nestedness#####

            if (top_cb$Con["active"]) {
                Nest.Cont <- nest.cont(dat)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(Nest.Cont[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste(names(Nest.Cont)[[i]])))
                }
                N.Cont1 <- transform(Nest.Cont[[1]], Value = as.character(Value))
                N.Cont1 <- transform(N.Cont1, Value = as.numeric(Value))
                N.Cont2 <- transform(Nest.Cont[[2]], Value = as.character(Value))
                N.Cont2 <- transform(N.Cont2, Value = as.numeric(Value))
                e$Result$A$Contribution.lower <- N.Cont1
                e$Result$A$Contribution.higher <- N.Cont2
            }

            ##### Specialization#####

            if (top_cb$Spe["active"]) {
                Specialization <- spe.lev(dat)

                rgDAT <- rGtkDataFrame(Specialization)
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

                gn1$appendPage(gsw, gtkLabel("Specialization"))
                Spec <- transform(Specialization, Value = as.character(Value))
                Spec <- transform(Spec, Value = as.numeric(Value))
                e$Result$A$Specialization <- Spec
            }

            ##### Specialization by level#####

            if (top_cb$SpeL["active"]) {
                Spe.by.lev <- spe.by.lev(dat)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(Spe.by.lev[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste(names(Spe.by.lev)[[i]])))
                }
                Spe.by.lev[[1]][2:ncol(Spe.by.lev[[1]])] <- apply(data.frame(Spe.by.lev[[1]][2:ncol(Spe.by.lev[[1]])]),
                  2, as.numeric)
                Spe.by.lev[[2]][2:ncol(Spe.by.lev[[2]])] <- apply(data.frame(Spe.by.lev[[2]][2:ncol(Spe.by.lev[[2]])]),
                  2, as.numeric)
                e$Result$A$Specialization.lower <- Spe.by.lev[[1]]
                e$Result$A$Specialization.higher <- Spe.by.lev[[2]]
            }

            ##### Node overlap and separation#####

            if (top_cb$Ove["active"]) {
                nos.res <- nos(dat)

                rgDAT <- rGtkDataFrame(nos.res)
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

                gn1$appendPage(gsw, gtkLabel("Overlap and separation"))
                nos2 <- transform(nos.res, Value = as.character(Value))
                nos2 <- transform(nos2, Value = as.numeric(Value))
                e$Result$A$Overlap <- nos2
            }

        }

        ##### Modularity#####

        if (mod_cb["active"]) {
            iter2 <- gtkComboBoxGetActiveIter(combobox_m)$iter
            item2 <- model_m$GetValue(iter2, 0)$value
            Mod.Bip <- Modularity.Bip(dat, method = as.character(item2))

            for (i in 1:3) {
                rgDAT <- rGtkDataFrame(Mod.Bip[[i]])
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

                gn1$appendPage(gsw, gtkLabel(paste(names(Mod.Bip)[[i]])))
            }
            e$Result$A$Modules.higher <- Mod.Bip[[1]]
            e$Result$A$Modules.lower <- Mod.Bip[[2]]
            e$Result$A$Likelihood <- Mod.Bip[[3]]

            e$Modules.all$A$Modules.higher <- Mod.Bip[[1]]
            e$Modules.all$A$Modules.lower <- Mod.Bip[[2]]
            e$Modules.all$A$Data <- item
            e$Modules.all$A$OrderA <- Mod.Bip[[4]]
            e$Modules.all$A$OrderB <- Mod.Bip[[5]]
            Graph10["sensitive"] <- TRUE
            SpinStop()
        }

        statB_window$destroy()
        if (any(names(e$Result) == name)) {
            e$Result[name] <- NULL
            e$Modules.all[name] <- NULL
        }
        names(e$Result)[length(e$Result)] <- name
        names(e$Modules.all)[length(e$Modules.all)] <- name
        SpinStop()

    }

    ###################################################################

    clickB_func <- function(h, ...) {
        if (any(sapply(gen_cb, "[", "active") == TRUE) | any(sapply(top_cb,
            "[", "active") == TRUE) | mod_cb["active"] == TRUE) {
            ok_button["sensitive"] <- TRUE
        } else {
            ok_button["sensitive"] <- FALSE
        }
    }

    ###################################################################

    statB_window <- gtkWindow(show = FALSE)
    statB_window$setTransientFor(Window)
    statB_window$setPosition("center-on-parent")
    statB_window["title"] <- "Statistics - Bipartite matrix"
    statB_window$setSizeRequest(300, 320)
    statB_window["border-width"] <- 2
    statB_window$setModal(TRUE)
    statB_window$setResizable(FALSE)

    vbox1 <- gtkVBox(FALSE, 5)
    statB_window$add(vbox1)

    hbox1 <- gtkHBox(FALSE, 0)
    name_label <- gtkLabel("Name:")
    model <- rGtkDataFrame(names(e$datB))
    combobox <- gtkComboBox(model)
    combobox$setSizeRequest(114, 20)

    crt <- gtkCellRendererText()
    combobox$packStart(crt)
    combobox$addAttribute(crt, "text", 0)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(combobox, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

    select_label <- gtkLabel("Select the options to perform:")
    select_label["xalign"] <- 0
    select_label$modifyFg(GtkStateType["normal"], "blue")
    vbox1$packStart(select_label, expand = FALSE, fill = FALSE)

    gen_frame <- gtkFrame("General")
    vbox1$packStart(gen_frame, expand = TRUE, fill = TRUE)
    gen_cb <- list()
    gen_cb$Net <- gtkCheckButton(label = "Network Attributes")
    gen_cb$Att <- gtkCheckButton(label = "Attributes by level")

    vbox2 <- gtkVBox()
    gen_frame$add(vbox2)
    sapply(gen_cb, vbox2$packStart)

    top_frame <- gtkFrame("Topology")
    vbox1$packStart(top_frame, expand = TRUE, fill = TRUE)

    top_cb <- list()
    top_cb$Pro <- gtkCheckButton(label = "Network properties at the species level")
    top_cb$Nes <- gtkCheckButton(label = "Nestedness")
    top_cb$Con <- gtkCheckButton(label = "Contribution per-species to nestedness")
    top_cb$Spe <- gtkCheckButton(label = "Specialization")
    top_cb$SpeL <- gtkCheckButton(label = "Specialization by level")
    top_cb$Ove <- gtkCheckButton(label = "Node overlap and separation")

    vbox3 <- gtkVBox()
    top_frame$add(vbox3)
    sapply(top_cb, vbox3$packStart)

    mod_cb <- gtkCheckButton(label = "Modularity:")

    model_m <- rGtkDataFrame(c("Beckett", "DormannStrauss"))
    combobox_m <- gtkComboBox(model_m)
    combobox_m$setSizeRequest(114, 20)
    crt_m <- gtkCellRendererText()
    combobox_m$packStart(crt_m)
    combobox_m$addAttribute(crt_m, "text", 0)

    hbox2 <- gtkHBox(FALSE, 2)
    hbox2$packStart(mod_cb, expand = FALSE, fill = FALSE)
    hbox2$packStart(combobox_m, expand = FALSE, fill = FALSE)
    vbox3$packStart(hbox2, expand = FALSE, fill = FALSE)

    sapply(gen_cb, gSignalConnect, "clicked", f = clickB_func)
    sapply(top_cb, gSignalConnect, "clicked", f = clickB_func)
    sapply(list(mod_cb), gSignalConnect, "clicked", f = clickB_func)

    sel_button <- gtkButton("Select All")
    sel_button$setSizeRequest(70, 25)
    gSignalConnect(sel_button, "clicked", f = function(h, ...) {
        lapply(gen_cb, function(h, ...) h[["active"]] <- TRUE)
        lapply(top_cb, function(h, ...) h[["active"]] <- TRUE)
        mod_cb["active"] <- TRUE
        ok_button["sensitive"] <- TRUE
    })
    des_button <- gtkButton("Deselect All")
    des_button$setSizeRequest(70, 25)
    gSignalConnect(des_button, "clicked", f = function(h, ...) {
        lapply(gen_cb, function(h, ...) h[["active"]] <- FALSE)
        lapply(top_cb, function(h, ...) h[["active"]] <- FALSE)
        mod_cb["active"] <- FALSE
        ok_button["sensitive"] <- FALSE
    })
    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = StatBok)
    ok_button$setSizeRequest(70, 25)
    ok_button["sensitive"] <- FALSE
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) statB_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    hbox3 <- gtkHBox(FALSE, 1)
    hbox3$packStart(sel_button, expand = FALSE, fill = FALSE)
    hbox3$packStart(des_button, expand = FALSE, fill = FALSE)
    hbox3$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox3$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox3, expand = FALSE, fill = FALSE)


    #####################################################

    ok_button["sensitive"] <- FALSE
    statB_window$setModal(TRUE)
    gtkComboBoxSetActive(combobox, 0)
    gtkComboBoxSetActive(combobox_m, 0)
    statB_window$showAll()
    statB_window$setResizable(FALSE)

    #####################################################
}

#################################################

#' Unweighted Network
#' @keywords internal

statU_func <- function(h, ...) {

    ##### Atributtes#####

    attri <- function(net, COM1, gr, mat, ...) {
        S <- network.size(net) #Package:network
        L <- NumberOfTrophicLinks(COM1) #Package:cheddar
        LS <- LinkageDensity(COM1)
        C <- DirectedConnectance(COM1)
        Top <- FractionTopLevelNodes(COM1)
        Int <- FractionIntermediateNodes(COM1)
        Bas <- FractionBasalNodes(COM1)
        Prey <- Bas+Int
        Predators <- Int+Top
        Herb <- Fraction_Herv(mat)
        Can <- FractionCannibalistic(COM1)
        Loop <- Fraction_Loops(mat)$per
        SW.TL <- ShortWeightedTrophicLevel(COM1, include.isolated = TRUE) #Package:cheddar
        Omn.Frac <- Fraction_Omni(mat, TL = SW.TL)
        GenSD <- sd(TrophicGenerality(COM1))
        VulSD <- sd(TrophicVulnerability(COM1))
        MxSim <- MeanMaximumTrophicSimilarity(COM1)
        TL <- mean(SW.TL)
        chains.length <- TrophicChainsStats(COM1)$chain.lengths #Package:cheddar
        ChLg <- mean(chains.length)
        ChSD <- sd(chains.length)
        Path <- mean_distance(gr, directed = TRUE) #Package:igraph
        Clust <- transitivity(gr, type = "global")
        A1 <- is.directed(net)  #Package:network
        A2 <- is.hyper(net)
        A3 <- has.loops(net)
        A4 <- is.multiplex(net)
        A5 <- is.bipartite(net)
        att.n <- c("S", "L", "L/S", "C", "Top", "Int", "Bas", "Prey", "Predators", "Herb", "Can",
            "Loop", "Omn.Frac", "GenSD", "VulSD", "MxSim", "TL", "ChLg", "ChSD",
            "Path", "Clust", "Directed", "Hyper", "Loops", "Multiple",
            "Bipartite")
        att.1 <- c(S, L, LS, C, Top, Int, Bas, Prey, Predators, Herb, Can, Loop, Omn.Frac, GenSD,
            VulSD, MxSim, TL, ChLg, ChSD, Path, Clust)
        att.1 <- unlist(lapply(att.1, function(x) format(x, digits = 4,
            nsmall = 4)))
        att <- data.frame(cbind(att.n, c(att.1, A1, A2, A3, A4, A5)))
        colnames(att) <- c("Attribute", "Value")
        return(att)
    }
                               
    # Percentage of herbivores

    Fraction_Herv <- function(dat, ...) {
        rownames(dat) <- colnames(dat)
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

    ##### Centrality#####

    # Population variance
    var.p <- function(x) {
        if (is.numeric(x) & length(x) > 1) {
            n <- length(x)
            var <- (var(x) * (n - 1))/n
            return(sqrt(var))
        } else {
            return(NA)
        }
    }

    # Bonacich Power Centrality
    cent1 <- function(dat, exp, ...) {
        if (class(try(bonpow(dat, exponent = exp, tol = 1e-20), silent = TRUE)) ==
            "numeric") { #Package:sna
            cent <- bonpow(dat, exponent = exp, tol = 1e-20)
            cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        } else {
            cent <- rep(NA, nrow(dat) + 4)
        }
        return(cent)
    }

    # Betweenness Centrality
    cent2 <- function(gr, ...) {
        cen1 <- centr_betw(gr, normalized = FALSE)$res #Package:igraph
        cen2 <- igraph::betweenness(gr, normalized = TRUE)
        g1 <- centr_betw(gr, normalized = FALSE)
        g2 <- centr_betw(gr, normalized = TRUE)
        ce1 <- c(cen1, Mean = mean(cen1), Var = var.p(cen1), Centralization = g1$centralization,
            Theoretical.max = g1$theoretical_max)
        ce2 <- c(cen2, Mean = mean(cen2), Var = var.p(cen2), Centralization = g2$centralization,
            Theoretical.max = g2$theoretical_max)
        res <- data.frame(ce1, ce2)
        colnames(res) <- c("Betw", "BetwNor")
        return(res)
    }

    # Closeness Centrality
    cent3 <- function(gr, ...) {
        c1 <- igraph::closeness(gr, mode = "in") #Package:igraph
        c2 <- igraph::closeness(gr, mode = "out")
        c3 <- igraph::closeness(gr, mode = "total")
        c1.n <- igraph::closeness(gr, mode = "in", normalized = TRUE)
        c2.n <- igraph::closeness(gr, mode = "out", normalized = TRUE)
        c3.n <- igraph::closeness(gr, mode = "total", normalized = TRUE)
        cent <- round(data.frame(c1, c1.n, c2, c2.n, c3, c3.n), digits = 5)

        g1 <- centr_clo(gr, mode = "in", normalized = FALSE) #Package:igraph
        g1.n <- centr_clo(gr, mode = "in", normalized = TRUE)
        g2 <- centr_clo(gr, mode = "out", normalized = FALSE)
        g2.n <- centr_clo(gr, mode = "out", normalized = TRUE)
        g3 <- centr_clo(gr, mode = "all", normalized = FALSE)
        g3.n <- centr_clo(gr, mode = "all", normalized = TRUE)

        cent <- data.frame(c1, c1.n, c2, c2.n, c3, c3.n)
        mean.cent <- unlist(lapply(cent, mean))
        var.cent <- unlist(lapply(cent, var.p))
        gcent <- c(g1$centralization, g1.n$centralization, g2$centralization,
            g2.n$centralization, g3$centralization, g3.n$centralization)
        tmax <- c(g1$theoretical_max, g1.n$theoretical_max, g2$theoretical_max,
            g2.n$theoretical_max, g3$theoretical_max, g3.n$theoretical_max)

        res <- round(data.frame(rbind(cent, Mean = mean.cent, Var = var.cent,
            Centralization = gcent, Theoretical.max = tmax)), digits = 6)

        names(res) <- c("InClos", "InClosNor", "OutClos", "OutClosNor",
            "TotClos", "TotClosNor")
        return(res)
    }

    # Eigenvector Centrality
    cent4 <- function(dat, ...) {
        cent <- evcent(dat) #Package:sna
        cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        return(cent)
    }

    # (Harary) Graph Centrality
    cent5 <- function(dat, ...) {
        cent <- graphcent(dat) #Package:sna
        cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        return(cent)
    }

    # Information Centrality
    cent6 <- function(dat, ...) {
        cent <- infocent(dat) #Package:sna
        cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        return(cent)
    }

    # Load Centrality
    cent7 <- function(dat, ...) {
        cent <- loadcent(dat) #Package:sna
        cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        return(cent)
    }

    # Stress Centrality
    cent8 <- function(dat, ...) {
        cent <- stresscent(dat) #Package:sna
        cent <- c(cent, Mean = mean(cent), Var = var.p(cent), NA, NA)
        return(cent)
    }

    # Degree Centrality
    cent9 <- function(gr, ...) {
        d1 <- igraph::degree(gr, mode = "in") #Package:igraph
        d2 <- igraph::degree(gr, mode = "out")
        d3 <- igraph::degree(gr, mode = "total")
        d1.n <- igraph::degree(gr, mode = "in", normalized = TRUE)
        d2.n <- igraph::degree(gr, mode = "out", normalized = TRUE)
        d3.n <- igraph::degree(gr, mode = "total", normalized = TRUE)

        g1 <- centr_degree(gr, mode = "in", normalized = FALSE)
        g1.n <- centr_degree(gr, mode = "in", normalized = TRUE)
        g2 <- centr_degree(gr, mode = "out", normalized = FALSE)
        g2.n <- centr_degree(gr, mode = "out", normalized = TRUE)
        g3 <- centr_degree(gr, mode = "all", normalized = FALSE)
        g3.n <- centr_degree(gr, mode = "all", normalized = TRUE)

        cent <- data.frame(cbind(d1, d1.n, d2, d2.n, d3, d3.n))
        mean.cent <- unlist(lapply(cent, mean))
        var.cent <- unlist(lapply(cent, var.p))
        gcent <- c(g1$centralization, g1.n$centralization, g2$centralization,
            g2.n$centralization, g3$centralization, g3.n$centralization)
        tmax <- c(g1$theoretical_max, g1.n$theoretical_max, g2$theoretical_max,
            g2.n$theoretical_max, g3$theoretical_max, g3.n$theoretical_max)

        res <- data.frame(rbind(cent, Mean = mean.cent, Var = var.cent,
            Centralization = gcent, Theoretical.max = tmax))
        colnames(res) <- c("InDeg", "InDegNorm", "OutDeg", "OutDegNorm",
            "TotDeg", "TotDegNorm")
        return(res)
    }
                               
    # Bridging_centrality                             
    cent10 <- function(gr, ...) {
        Bet <- igraph::betweenness(gr, normalized = TRUE) #igraph
        Deg <- igraph::degree(gr) #igraph
        Bridge.c <- c()
        for (i in 1 : length(Deg)){
            Node <- names(Deg)[i]
            Nei <- igraph::neighbors(gr, Node) #igraph
            Bridge.c[i] <- Bet[i]*((1/Deg[i])/sum(1/Deg[Nei]))
        }
        names(Bridge.c) <- names(Deg)
        Bridge.c <- c(Bridge.c, Mean = mean(Bridge.c), Var = var(Bridge.c), Centralization = NA, Theoretical.max = NA)
        Bridging_cent <- data.frame(Bridging = Bridge.c)
        return (Bridging_cent)
    }
                               
    # Reachability
    reach <- function(dat, ...) {
        res <- data.frame(reachability(dat)) #Package:sna
        return(res)
    }

    ##### Ecological Indices#####

    # Trophic Level
    TL <- function(COM1, ...) {
        level <- numeric()
        TL1 <- TrophicLevels(COM1, include.isolated = TRUE) #Package:cheddar
        NODES <- rownames(TL1)
        TL2 <- IsBasalNode(COM1)
        TL3 <- IsTopLevelNode(COM1)
        TL4 <- IsIntermediateNode(COM1)
        if (any(TL2) == TRUE) {
            level <- format(data.frame(NODES, TL1, TL2, TL3, TL4), digits = 3,
                nsmall = 4)
            colnames(level) <- c("Nodes", colnames(TL1), "IsBasal", "IsTopLevel",
                "IsIntermediate")
        } else {
            level <- format(data.frame(NODES, TL2, TL3, TL4), digits = 3,
                nsmall = 4)
            colnames(level) <- c("Nodes", "IsBasal", "IsTopLevel", "IsIntermediate")
        }
        return(level)
    }

    ### Omnivory
    OmnIndex <- function(gr, COM1, ...) {

        BasalN <- IsBasalNode(COM1)
        nodB <- names(BasalN[BasalN == TRUE])
        Nodes <- names(BasalN)

        ### NUMBER OF LINKS DOWN TO THE BASAL SPECIES

        all.paths <- function(x, gr, y, ...) {
            all_simple_paths(gr, from = x, to = y, mode = "in") #Package:igraph
        }
        paths.length <- function(y, ...) {
            result <- unlist(lapply(y, length)) - 1
            return(result)
        }

        ### POPULATION STANDARD DESVIATION
        sd.p <- function(x) {
            z <- c(x)
            if (is.numeric(z) & length(z) > 1) {
                n <- length(z)
                sp <- (var(z) * (n - 1))/n
                return(sqrt(sp))
            } else {
                return(0)
            }
        }

        # Functions

        allpaths <- function(x, gr, y, ...) {
            all_simple_paths(gr, from = x, to = y, mode = "in") #Package:igraph
        }

        length.paths <- function(y, ...) {
            result <- unlist(lapply(y, length)) - 1
            return(result)
        }

        # SD AND FREQUENCIES
        basal.paths <- sapply(Nodes, allpaths, gr = gr, y = nodB)
        all.length <- lapply(basal.paths, FUN = length.paths)

        max.path <- max(unlist(all.length))
        freq <- matrix(0, ncol = max.path, nrow = length(Nodes))
        for (m in 1:length(Nodes)) {
            for (n in 1:max.path) {
                freq[m, n] <- length(all.length[[m]][all.length[[m]] ==
                  n & all.length[[m]] != 0])
            }
        }

        sd.p.all <- unlist(lapply(all.length, sd.p))

        freq.paths <- data.frame(Nodes, format(sd.p.all, digits = 3, nsmall = 4),
            freq, row.names = NULL)
        colnames(freq.paths) <- c("Nodes", "SD", paste("L", 1:max.path,
            sep = ""))
        omn.I <- format(mean(sd.p.all), digits = 3, nsmall = 4)

        ### IS OMNIVORY
        OM1 <- IsOmnivore(COM1, level = ShortestTrophicLevel) #Package:cheddar
        OM2 <- IsOmnivore(COM1, level = ShortWeightedTrophicLevel)
        OM3 <- IsOmnivore(COM1, level = LongestTrophicLevel)
        OM4 <- IsOmnivore(COM1, level = LongWeightedTrophicLevel)
        OM5 <- IsOmnivore(COM1, level = ChainAveragedTrophicLevel)
        OM6 <- IsOmnivore(COM1, level = PreyAveragedTrophicLevel)
        Is.Omn <- data.frame(Nodes, OM1, OM2, OM3, OM4, OM5, OM6, row.names = NULL)
        colnames(Is.Omn) <- c("Nodes", "ShortestTL", "ShortWeightedTL",
            "LongestTL", "LongWeightedTL", "ChainAveragedTL", "PreyAveragedTL")

        ### OMNIVORY INDEX
        Fr1 <- FractionOmnivorous(COM1, level = ShortestTrophicLevel) #Package:cheddar
        Fr2 <- FractionOmnivorous(COM1, level = ShortWeightedTrophicLevel)
        Fr3 <- FractionOmnivorous(COM1, level = LongestTrophicLevel)
        Fr4 <- FractionOmnivorous(COM1, level = LongWeightedTrophicLevel)
        Fr5 <- FractionOmnivorous(COM1, level = ChainAveragedTrophicLevel)
        Fr6 <- FractionOmnivorous(COM1, level = PreyAveragedTrophicLevel)
        Om1 <- c("Shortest", "Short Weighted", "Longest", "Long Weighted",
            "Chain Averaged", "Prey Averaged", "NA", "Omnivory Index")
        Om2 <- c(format(c(Fr1, Fr2, Fr3, Fr4, Fr5, Fr6), digits = 3, nsmall = 4),
            NA, omn.I)
        Omn <- data.frame(Om1, Om2, row.names = NULL)
        colnames(Omn) <- c("Method", "Prop")


        return(list(Frequencies = freq.paths, Is.Omnivory = Is.Omn, Omnivory = Omn))
    }

    #################################################

    StatUok <- function(h, ...) {

        ##### ITEM#####

        iter <- gtkComboBoxGetActiveIter(combobox)$iter
        item <- model$GetValue(iter, 0)$value

        ##### DATA#####

        dat <- as.matrix(e$datU[[item]])

        #### COMMUNITY#####

        NODE <- colnames(dat)
        COM1 <- e$COM.all[[item]]

        ##### NETWORK#####

        net <- e$mod.all[[item]]

        ##### IGRAPH#####

        gr <- graph_from_adjacency_matrix(as.matrix(dat)) #Package:igraph

        ##### RESULT GNOTEBOOK#####

        e$cont <- e$cont + 1
        name <- paste(e$cont, ".", item, sep = "")

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
        statU_window["visible"] <- FALSE

        ##### SPINNER START#####

        SpinStart()
        e$run.spin <- c(e$run.spin, TRUE)

        ##### ATTRIBUTES#####

        if (gen_cb["active"]) {
            Attribute <- attri(net, COM1, gr, mat = dat)
            rgDAT <- rGtkDataFrame(Attribute)
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

            gn1$appendPage(gsw, gtkLabel("Attributes"))
            e$Result$A$Attributes <- Attribute

        }

        ##### TOPOLOGY#####

        if (any(sapply(top_cb, "[", "active") == TRUE) | bon_cb["active"]) {

            ##### Centrality#####

            Centrality <- data.frame(Nodes = c(NODE, "Mean", "Var", "Centralization",
                "Theoretical.max"))
            #####
            if (bon_cb["active"]) {
                Bonacich <- cent1(dat, exp = as.numeric(bon_ent$getText()))
                Centrality <- data.frame(Centrality, Bonacich)
            }
            #####
            if (top_cb$Bet["active"]) {
                Betweenness <- cent2(gr)
                Centrality <- data.frame(Centrality, Betweenness)
            }
            if (top_cb$Clos["active"]) {
                Closenees <- cent3(gr)
                Centrality <- data.frame(Centrality, Closenees)
            }
            if (top_cb$Eig["active"]) {
                Eigen <- cent4(dat)
                Centrality <- data.frame(Centrality, Eigen)
            }
            if (top_cb$Har["active"]) {
                Harary <- cent5(dat)
                Centrality <- data.frame(Centrality, Harary)
            }
            if (top_cb$Info["active"]) {
                Information <- cent6(dat)
                Centrality <- data.frame(Centrality, Information)
            }
            if (top_cb$Loa["active"]) {
                Load <- cent7(dat)
                Centrality <- data.frame(Centrality, Load)
            }
            if (top_cb$Str["active"]) {
                Stress <- cent8(dat)
                Centrality <- data.frame(Centrality, Stress)
            }
            if (top_cb$Deg["active"]) {
                Degree <- cent9(gr)
                Centrality <- data.frame(Centrality, Degree)
            }
            if (top_cb$Brid["active"]) {
                Bridging <- cent10(gr)
                Centrality <- data.frame(Centrality, Bridging)
            }

            Centrality[, 2:ncol(Centrality)] <- format(Centrality[, 2:ncol(Centrality)],
                digits = 3, nsmall = 4)
            rgDAT <- rGtkDataFrame(Centrality)
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

            gn1$appendPage(gsw, gtkLabel("Centrality"))

            Centrality[, 2:ncol(Centrality)] <- apply(data.frame(Centrality[,
                2:ncol(Centrality)]), 2, as.numeric)
            e$Result$A$Centrality <- Centrality
        }

        ##### Reachability#####

        if (rea_cb["active"]) {
            Reachability <- reach(dat)
            colnames(Reachability) <- NODE
            Reachability <- data.frame(Nodes = NODE, Reachability)

            rgDAT <- rGtkDataFrame(Reachability)
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
            gn1$appendPage(gsw, gtkLabel("Reachability"))

            e$Result$A$Reachability <- Reachability
        }

        ##### ECOLOGICAL INDICES#####

        if (eco_cb["active"]) {

            TrophicL <- TL(COM1)

            rgDAT <- rGtkDataFrame(TrophicL)
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

            gn1$appendPage(gsw, gtkLabel("Trophic Level"))

            if (any(IsBasalNode(COM1)) == TRUE) {
                TrophicL[, 2:7] <- apply(TrophicL[, 2:7], 2, as.numeric)
                O <- OmnIndex(gr, COM1)

                for (i in 1:length(O)) {
                  rgDAT <- rGtkDataFrame(O[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste(names(O)[i])))

                }
            }
            e$Result$A$TrophicL <- TrophicL
            if (length(O) == 3) {
                Fre <- O$Frequencies
                Fre[, 2:ncol(Fre)] <- apply(data.frame(Fre[, 2:ncol(Fre)]),
                  2, as.numeric)
                e$Result$A$Frequencies <- Fre
            }
            e$Result$A$Is.Omnivory <- O$Is.Omnivory
            e$Result$A$Omnivory <- O$Omnivory
        }

        statU_window$destroy()
        if (any(names(e$Result) == name)) {
            e$Result[name] <- NULL
        }
        names(e$Result)[length(e$Result)] <- name
        SpinStop()
    }

    ###################################################################

    clickU_func <- function(h, ...) {
        if (any(sapply(list(gen_cb, bon_cb, rea_cb, eco_cb), "[", "active") ==
            TRUE) | any(sapply(top_cb, "[", "active") == TRUE)) {
            ok_button["sensitive"] <- TRUE
        } else {
            ok_button["sensitive"] <- FALSE
        }
    }

    ###################################################################

    statU_window <- gtkWindow(show = FALSE)
    statU_window$setTransientFor(Window)
    statU_window$setPosition("center-on-parent")
    statU_window["title"] <- "Statistics - Unweighted matrix"
    statU_window$setSizeRequest(300, 415)
    statU_window["border-width"] <- 2
    statU_window$setModal(TRUE)
    statU_window$setResizable(FALSE)

    vbox1 <- gtkVBox(FALSE, 5)
    statU_window$add(vbox1)

    hbox1 <- gtkHBox(FALSE, 0)
    name_label <- gtkLabel("Name:")
    model <- rGtkDataFrame(names(e$datU))
    combobox <- gtkComboBox(model)
    combobox$setSizeRequest(114, 20)
    crt <- gtkCellRendererText()
    combobox$packStart(crt)
    combobox$addAttribute(crt, "text", 0)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(combobox, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

    select_label <- gtkLabel("Select the options to perform:")
    select_label["xalign"] <- 0
    select_label$modifyFg(GtkStateType["normal"], "blue")
    vbox1$packStart(select_label, expand = FALSE, fill = FALSE)

    gen_frame <- gtkFrame("General")
    vbox1$packStart(gen_frame, expand = TRUE, fill = TRUE)
    gen_cb <- gtkCheckButton(label = "Attributes")
    gen_frame$add(gen_cb)

    top_frame <- gtkFrame("Topology")
    vbox1$packStart(top_frame, expand = TRUE, fill = TRUE)
    bon_cb <- gtkCheckButton(label = "Bonacich Power Centrality")
    bol_label <- gtkLabel("   Exponent:")
    bon_ent <- gtkEntry()
    bon_ent$setSizeRequest(30, 20)
    bon_ent$insertText(1)
    gSignalConnect(bon_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox2 <- gtkHBox(FALSE, 2)
    hbox2$packStart(bon_cb, expand = FALSE, fill = FALSE)
    hbox2$packStart(bol_label, expand = FALSE, fill = FALSE)
    hbox2$packStart(bon_ent, expand = FALSE, fill = FALSE)

    top_cb <- list()
    top_cb$Bet <- gtkCheckButton(label = "Betweenness Centrality")
    top_cb$Clos <- gtkCheckButton(label = "Closenees Centrality")
    top_cb$Eig <- gtkCheckButton(label = "Eigenvector Centrality")
    top_cb$Har <- gtkCheckButton(label = "Harary Graph Centrality")
    top_cb$Info <- gtkCheckButton(label = "Information Centrality")
    top_cb$Loa <- gtkCheckButton(label = "Load Centrality")
    top_cb$Str <- gtkCheckButton(label = "Stress Centrality")
    top_cb$Deg <- gtkCheckButton(label = "Degree Centrality")
    top_cb$Brid <- gtkCheckButton(label = "Bridging Centrality")

    rea_cb <- gtkCheckButton(label = "Reachability")
    hbox3 <- gtkHBox(FALSE, 2)
    hbox3$packStart(rea_cb, expand = FALSE, fill = FALSE)

    vbox2 <- gtkVBox()
    top_frame$add(vbox2)
    vbox2$packStart(hbox2)
    sapply(top_cb, vbox2$packStart)
    vbox2$packStart(hbox3)

    eco_frame <- gtkFrame("Ecological Indices")
    vbox1$packStart(eco_frame, expand = TRUE, fill = TRUE)
    eco_cb <- gtkCheckButton(label = "Trophic Level and Omnivory")
    eco_frame$add(eco_cb)

    sapply(list(gen_cb, bon_cb, rea_cb, eco_cb), gSignalConnect, "clicked",
        f = clickU_func)
    sapply(top_cb, gSignalConnect, "clicked", f = clickU_func)

    sel_button <- gtkButton("Select All")
    sel_button$setSizeRequest(70, 25)
    gSignalConnect(sel_button, "clicked", f = function(h, ...) {
        lapply(list(gen_cb, bon_cb, rea_cb, eco_cb), function(h, ...) h[["active"]] <- TRUE)
        lapply(top_cb, function(h, ...) h[["active"]] <- TRUE)
        ok_button["sensitive"] <- TRUE
    })
    des_button <- gtkButton("Deselect All")
    des_button$setSizeRequest(70, 25)
    gSignalConnect(des_button, "clicked", f = function(h, ...) {
        lapply(list(gen_cb, bon_cb, rea_cb, eco_cb), function(h, ...) h[["active"]] <- FALSE)
        lapply(top_cb, function(h, ...) h[["active"]] <- FALSE)
        ok_button["sensitive"] <- FALSE
    })
    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = StatUok)
    ok_button$setSizeRequest(70, 25)
    ok_button["sensitive"] <- FALSE
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) statU_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    hbox4 <- gtkHBox(FALSE, 1)
    hbox4$packStart(sel_button, expand = FALSE, fill = FALSE)
    hbox4$packStart(des_button, expand = FALSE, fill = FALSE)
    hbox4$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox4$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox4, expand = FALSE, fill = FALSE)

    #####################################################

    ok_button["sensitive"] <- FALSE
    statU_window$setModal(TRUE)
    gtkComboBoxSetActive(combobox, 0)
    statU_window$showAll()
    statU_window$setResizable(FALSE)

    #####################################################

}

#################################################

#' Weighted Network
#' @keywords internal

statW_func <- function(h, ...) {

    ##### Attributes#####

    attriW <- function(netW, ...) {
        A <- format(enaStructure(netW)$ns, digits = 3, nsmall = 4) #Package:enaR
        B <- is.directed(netW) #Packages:network
        C <- is.hyper(netW)
        D <- has.loops(netW)
        E <- is.multiplex(netW)
        F <- is.bipartite(netW)
        G <- ssCheck(netW)
        att.n <- c(colnames(A), "Directed", "Hyper", "Loops", "Multiple",
            "Bipartite", "Balanced")
        att <- data.frame(cbind(att.n, t(data.frame(A, B, C, D, E, F, G))))
        colnames(att) <- c("Attribute", "Value")
        rownames(att) <- NULL
        return(att)
    }

    ##### Flow Based Network Statistics#####

    Flow <- function(netW) {
        F <- enaFlow(netW)$ns #Package:enaR
        flow <- data.frame(Attribute = colnames(F), Value = t(format(F,
            nsmall = 3, digits = 4)), row.names = NULL)
        return(flow)
    }

    ##### Centrality#####

    ##### Environ Centrality#####
    EnvCentW <- function(netW, ...) {
        F <- enaFlow(netW) #Package:enaR
        Env <- environCentrality(F$N)
        cent <- data.frame(Env$ECin, Env$ECout, Env$AEC)
        return(cent)
    }

    ##### Eigen Centrality#####
    EiCentW <- function(netW, ...) {
        Eig <- eigenCentrality(x = as.matrix(netW, attrname = "flow")) #Package:enaR
        cent <- data.frame(Eig$EVCin, Eig$EVCout, Eig$AEVC)
        return(cent)
    }

    ##### Closeness centrality#####
    ClosCentW <- function(netW, ...) { #Package:tnet
        net.list <- as.tnet(as.matrix(netW, attrname = "flow"), type = "weighted one-mode tnet")
        cent <- tnet::closeness_w(net.list, directed = TRUE, gconly = FALSE)[,
            2:3]
        cent <- data.frame(cent)
        colnames(cent) <- c("Clos", "ClosNorm")
        return(cent)
    }

    ##### Betweenness centrality#####
    BetCentW <- function(netW, ...) {
        net.list <- as.tnet(as.matrix(netW, attrname = "flow"), type = "weighted one-mode tnet")
        cent <- tnet::betweenness_w(net.list, directed = TRUE)[, 2] #Package:tnet
        cent <- data.frame(cent)
        colnames(cent) <- "Betw"
        return(cent)
    }

    ##### Degree Centrality#####
    DegCentW <- function(netW, ...) {
        net.list <- as.tnet(as.matrix(netW, attrname = "flow"), type = "weighted one-mode tnet")
        cent <- degree_w(net.list)[, 2:3] #Package:tnet
        cent <- data.frame(cent)
        colnames(cent) <- c("Degree", "DegreeOut")
        return(cent)
    }

    ##### Ecological#####

    ##### Impact#####
    Imp <- function(netW, ...) {
        im <- enaMTI(netW) #Package:enaR
        Nodes <- colnames(im$G)
        G <- data.frame(Nodes, format(im$G, digits = 3, nsmall = 4), row.names = NULL)
        FP <- data.frame(Nodes, format(im$FP, digits = 3, nsmall = 4),
            row.names = NULL)
        Q <- data.frame(Nodes, format(im$Q, digits = 3, nsmall = 4), row.names = NULL)
        M <- data.frame(Nodes, format(im$M, digits = 3, nsmall = 4), row.names = NULL)
        return(list(G = G, FP = FP, Q = Q, M = M))
    }

    ##### Trophic Aggregations Analysis#####
    TrAgg <- function(netW, ...) {
        Nod <- colnames(as.matrix(netW, attrname = "flow"))
        Liv <- unpack(netW)$living
        NodesDet <- Nod[Liv == FALSE]
        Nodes <- c(Nod[Liv == TRUE], NodesDet)
        tro <- enaTroAgg(netW) #Package:enaR
        ETL <- c(tro$ETL[Liv == TRUE], tro$ETL[Liv == FALSE])
        if (any(names(tro) == "CI")) {
            CI <- c(tro$CI[Liv == TRUE], tro$CI[Liv == FALSE])
        } else {
            CI <- c(rep(0, length(Nodes)))
        }
        CE <- c(tro$CE[Liv == TRUE], tro$CE[Liv == FALSE])
        CR <- c(tro$CR[Liv == TRUE], tro$CR[Liv == FALSE])
        GC <- c(tro$GC[Liv == TRUE], rep(0, length(NodesDet)))
        Agg1 <- data.frame(Nodes, ETL, CI, CE, CR, GC, RDP = c(tro$RDP,
            rep(0, length(NodesDet))), LS = c(tro$LS, rep(0, length(NodesDet))),
            TE = c(tro$TE, rep(0, length(NodesDet))))
        Agg1[2:9] <- format(Agg1[2:9], digits = 3, nsmall = 4)
        ns <- tro$ns
        Agg2 <- data.frame(Attribute = colnames(ns), Value = t(format(ns,
            digits = 3, nsmall = 4)), row.names = NULL)
        return(list(Trophic.Level = Agg1, Trophic.Aggregations = Agg2))
    }

    ##### Ascendency of an Ecological Network#####
    Asc <- function(netW, ...) {
        Asc0 <- enaAscendency(netW) #Package:enaR
        CAP.asc <- c(Asc0[1, 20:23], Asc0[1, 4], 1, 100)
        A.asc <- c(Asc0[1, 12:15], Asc0[1, 5], Asc0[1, 7], Asc0[1, 7] *
            100)
        OH.asc <- c(Asc0[1, 16:19], Asc0[1, 6], Asc0[1, 8], Asc0[1, 8] *
            100)
        Attr <- c("Input", "Internal", "Export", "Respiration", "Total",
            "Ratio", "Percentage")
        Asc1 <- data.frame(CAP = CAP.asc, ASC = A.asc, OH = OH.asc, row.names = NULL)
        Asc1 <- data.frame(Attribute = Attr, format(round(Asc1, digits = 3),
            nsmall = 4))

        Info1 <- c(Asc0[1, 1:3])
        Info2 <- c("H = Diversity of flows", "AMI", "Hr = H-AMI")
        Info <- data.frame(Attribute = Info2, Value = format(Info1, digits = 3,
            nsmall = 4), row.names = NULL)

        return(list(Information = Info, Ascendency = Asc1))
    }

    ###################################################################

    StatWok <- function(h, ...) {

        ##### ITEM#####

        iter <- gtkComboBoxGetActiveIter(combobox)$iter
        item <- model$GetValue(iter, 0)$value

        ##### NETWORK WEIGHTED#####

        netW <- e$modW[[item]]

        ##### RESULT GNOTEBOOK#####

        e$cont <- e$cont + 1
        name <- paste(e$cont, ".", item, sep = "")

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
        statW_window["visible"] <- FALSE

        ##### SPINNER START#####

        SpinStart()
        e$run.spin <- c(e$run.spin)

        ##### ATTRIBUTES#####

        if (any(sapply(gen_cb, "[", "active") == TRUE)) {

            ##### Attribute#####

            if (gen_cb$Att["active"]) {
                Attribute <- attriW(netW)
                rgDAT <- rGtkDataFrame(Attribute)
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

                gn1$appendPage(gsw, gtkLabel("Attributes"))
                e$Result$A$Attributes <- Attribute
            }

            ##### Flow#####

            if (gen_cb$Flo["active"]) {
                FlowStat <- Flow(netW)
                rgDAT <- rGtkDataFrame(FlowStat)
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

                gn1$appendPage(gsw, gtkLabel("Flow Analysis"))

                FlowS <- transform(FlowStat, Value = as.character(Value))
                FlowS <- transform(FlowS, Value = as.numeric(Value))

                e$Result$A$Flow <- FlowS
            }
        }

        ##### TOPOLOGY#####

        if (any(sapply(top_cb, "[", "active") == TRUE)) {

            ##### Centrality#####

            Centrality <- data.frame(Nodes = colnames(as.matrix(e$modW[[item]])))

            #####
            if (top_cb$Bet["active"]) {
                Betweenness <- BetCentW(netW)
                Centrality <- data.frame(Centrality, Betweenness)
            }
            if (top_cb$Clo["active"]) {
                Closenees <- ClosCentW(netW)
                Centrality <- data.frame(Centrality, Closenees)
            }
            if (top_cb$Deg["active"]) {
                Degree <- DegCentW(netW)
                Centrality <- data.frame(Centrality, Degree)
            }
            if (top_cb$Eig["active"]) {
                Eigen <- EiCentW(netW)
                Centrality <- data.frame(Centrality, Eigen)
            }
            if (top_cb$Env["active"]) {
                Environ <- EnvCentW(netW)
                Centrality <- data.frame(Centrality, Environ)
            }

            #####
            Centrality[, 2:ncol(Centrality)] <- format(Centrality[, 2:ncol(Centrality)],
                digits = 3, nsmall = 4)

            rgDAT <- rGtkDataFrame(Centrality)
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

            gn1$appendPage(gsw, gtkLabel("Centrality"))

            Centrality[, 2:ncol(Centrality)] <- apply(data.frame(Centrality[,
                2:ncol(Centrality)]), 2, as.numeric)
            e$Result$A$Centrality <- Centrality
        }

        ##### ECOLOGICAL#####

        if (any(sapply(eco_cb, "[", "active") == TRUE)) {

            ##### Ascendency#####

            if (eco_cb$Asc["active"]) {
                As <- Asc(netW)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(As[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste(names(As)[i])))

                }

                As$Ascendency[, 2:ncol(As$Ascendency)] <- apply(As$Ascendency[,
                  2:ncol(As$Ascendency)], 2, as.numeric)
                Info <- transform(As$Information, Value = as.character(Value))
                Info <- transform(Info, Value = as.numeric(Value))

                e$Result$A$Ascendency <- As$Ascendency
                e$Result$A$Information <- Info

            }

            ##### TROPHIC LEVEL#####

            if (eco_cb$Tro["active"]) {
                TrophL <- TrAgg(netW)

                for (i in 1:2) {
                  rgDAT <- rGtkDataFrame(TrophL[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(gsub(".", " ", names(TrophL)[i],
                    fixed = TRUE)))

                }

                TrophL$Trophic.Level[, 2:ncol(TrophL$Trophic.Level)] <- apply(TrophL$Trophic.Level[,
                  2:ncol(TrophL$Trophic.Level)], 2, as.numeric)
                TrophL$Trophic.Aggregations <- transform(TrophL$Trophic.Aggregations,
                  Value = as.character(Value))
                TrophL$Trophic.Aggregations <- transform(TrophL$Trophic.Aggregations,
                  Value = as.numeric(Value))

                e$Result$A$Trophic.Level <- TrophL$Trophic.Level
                e$Result$A$Trophic.Aggregations <- TrophL$Trophic.Aggregations
            }


            ##### Impact#####

            if (eco_cb$Imp["active"]) {
                Impact <- Imp(netW)

                for (i in 1:4) {
                  rgDAT <- rGtkDataFrame(Impact[[i]])
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

                  gn1$appendPage(gsw, gtkLabel(paste("Impact", names(Impact)[i],
                    sep = "")))

                }

                Impact$G[, 2:ncol(Impact$G)] <- apply(Impact$G[, 2:ncol(Impact$G)],
                  2, as.numeric)
                Impact$FP[, 2:ncol(Impact$FP)] <- apply(Impact$FP[, 2:ncol(Impact$FP)],
                  2, as.numeric)
                Impact$Q[, 2:ncol(Impact$Q)] <- apply(Impact$Q[, 2:ncol(Impact$Q)],
                  2, as.numeric)
                Impact$M[, 2:ncol(Impact$M)] <- apply(Impact$M[, 2:ncol(Impact$M)],
                  2, as.numeric)

                e$Result$A$ImpactG <- Impact$G
                e$Result$A$ImpactFP <- Impact$FP
                e$Result$A$ImpactQ <- Impact$Q
                e$Result$A$ImpactM <- Impact$M

            }

        }

        statW_window$destroy()
        if (any(names(e$Result) == name)) {
            e$Result[name] <- NULL
        }
        names(e$Result)[length(e$Result)] <- name
        SpinStop()
    }

    ###################################################################

    clickW_func <- function(h, ...) {
        if (any(sapply(gen_cb, "[", "active") == TRUE) | any(sapply(top_cb,
            "[", "active") == TRUE) | any(sapply(eco_cb, "[", "active") ==
            TRUE)) {
            ok_button["sensitive"] <- TRUE
        } else {
            ok_button["sensitive"] <- FALSE
        }
    }

    ###################################################################

    statW_window <- gtkWindow(show = FALSE)
    statW_window$setTransientFor(Window)
    statW_window$setPosition("center-on-parent")
    statW_window["title"] <- "Statistics - Weighted matrix"
    statW_window$setSizeRequest(300, 400)
    statW_window["border-width"] <- 2
    statW_window$setModal(TRUE)
    statW_window$setResizable(FALSE)

    vbox1 <- gtkVBox(FALSE, 5)
    statW_window$add(vbox1)

    hbox1 <- gtkHBox(FALSE, 0)
    name_label <- gtkLabel("Name:")
    model <- rGtkDataFrame(names(e$datW))
    combobox <- gtkComboBox(model)
    combobox$setSizeRequest(114, 20)

    crt <- gtkCellRendererText()
    combobox$packStart(crt)
    combobox$addAttribute(crt, "text", 0)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(combobox, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

    select_label <- gtkLabel("Select the options to perform:")
    select_label["xalign"] <- 0
    select_label$modifyFg(GtkStateType["normal"], "blue")
    vbox1$packStart(select_label, expand = FALSE, fill = FALSE)

    gen_frame <- gtkFrame("General")
    vbox1$packStart(gen_frame, expand = TRUE, fill = TRUE)
    gen_cb <- list()
    gen_cb$Att <- gtkCheckButton(label = "Attributes")
    gen_cb$Flo <- gtkCheckButton(label = "Flow Analysis")

    vbox2 <- gtkVBox()
    gen_frame$add(vbox2)
    sapply(gen_cb, vbox2$packStart)

    top_frame <- gtkFrame("Topology")
    vbox1$packStart(top_frame, expand = TRUE, fill = TRUE)

    top_cb <- list()
    top_cb$Bet <- gtkCheckButton(label = "Betweenness Centrality")
    top_cb$Clos <- gtkCheckButton(label = "Closenees Centrality")
    top_cb$Deg <- gtkCheckButton(label = "Degree Centrality")
    top_cb$Eig <- gtkCheckButton(label = "Eigenvector Centrality")
    top_cb$Env <- gtkCheckButton(label = "Environ Centrality")

    vbox3 <- gtkVBox()
    top_frame$add(vbox3)
    sapply(top_cb, vbox3$packStart)

    eco_frame <- gtkFrame("Ecological Indices")
    vbox1$packStart(eco_frame, expand = TRUE, fill = TRUE)

    eco_cb <- list()
    eco_cb$Asc <- gtkCheckButton(label = "Ascendency")
    eco_cb$Tro <- gtkCheckButton(label = "Trophic Level")
    eco_cb$Omn <- gtkCheckButton(label = "Omnivory")
    eco_cb$Imp <- gtkCheckButton(label = "Impact")

    vbox4 <- gtkVBox()
    eco_frame$add(vbox4)
    sapply(eco_cb, vbox4$packStart)

    sapply(gen_cb, gSignalConnect, "clicked", f = clickW_func)
    sapply(top_cb, gSignalConnect, "clicked", f = clickW_func)
    sapply(eco_cb, gSignalConnect, "clicked", f = clickW_func)

    sel_button <- gtkButton("Select All")
    sel_button$setSizeRequest(70, 25)
    gSignalConnect(sel_button, "clicked", f = function(h, ...) {
        lapply(gen_cb, function(h, ...) h[["active"]] <- TRUE)
        lapply(top_cb, function(h, ...) h[["active"]] <- TRUE)
        lapply(eco_cb, function(h, ...) h[["active"]] <- TRUE)
        ok_button["sensitive"] <- TRUE
    })
    des_button <- gtkButton("Deselect All")
    des_button$setSizeRequest(70, 25)
    gSignalConnect(des_button, "clicked", f = function(h, ...) {
        lapply(gen_cb, function(h, ...) h[["active"]] <- FALSE)
        lapply(top_cb, function(h, ...) h[["active"]] <- FALSE)
        lapply(eco_cb, function(h, ...) h[["active"]] <- FALSE)
        ok_button["sensitive"] <- FALSE
    })
    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = StatWok)
    ok_button$setSizeRequest(70, 25)
    ok_button["sensitive"] <- FALSE
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) statW_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    hbox2 <- gtkHBox(FALSE, 1)
    hbox2$packStart(sel_button, expand = FALSE, fill = FALSE)
    hbox2$packStart(des_button, expand = FALSE, fill = FALSE)
    hbox2$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox2$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox2, expand = FALSE, fill = FALSE)

    #####################################################

    ok_button["sensitive"] <- FALSE
    statW_window$setModal(TRUE)
    gtkComboBoxSetActive(combobox, 0)
    statW_window$showAll()
    statW_window$setResizable(FALSE)

    #####################################################
}

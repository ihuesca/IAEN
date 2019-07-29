#######################################################

#' Import-Bipartite network
#' @keywords internal

wB <- function(h, ...) {
    #######################################################

    Imp_func <- function(h, ...) {
        dialog <- gtkFileChooserDialog(title = "Open a file", parent = imp_window,
            action = "open", "gtk-ok", GtkResponseType["ok"], "gtk-cancel",
            GtkResponseType["cancel"], show = FALSE)

        gSignalConnect(dialog, "response", f = function(dialog, response,
            data) {
            if (response == GtkResponseType["ok"]) {
                filename <- dialog$getFilename()
                e$BaseName <- basename(dialog$getFilename())
                e$FileName = filename
                e$FilterName = dialog$getFilter()$getName()
            } else {
                e$FileName = "NULL"
            }
            dialog$destroy()
        })

        fileFilter <- gtkFileFilter()
        fileFilter$setName(c(".csv"))
        fileFilter$addPattern("*.csv")
        dialog$addFilter(fileFilter)

        fileFilter2 <- gtkFileFilter()
        fileFilter2$setName(".xlsx")
        fileFilter2$addPattern("*.xlsx")
        dialog$addFilter(fileFilter2)

        fileFilter3 <- gtkFileFilter()
        fileFilter3$setName(".txt")
        fileFilter3$addPattern("*.txt")
        dialog$addFilter(fileFilter3)

        dialog$setModal(TRUE)
        dialog$run()

        DATA <- e$FileName
        if (file.exists(DATA) == TRUE) {
            ok_button["sensitive"] = FALSE
            import_ent$setText("")
            import_ent$modifyText(GtkStateType["normal"], "blue")
            import_ent$insertText(e$BaseName)
            check_button["sensitive"] = TRUE

            if (e$FilterName == ".csv") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read.table(DATA, header = T, sep = ",")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1 <- d1
            }
            if (e$FilterName == ".txt") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read.table(DATA, header = T, sep = "\t")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1 <- d1
            }
            if (e$FilterName == ".xlsx") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read_excel(DATA, 1) #Package:readxl
                d1 <- data.frame(d1)
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                #assign("dat1", d1, envir = e)
                e$dat1<-d1
            }
        } else {
            ok_button["sensitive"] = FALSE
            import_ent$setText("")
            import_ent$modifyText(GtkStateType["normal"], "red")
            import_ent$insertText("Error")
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("NULL")
            check_button["sensitive"] = FALSE
        }
    }

    ########################################################

    Check_func <- function(h, ...) {
        if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | is.data.frame(e$dat1) ==
            FALSE | is.numeric(as.matrix(e$dat1)) == FALSE | any(is.na(e$dat1)) ==
            TRUE | is.null(names(e$dat1)) == TRUE | nrow(e$dat1) < 4 |
            ncol(e$dat1) < 4) {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("Incorrect")
            war <- "Warnings:"
            if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | nrow(e$dat1) <
                4 | ncol(e$dat1) < 4) {
                war <- c(war, "Matrix dimension is incorrect.")
            }
            if (is.numeric(as.matrix(e$dat1)) == FALSE) {
                war <- c(war, "There are non-numerical values.")
            }
            if (any(is.na(e$dat1)) == TRUE) {
                war <- c(war, "Missing data.")
            }
            if (is.null(names(e$dat1)) == TRUE) {
                war <- c(war, "Matrix names are incorrect.")
            }
            war_window <- gtkMessageDialogNew(imp_window, "destroy-with-parent",
                GtkMessageType["error"], GtkButtonsType["close"], paste(war,
                  collapse = "\n"), show = FALSE)
            gSignalConnect(war_window, "response", gtkWidgetDestroy)
            war_window["modal"] <- TRUE
            war_window$run()
        } else {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "blue")
            check_ent$insertText("Correct")
            ok_button["sensitive"] = TRUE
        }
    }

    ########################################################

    imp_window <- gtkWindow(show = FALSE)
    imp_window$setTransientFor(Window)
    imp_window$setPosition("center-on-parent")
    imp_window["title"] <- "Import/Check Adjacent Bipartite Matrix"
    imp_window$setSizeRequest(370, 95)
    imp_window["border-width"] <- 7
    imp_window$setModal(TRUE)
    imp_window$setResizable(FALSE)

    tab1 <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
    imp_window$add(tab1)

    import_button <- gtkButton("Import")
    gSignalConnect(import_button, "clicked", f = Imp_func)
    import_button$setSizeRequest(70, 25)
    import_ent <- gtkEntry()
    import_ent$modifyText(GtkStateType["normal"], "red")
    import_ent$insertText("NULL")
    import_ent["editable"] <- FALSE
    name_label <- gtkLabel("Name:")
    name_ent <- gtkEntry()
    name_ent$setSizeRequest(77, 23)
    name_ent$insertText("Bipartite")
    gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

    hbox1 <- gtkHBox(FALSE, 1)
    vbox1 <- gtkVBox(FALSE, 1)
    vbox1$packStart(import_button, expand = TRUE, fill = FALSE)
    hbox1$packStart(vbox1, expand = FALSE, fill = FALSE)
    hbox1$packStart(import_ent, expand = TRUE, fill = TRUE)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(name_ent, expand = FALSE, fill = FALSE)

    check_button <- gtkButton("Check")
    gSignalConnect(check_button, "clicked", f = Check_func)
    check_button$setSizeRequest(70, 25)
    check_button["sensitive"] <- FALSE

    check_ent <- gtkEntry()
    check_ent$modifyText(GtkStateType["normal"], "red")
    check_ent$insertText("NULL")
    check_ent["editable"] <- FALSE
    hbox2 <- gtkHBox(FALSE, 1)
    vbox2 <- gtkVBox(FALSE, 1)
    vbox2$packStart(check_button, expand = TRUE, fill = FALSE)
    hbox2$packStart(vbox2, expand = FALSE, fill = FALSE)
    hbox2$packStart(check_ent, expand = TRUE, fill = TRUE)

    ok_func <- function(h, ...) {
        row.z <- apply(e$dat1, 1, sum)
        col.z <- apply(e$dat1, 2, sum)
        if (any(row.z == 0) == TRUE | any(col.z == 0) == TRUE) {
            war <- c("Warnings:", "There are rows or columns with zeros",
                "These were eliminated")
            war_window <- gtkMessageDialogNew(imp_window, "destroy-with-parent",
                GtkMessageType["error"], GtkButtonsType["close"], paste(war,
                  collapse = "\n"), show = FALSE)
            gSignalConnect(war_window, "response", gtkWidgetDestroy)
            war_window["modal"] <- TRUE
            war_window$run()
            c.z <- c(1:ncol(e$dat1))
            r.z <- c(1:nrow(e$dat1))
            c.z1 <- c.z[col.z == 0]
            r.z1 <- r.z[row.z == 0]
            if (length(r.z1) > 0) {
                e$dat1 <- e$dat1[-c(r.z1), ]
            }
            if (length(c.z1) > 0) {
                e$dat1 <- e$dat1[, -c(c.z1)]
            }
        }
        sapply(list(fExport, sB, fExport.Action.Tool), function(i) i["sensitive"] = TRUE)
        Notebook["page"] <- 1

        daf <- data.frame(rownames(e$dat1), e$dat1, row.names = NULL)
        colnames(daf) <- c("Nodes", colnames(e$dat1))
        name <- name.rep(n0 = name_ent$getText())
        imp_window$destroy()

        rgDAT <- rGtkDataFrame(daf)
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

        data.note$insertPageWithCloseButton(gsw, label.text = name)

        e$datB$DAT <- e$dat1
        names(e$datB)[length(e$datB)] <- name
        e$dat.all$DAT <- daf
        names(e$dat.all)[length(e$dat.all)] <- name

        ##### NETWORK#####

        dat <- as.matrix(e$dat1)
        e$mod.all$A <- network(dat, bipartite = TRUE) #Packages:network
        names(e$mod.all)[length(e$mod.all)] <- name

        sapply(list(Graph1, Graph2, Graph8, Graph9, sB, fExport, fExport.Action.Tool),
            function(i) i["sensitive"] <- TRUE)
    }

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = ok_func)
    ok_button$setSizeRequest(70, 25)
    ok_button["sensitive"] <- FALSE
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) imp_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    hbox3 <- gtkHBox(FALSE, 1)
    hbox3$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox3$packEnd(ok_button, expand = FALSE, fill = FALSE)

    tab1$attach(hbox1, left.attach = 0, 1, top.attach = 0, 1)
    tab1$attach(hbox2, left.attach = 0, 1, top.attach = 1, 2)
    tab1$attach(hbox3, left.attach = 0, 1, top.attach = 2, 3)

    ###################################################

    imp_window$setModal(TRUE)
    imp_window$showAll()
    imp_window$setResizable(FALSE)

}

#######################################################

#' Import-Unweighted Network
#' @keywords internal

wU <- function(h, ...) {
    #######################################################

    Imp_func <- function(h, ...) {

        dialog <- gtkFileChooserDialog(title = "Open a file", parent = imp_window,
            action = "open", "gtk-ok", GtkResponseType["ok"], "gtk-cancel",
            GtkResponseType["cancel"], show = FALSE)

        gSignalConnect(dialog, "response", f = function(dialog, response,
            data) {
            if (response == GtkResponseType["ok"]) {
                filename <- dialog$getFilename()
                e$BaseName <- basename(dialog$getFilename())
                e$FileName <- filename
                e$FilterName <- dialog$getFilter()$getName()
            } else {
                e$FileName <- "NULL"
            }
            dialog$destroy()
        })

        fileFilter <- gtkFileFilter()
        fileFilter$setName(c(".csv"))
        fileFilter$addPattern("*.csv")
        dialog$addFilter(fileFilter)

        fileFilter2 <- gtkFileFilter()
        fileFilter2$setName(".xlsx")
        fileFilter2$addPattern("*.xlsx")
        dialog$addFilter(fileFilter2)

        fileFilter3 <- gtkFileFilter()
        fileFilter3$setName(".txt")
        fileFilter3$addPattern("*.txt")
        dialog$addFilter(fileFilter3)

        dialog$setModal(TRUE)
        dialog$run()

        DATA <- e$FileName
        if (file.exists(DATA) == TRUE) {
            ok_button["sensitive"] <- FALSE
            import_ent$setText("")
            import_ent$modifyText(GtkStateType["normal"], "blue")
            import_ent$insertText(e$BaseName)
            check_button["sensitive"] <- TRUE

            if (e$FilterName == ".csv") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read.table(DATA, header = T, sep = ",")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn <- 1
                } else {
                  rn <- NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1 <- d1
            }
            if (e$FilterName == ".txt") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read.table(DATA, header = T, sep = "\t")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn <- 1
                } else {
                  rn <- NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1 <- d1
            }
            if (e$FilterName == ".xlsx") {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                d1 <- read_excel(DATA, 1) #Package:readxl
                d1 <- data.frame(d1)
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn <- 1
                } else {
                  rn <- NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1 <- d1
            }
        } else {
            ok_button["sensitive"] <- FALSE
            import_ent$setText("")
            import_ent$modifyText(GtkStateType["normal"], "red")
            import_ent$insertText("Error")
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("NULL")
            check_button["sensitive"] <- FALSE
        }
    }

    ########################################################

    Check_func <- function(h, ...) {
        if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | nrow(e$dat1) != ncol(e$dat1) |
            is.data.frame(e$dat1) == FALSE | is.numeric(as.matrix(e$dat1)) ==
            FALSE | any(as.matrix(e$dat1) != 1 & as.matrix(e$dat1) != 0) ==
            TRUE | any(is.na(e$dat1)) == TRUE | is.null(names(e$dat1)) ==
            TRUE) {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("Incorrect")
            war <- "Warnings:"
            if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | nrow(e$dat1) !=
                ncol(e$dat1)) {
                war <- c(war, "Matrix dimension is incorrect.")
            }
            if (is.numeric(as.matrix(e$dat1)) == FALSE) {
                war <- c(war, "There are non-numerical values.")
            }
            if (any(as.matrix(e$dat1) != 1 & as.matrix(e$dat1) != 0 & is.na(e$dat1) ==
                FALSE) == TRUE) {
                war <- c(war, "There are values other than 0 and 1.")
            }
            if (any(is.na(e$dat1)) == TRUE) {
                war <- c(war, "Missing data.")
            }
            if (is.null(names(e$dat1)) == TRUE) {
                war <- c(war, "Matrix names are incorrect.")
            }
            war_window <- gtkMessageDialogNew(imp_window, "destroy-with-parent",
                GtkMessageType["error"], GtkButtonsType["close"], paste(war,
                  collapse = "\n"), show = FALSE)
            gSignalConnect(war_window, "response", gtkWidgetDestroy)
            war_window["modal"] <- TRUE
            war_window$run()
        } else {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "blue")
            check_ent$insertText("Correct")
            ok_button["sensitive"] <- TRUE
        }
    }

    ########################################################

    imp_window <- gtkWindow(show = FALSE)
    imp_window$setTransientFor(Window)
    imp_window$setPosition("center-on-parent")
    imp_window["title"] <- "Import/Check Adjacent Unweighted Matrix"
    imp_window$setSizeRequest(370, 95)
    imp_window["border-width"] <- 7
    imp_window$setModal(TRUE)
    imp_window$setResizable(FALSE)

    tab1 <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
    imp_window$add(tab1)

    import_button <- gtkButton("Import")
    gSignalConnect(import_button, "clicked", f = Imp_func)
    import_button$setSizeRequest(70, 25)
    import_ent <- gtkEntry()
    import_ent$modifyText(GtkStateType["normal"], "red")
    import_ent$insertText("NULL")
    import_ent["editable"] <- FALSE
    name_label <- gtkLabel("Name:")
    name_ent <- gtkEntry()
    name_ent$setSizeRequest(77, 23)
    name_ent$insertText("Unweighted")
    gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

    hbox1 <- gtkHBox(FALSE, 1)
    vbox1 <- gtkVBox(FALSE, 1)
    vbox1$packStart(import_button, expand = TRUE, fill = FALSE)
    hbox1$packStart(vbox1, expand = FALSE, fill = FALSE)
    hbox1$packStart(import_ent, expand = TRUE, fill = TRUE)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(name_ent, expand = FALSE, fill = FALSE)

    check_button <- gtkButton("Check")
    gSignalConnect(check_button, "clicked", f = Check_func)
    check_button$setSizeRequest(70, 25)
    check_button["sensitive"] <- FALSE

    check_ent <- gtkEntry()
    check_ent$modifyText(GtkStateType["normal"], "red")
    check_ent$insertText("NULL")
    check_ent["editable"] <- FALSE
    hbox2 <- gtkHBox(FALSE, 1)
    vbox2 <- gtkVBox(FALSE, 1)
    vbox2$packStart(check_button, expand = TRUE, fill = FALSE)
    hbox2$packStart(vbox2, expand = FALSE, fill = FALSE)
    hbox2$packStart(check_ent, expand = TRUE, fill = TRUE)

    ok_func <- function(h, ...) {
        #####
        double.z <- apply(e$dat1, 1, sum) + apply(e$dat1, 2, sum)
        if (any(double.z == 0) == TRUE) {
            war <- c("Warnings:", "There are nodes with double zeros.",
                "These were eliminated")
            war_window <- gtkMessageDialogNew(imp_window, "destroy-with-parent",
                GtkMessageType["error"], GtkButtonsType["close"], paste(war,
                  collapse = "\n"), show = FALSE)
            gSignalConnect(war_window, "response", gtkWidgetDestroy)
            war_window["modal"] <- TRUE
            war_window$run()
            d.z <- c(1:ncol(e$dat1))
            d.z1 <- d.z[double.z == 0]
            e$dat1 <- e$dat1[-c(d.z1), ]
            e$dat1 <- e$dat1[, -c(d.z1)]
        }

        #####
        sapply(list(fExport, sU, fExport.Action.Tool), function(i) i["sensitive"] <- TRUE)
        Notebook["page"] <- 1

        daf <- data.frame(rownames(e$dat1), e$dat1)
        colnames(daf) <- c("Nodes", colnames(e$dat1))
        name <- name.rep(n0 = name_ent$getText())
        imp_window$destroy()

        rgDAT <- rGtkDataFrame(daf)
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

        data.note$insertPageWithCloseButton(gsw, label.text = name)

        e$datU$DAT <- e$dat1
        names(e$datU)[length(e$datU)] <- name
        e$dat.all$DAT <- e$dat1
        names(e$dat.all)[length(e$dat.all)] <- name

        #####

        ##### DATA#####

        dat <- as.matrix(e$dat1)

        #### COMMUNITY#####

        NODE <- colnames(dat)
        colnames(dat) <- rownames(dat) <- NODE
        COM1 <- Community(nodes = data.frame(node = NODE), trophic.links = PredationMatrixToLinks(dat),
            properties = list(title = name)) #Packages:cheddar
        e$COM.all$A <- COM1
        if (any(names(e$COM.all) == name)) {
            e$COM.all$A <- NULL
        } else {
            names(e$COM.all)[length(e$COM.all)] <- name
        }

        ##### NETWORK#####

        net <- network(dat)  #Packages:network
        e$mod.all$A <- net
        names(e$mod.all)[length(e$mod.all)] <- name

        #####

        sapply(list(Graph1, Graph2, Graph3, Graph4, Graph5, sU, fExport,
            fExport.Action.Tool), function(i) i["sensitive"] <- TRUE)
    }

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = ok_func)
    ok_button$setSizeRequest(70, 25)
    ok_button["sensitive"] <- FALSE
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) imp_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    hbox3 <- gtkHBox(FALSE, 1)
    hbox3$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox3$packEnd(ok_button, expand = FALSE, fill = FALSE)

    tab1$attach(hbox1, left.attach = 0, 1, top.attach = 0, 1)
    tab1$attach(hbox2, left.attach = 0, 1, top.attach = 1, 2)
    tab1$attach(hbox3, left.attach = 0, 1, top.attach = 2, 3)

    ###################################################
    imp_window$setModal(TRUE)
    imp_window$showAll()
    imp_window$setResizable(FALSE)
}

#######################################################

#' Import-Weighted Network
#' @keywords internal

wW <- function(h, ...) {
    ############################################################

    Check_func <- function(h, ...) {

        mat <- e$dat1[1:nrow(e$dat1), 1:nrow(e$dat1)]
        inputs <- e$dat1[, nrow(e$dat1) + 1]
        exports <- e$dat1[, nrow(e$dat1) + 2]
        outputs <- e$dat1[, nrow(e$dat1) + 3]
        Respiration <- e$dat1[, nrow(e$dat1) + 4]
        Storage <- e$dat1[, nrow(e$dat1) + 5]
        Living <- e$dat1[, nrow(e$dat1) + 6]
        k <- pack(flow = mat, input = inputs, export = exports, output = outputs,
            living = as.logical(Living), respiration = Respiration, storage = Storage)
        #assign("mod0", k, env = e)
        e$mod0<-k
        if (ssCheck(e$mod0) == TRUE) {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "blue")
            check_ent$insertText("Correct. Balanced.")
            combobox["sensitive"] <- FALSE
            gtkComboBoxSetActive(combobox, 0)
            ok_button["sensitive"] = TRUE
            e$mod1<-e$mod0
            #assign("mod1", e$mod0, env = e)
        } else {
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("Incorrect. Unbalanced.")
            ok_button["sensitive"] = FALSE
            gtkComboBoxSetActive(combobox, 0)
            combobox["sensitive"] <- TRUE

        }
    }

    #######################################################

    Imp_func <- function(h, ...) {

        dialog <- gtkFileChooserDialog(title = "Open a file", parent = imp_window,
            action = "open", "gtk-ok", GtkResponseType["ok"], "gtk-cancel",
            GtkResponseType["cancel"], show = FALSE)

        gSignalConnect(dialog, "response", f = function(dialog, response,
            data) {
            if (response == GtkResponseType["ok"]) {
                filename <- dialog$getFilename()
                e$BaseName <- basename(dialog$getFilename())
                e$FileName = filename
                e$FilterName = dialog$getFilter()$getName()
            } else {
                e$FileName = "NULL"
            }
            dialog$destroy()
        })

        fileFilter <- gtkFileFilter()
        fileFilter$setName(c(".csv"))
        fileFilter$addPattern("*.csv")
        dialog$addFilter(fileFilter)

        fileFilter2 <- gtkFileFilter()
        fileFilter2$setName(".xlsx")
        fileFilter2$addPattern("*.xlsx")
        dialog$addFilter(fileFilter2)

        fileFilter3 <- gtkFileFilter()
        fileFilter3$setName(".txt")
        fileFilter3$addPattern("*.txt")
        dialog$addFilter(fileFilter3)

        dialog$setModal(TRUE)
        dialog$run()

        DATA <- e$FileName
        t = 0
        if (file.exists(DATA) == TRUE) {
            t = 1
            if (e$FilterName == ".csv") {
                d1 <- read.table(DATA, header = T, sep = ",")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1<-d1
                #assign("dat1", d1, envir = e)
            }
            if (e$FilterName == ".txt") {
                d1 <- read.table(DATA, header = T, sep = "\t")
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1<-d1
                #assign("dat1", d1, envir = e)
            }
            if (e$FilterName == ".xlsx") {
                d1 <- read_excel(DATA, 1) #Package:readxl
                d1 <- data.frame(d1)
                if (anyDuplicated(d1[, 1]) == 0 & anyNA(d1[, 1]) == FALSE) {
                  rn = 1
                } else {
                  rn = NULL
                }
                d1 <- data.frame(d1, row.names = rn)
                e$dat1<-d1
                #assign("dat1", d1, envir = e)
            }

        } else {
            ok_button["sensitive"] = FALSE
            import_ent$setText("")
            import_ent$modifyText(GtkStateType["normal"], "red")
            import_ent$insertText("Error")
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("NULL")
            check_button["sensitive"] = FALSE

            model_Nodes$setFrame(data.frame())
            model_Input$setFrame(data.frame())
            model_Export$setFrame(data.frame())
            model_Output$setFrame(data.frame())
            model_Respiration$setFrame(data.frame())
            model_Storage$setFrame(data.frame())
            model_Living$setFrame(data.frame())
        }

        if (t == 1) {
            if (ncol(e$dat1) > 0 & nrow(e$dat1) > 0 & nrow(e$dat1) != ncol(e$dat1) &
                is.data.frame(e$dat1) == TRUE & all(is.na(e$dat1)) == FALSE &
                is.null(rownames(e$dat1)) == FALSE & is.null(colnames(e$dat1)) ==
                FALSE & is.numeric(as.matrix(e$dat1)) == TRUE & (nrow(e$dat1) +
                6) == ncol(e$dat1)) {

                ok_button["sensitive"] = FALSE
                import_ent$setText("")
                import_ent$modifyText(GtkStateType["normal"], "blue")
                import_ent$insertText(e$BaseName)
                check_button["sensitive"] = TRUE
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                combobox["sensitive"] <- FALSE
                gtkComboBoxSetActive(combobox, 0)
                nam <- colnames(e$dat1)

                model_Nodes$setFrame(data.frame(colnames(e$dat1)[1:nrow(e$dat1)]))
                model_Input$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  1]))
                model_Export$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  2]))
                model_Output$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  3]))
                model_Respiration$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  4]))
                model_Storage$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  5]))
                model_Living$setFrame(data.frame(colnames(e$dat1)[nrow(e$dat1) +
                  6]))

            } else {
                ok_button["sensitive"] = FALSE
                import_ent$setText("")
                import_ent$modifyText(GtkStateType["normal"], "red")
                import_ent$insertText("Error")
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "red")
                check_ent$insertText("NULL")
                check_button["sensitive"] = FALSE
                combobox["sensitive"] <- FALSE
                gtkComboBoxSetActive(combobox, 0)

                model_Nodes$setFrame(data.frame())
                model_Input$setFrame(data.frame())
                model_Export$setFrame(data.frame())
                model_Output$setFrame(data.frame())
                model_Respiration$setFrame(data.frame())
                model_Storage$setFrame(data.frame())
                model_Living$setFrame(data.frame())

                war <- "Warnings:"
                if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | nrow(e$dat1) ==
                  ncol(e$dat1) | (nrow(e$dat1) + 6) == ncol(e$dat1)) {
                  war <- c(war, "Matrix dimension is incorrect.")
                }
                if (is.numeric(as.matrix(e$dat1)) == FALSE) {
                  war <- c(war, "There are non-numerical values.")
                }
                if (ncol(e$dat1) == 0 | nrow(e$dat1) == 0 | nrow(e$dat1) ==
                  ncol(e$dat1) | (nrow(e$dat1) + 6) != ncol(e$dat1)) {
                  war <- c(war, "Matrix dimension is incorrect.")
                }
                if (any(is.na(e$dat1)) == TRUE) {
                  war <- c(war, "Missing data.")
                }
                if (is.null(names(e$dat1)) == TRUE) {
                  war <- c(war, "Matrix names are incorrect.")
                }
                war_window <- gtkMessageDialogNew(imp_window, "destroy-with-parent",
                  GtkMessageType["error"], GtkButtonsType["close"], paste(war,
                    collapse = "\n"), show = FALSE)
                gSignalConnect(war_window, "response", gtkWidgetDestroy)
                war_window["modal"] <- TRUE
                war_window$run()
            }
        }
    }

    ##################################################
    options(warn = -1)

    #assign("mod0", 0, env = e)
    #assign("mod1", 0, env = e)
    e$mod0<-0
    e$mod1<-0

    imp_window <- gtkWindow(show = FALSE)
    imp_window$setTransientFor(Window)
    imp_window$setPosition("center-on-parent")
    imp_window["title"] <- "Import/Check Adjacent Weighted Matrix"
    imp_window$setSizeRequest(480, 300)
    imp_window["border-width"] <- 7
    imp_window$setModal(TRUE)
    imp_window$setResizable(FALSE)

    ##################################################

    tab1 <- gtkTable(rows = 8, columns = 3, homogeneous = TRUE)
    imp_window$add(tab1)

    import_button <- gtkButton("Import")
    gSignalConnect(import_button, "clicked", f = Imp_func)
    import_button$setSizeRequest(70, 25)
    import_ent <- gtkEntry()
    import_ent$modifyText(GtkStateType["normal"], "red")
    import_ent$insertText("NULL")
    import_ent["editable"] <- FALSE
    name_label <- gtkLabel("Name:")
    name_ent <- gtkEntry()
    name_ent$setSizeRequest(77, 23)
    name_ent$insertText("Weighted")
    gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

    hbox1 <- gtkHBox(FALSE, 1)
    vbox1 <- gtkVBox(FALSE, 1)
    vbox1$packStart(import_button, expand = TRUE, fill = FALSE)
    hbox1$packStart(vbox1, expand = FALSE, fill = FALSE)
    hbox1$packStart(import_ent, expand = TRUE, fill = TRUE)
    hbox1$packStart(name_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(name_ent, expand = FALSE, fill = FALSE)

    ##################################################

    view1 <- gtkTreeView()
    view1$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Nodes <- rGtkDataFrame(data.frame())
    view1$setModel(model_Nodes)
    scrolled_Nodes <- gtkScrolledWindow()
    scrolled_Nodes$add(view1)
    view1["headers-visible"] = FALSE
    frame_Nodes <- gtkFrame("*Nodes")
    frame_Nodes$add(scrolled_Nodes)

    view2 <- gtkTreeView()
    view2$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Input <- rGtkDataFrame(data.frame(integer(0)))
    view2$setModel(model_Input)
    view2["headers-visible"] = FALSE
    frame_Input <- gtkFrame("*Inputs")
    frame_Input$add(view2)

    view3 <- gtkTreeView()
    view3$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Output <- rGtkDataFrame(data.frame())
    view3$setModel(model_Output)
    view3["headers-visible"] = FALSE
    frame_Output <- gtkFrame("Outputs")
    frame_Output$add(view3)

    view4 <- gtkTreeView()
    view4$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Export <- rGtkDataFrame(data.frame())
    view4$setModel(model_Export)
    view4["headers-visible"] = FALSE
    frame_Export <- gtkFrame("*Exports")
    frame_Export$add(view4)

    view5 <- gtkTreeView()
    view5$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Respiration <- rGtkDataFrame(data.frame())
    view5$setModel(model_Respiration)
    view5["headers-visible"] = FALSE
    frame_Respiration <- gtkFrame("Respiration")
    frame_Respiration$add(view5)

    view6 <- gtkTreeView()
    view6$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Living <- rGtkDataFrame(data.frame())
    view6$setModel(model_Living)
    view6["headers-visible"] = FALSE
    frame_Living <- gtkFrame("Living")
    frame_Living$add(view6)

    view7 <- gtkTreeView()
    view7$insertColumnWithAttributes(0, "", gtkCellRendererText(), text = 0)
    model_Storage <- rGtkDataFrame(data.frame())
    view7$setModel(model_Storage)
    view7["headers-visible"] = FALSE
    frame_Storage <- gtkFrame("Storage")
    frame_Storage$add(view7)

    ###########################################################

    balance_func <- function(h, ...) {
        iter <- gtkComboBoxGetActiveIter(combobox)$iter
        item <- model$GetValue(iter, 0)$value
        if (item == "NULL") {
            ok_button["sensitive"] = FALSE
            check_ent$setText("")
            check_ent$modifyText(GtkStateType["normal"], "red")
            check_ent$insertText("NULL")
        } else {
            k2 <- balance(e$mod0, method = item, tol = 5) #Package:enaR
            if (ssCheck(k2) == TRUE) {
                ok_button["sensitive"] = TRUE
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "blue")
                check_ent$insertText(paste("Correct. Balanced. ", "(",
                  item, ")", sep = ""))
                #assign("mod1", k2, env = e)
                e$mod1<-k2
            } else {
                check_ent$setText("")
                check_ent$modifyText(GtkStateType["normal"], "blue")
                check_ent$insertText(paste("Incorrect. Unbalanced. ", "(",
                  item, ")", sep = ""))
            }
        }
    }

    ########################################################

    ok_func <- function(h, ...) {
        d2 <- unpack(e$mod1)
        d2.1 <- data.frame(colnames(d2$F), d2$F, d2$z, d2$e, d2$y, d2$r,
            d2$X, as.numeric(d2$living))
        d2.1 <- format(d2.1, digits = 3, nsmall = 4)
        rownames(d2.1) <- NULL
        colnames(d2.1) <- c("Nodes", colnames(d2$F), "Inp", "Exp", "Out",
            "Res", "Stor", "Liv")
        name <- name.rep(n0 = name_ent$getText())
        imp_window$destroy()
        sapply(list(fExport, sW, fExport.Action.Tool), function(i) i["sensitive"] = TRUE)
        Notebook["page"] <- 1

        rgDAT <- rGtkDataFrame(d2.1)
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
        data.note$insertPageWithCloseButton(gsw, label.text = name)
        d2.1[, 2:ncol(d2.1)] <- apply(d2.1[, 2:ncol(d2.1)], 2, as.numeric)

        e$datW$DAT <- d2.1
        names(e$datW)[length(e$datW)] <- name
        e$dat.all$DAT <- d2.1
        names(e$dat.all)[length(e$dat.all)] <- name
        e$modW$A <- e$mod1
        e$mod.all$A <- e$mod1
        names(e$modW)[length(e$modW)] <- name
        names(e$mod.all)[length(e$mod.all)] <- name

        sapply(list(Graph1, Graph2, Graph7, sW, fExport, fExport.Action.Tool),
            function(i) i["sensitive"] <- TRUE)
    }

    ########################################################

    check_button <- gtkButton("Check")
    gSignalConnect(check_button, "clicked", f = Check_func)
    check_button$setSizeRequest(70, 25)
    check_button["sensitive"] <- FALSE

    check_ent <- gtkEntry()
    check_ent$setSizeRequest(142, 23)
    check_ent$modifyText(GtkStateType["normal"], "red")
    check_ent$insertText("NULL")
    check_ent["editable"] <- FALSE
    hbox2 <- gtkHBox(FALSE, 1)
    vbox2 <- gtkVBox(FALSE, 1)
    vbox2$packStart(check_button, expand = TRUE, fill = FALSE)
    hbox2$packStart(vbox2, expand = FALSE, fill = FALSE)
    hbox2$packStart(check_ent, expand = FALSE, fill = FALSE)
    balance_label <- gtkLabel("Balance:")
    model <- rGtkDataFrame(c("NULL", "I", "O", "IO", "OI", "AVG", "AVG2"))
    combobox <- gtkComboBox(model)
    gSignalConnect(combobox, "changed", balance_func)
    combobox["sensitive"] <- FALSE
    crt <- gtkCellRendererText()
    combobox$packStart(crt)
    combobox$addAttribute(crt, "text", 0)
    hbox2$packStart(balance_label, expand = FALSE, fill = FALSE)
    vbox3 <- gtkVBox(FALSE, 1)
    vbox3$packStart(combobox, expand = TRUE, fill = FALSE)
    hbox2$packStart(vbox3, expand = FALSE, fill = FALSE)

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = ok_func)
    ok_button$setSizeRequest(70, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) imp_window$destroy())
    cancel_button$setSizeRequest(70, 25)
    vbox4 <- gtkVBox(FALSE, 1)
    vbox4$packStart(ok_button, expand = TRUE, fill = FALSE)
    vbox5 <- gtkVBox(FALSE, 1)
    vbox5$packStart(cancel_button, expand = TRUE, fill = FALSE)
    hbox2$packEnd(vbox5, expand = FALSE, fill = FALSE)
    hbox2$packEnd(vbox4, expand = FALSE, fill = FALSE)

    tab1$attach(hbox1, left.attach = 0, 3, top.attach = 0, 1)
    tab1$attach(frame_Nodes, left.attach = 0, 1, top.attach = 1, 7)
    tab1$attach(frame_Input, left.attach = 1, 2, top.attach = 1, 3)
    tab1$attach(frame_Output, left.attach = 1, 2, top.attach = 3, 5)
    tab1$attach(frame_Export, left.attach = 1, 2, top.attach = 5, 7)
    tab1$attach(frame_Respiration, left.attach = 2, 3, top.attach = 1,
        3)
    tab1$attach(frame_Living, left.attach = 2, 3, top.attach = 3, 5)
    tab1$attach(frame_Storage, left.attach = 2, 3, top.attach = 5, 7)
    tab1$attach(hbox2, left.attach = 0, 3, top.attach = 7, 8)

    ########################################################

    ok_button["sensitive"] = FALSE
    combobox["sensitive"] <- FALSE
    gtkComboBoxSetActive(combobox, 0)
    check_button["sensitive"] = FALSE
    check_ent$setText("")
    check_ent$modifyText(GtkStateType["normal"], "red")
    check_ent$insertText("NULL")

    #########################################################

    imp_window$setModal(TRUE)
    imp_window$showAll()
    imp_window$setResizable(FALSE)

}


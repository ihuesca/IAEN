#' Headmap
#' @keywords internal

window_plot_heatmap <- function(h, ...) {

    plot.grid <- function(dat, dat.melt, zT, col.grid, id.nam, clus, is.binary) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  plot.res
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(heatmap_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "changed", f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE

        }

        #################################################

        heatmap_graphic <- gtkWindow(show = TRUE)
        heatmap_graphic["title"] <- paste("Heatmap:", id.nam)
        heatmap_graphic$setSizeRequest(500, 400)
        heatmap_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        heatmap_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) heatmap_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        cols <- c("white", col.grid)

        if (clus == FALSE) {
            Sys.sleep(0.1)
            plot.res <- ggplot(data = dat.melt, aes(x = X1, y = X2, fill = value)) + #Package:ggplot2
                geom_tile(color = "black") + theme(axis.text.x = element_text(angle = 90,
                hjust = 1, vjust = 0.5, colour = "black", size = zT), axis.text.y = element_text(colour = "black",
                size = zT), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank()) + scale_fill_gradientn(colours = cols) +
                guides(fill = FALSE) + labs(x = NULL, y = NULL)
            print(plot.res)
        } else {
            if (is.binary == TRUE) {
                Sys.sleep(0.1)
                dist.method <- plot.res %<a-% heatmap.2(x = dat, col = cols, #Package:gplots
                  symbreaks = FALSE, tracecol = "black", key = FALSE, linecol = FALSE,
                  distfun = function(x) {
                    dist(x, method = "binary")
                  }, density.info = "none", keysize = 0.7, offsetCol = 0,
                  offsetRow = 0, margins = c(7, 7), trace = "both", cexRow = zT/7,
                  cexCol = zT/7)
                plot.res
            } else {
                hc.rows <- hclust(dist(dat, method = "euclidean"))
                hc.cols <- hclust(dist(dat, method = "euclidean"))
                dat[dat == 0] <- NA
                col.deg <- colorRampPalette(c("white", col.grid))
                cols <- c(col.deg(10)[2:10])
                Sys.sleep(0.1)
                plot.res %<a-% heatmap.2(dat, Colv = as.dendrogram(hc.cols), #Package:gplots
                  Rowv = as.dendrogram(hc.rows), scale = "none", col = cols,
                  tracecol = 0, linecol = "black", hline = 0, vline = 0,
                  na.color = "white", key = FALSE, trace = "both", cexRow = zT/7,
                  cexCol = zT/7, density.info = "none", rowsep = 8, offsetCol = 0,
                  offsetRow = 0, keysize = 0.7, margins = c(7, 7))
                plot.res
            }

        }
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(heatmap_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)

    #################################################

    heatmap_window <- gtkWindow(show = FALSE)
    heatmap_window$setTransientFor(Window)
    heatmap_window$setPosition("center-on-parent")
    heatmap_window["title"] <- "Heatmap"
    heatmap_window$setSizeRequest(200, 170)
    heatmap_window["border-width"] <- 2
    heatmap_window$setModal(TRUE)
    heatmap_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    heatmap_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$mod.all))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Cluster

    hbox_t1 <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("With Cluster:")
    cluster_no <- gtkRadioButton(label = "No")
    cluster_yes <- gtkRadioButtonNewWithLabelFromWidget(group = cluster_no,
        label = "Yes")

    hbox_t1$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_t1$packStart(cluster_no, FALSE, FALSE, 0)
    hbox_t1$packStart(cluster_yes, FALSE, FALSE, 0)
    opt_table$attach(hbox_t1, left.attach = 0, 1, top.attach = 0, 1)

    #################################################

    # Color heatmap

    hbox_t2 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Heatmap color:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_t2$packStart(button, FALSE, FALSE, 0)
    hbox_t2$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_t2, left.attach = 0, 1, top.attach = 1, 2, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size text

    hbox_t3 <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("Size text:")
    sizeT_ent <- gtkEntry()
    sizeT_ent$setSizeRequest(20, 20)
    sizeT_ent$insertText(5)
    gSignalConnect(sizeT_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_t3$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_t3$packStart(sizeT_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_t3, left.attach = 0, 1, top.attach = 2, 3)

    #################################################

    PlotOk <- function(h, ...) {
        heatmap_window["visible"] <- FALSE
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        g.net0 <- e$mod.all[[item]]
        if (network::is.bipartite(g.net0) == TRUE) {
            dat <- as.matrix(e$datB[[item]])
        } else {
            dat <- as.matrix(g.net0, attrname = "flow")
        }
        dat.melt <- melt(dat)
        if (cluster_no["active"]) {
            Clust <- FALSE
        } else if (cluster_yes["active"]) {
            Clust <- TRUE
        }
        if (any(!(dat == 0 | dat == 1))) {
            is.binary <- FALSE
        } else {
            is.binary <- TRUE
        }
        col.grid <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)

        plot.grid(dat = dat, dat.melt = dat.melt, zT = as.numeric(sizeT_ent$getText()),
            col.grid = col.grid, id.nam = item, clus = Clust, is.binary = is.binary)
        heatmap_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) heatmap_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    heatmap_window$showAll()

}

#################################################

#' Impact
#' @keywords internal

window_plot_impact <- function(h, ...) {

    plot.impact <- function(dat.melt, zT, low.col, high.col, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(plot.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(impact_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE
        }

        #################################################

        impact_graphic <- gtkWindow(show = TRUE)
        impact_graphic["title"] <- paste("Impact:", id.nam)
        impact_graphic$setSizeRequest(500, 400)
        impact_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        impact_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) impact_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        Sys.sleep(0.1)
        plot.res <- ggplot(data = dat.melt, aes(x = X1, y = X2, fill = value)) + #Package:ggplot2
            geom_tile(color = "black") + theme(axis.text.x = element_text(angle = 90,
            hjust = 1, vjust = 0.5, colour = "black", size = zT), axis.text.y = element_text(colour = "black",
            size = zT), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()) + scale_fill_gradient2(low = low.col,
            mid = "white", high = high.col, midpoint = 0) + labs(x = NULL,
            y = NULL)
        print(plot.res)
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(impact_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    color_func2 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(impact_window)
        dialog$setPosition("center-on-parent")
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color2$red <- color[["red"]]
            e$color2$green <- color[["green"]]
            e$color2$blue <- color[["blue"]]
            drawing_area2$modifyBg("normal", e$color2)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)
    e$color2 <- list(red = 0, green = 0, blue = 65535)

    #################################################

    impact_window <- gtkWindow(show = FALSE)
    impact_window$setTransientFor(Window)
    impact_window$setPosition("center-on-parent")
    impact_window["title"] <- "Impact"
    impact_window$setSizeRequest(200, 170)
    impact_window["border-width"] <- 2
    impact_window$setModal(TRUE)
    impact_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    impact_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$modW))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Positive

    hbox_pos <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Positives values:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_pos$packStart(button, FALSE, FALSE, 0)
    hbox_pos$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_pos, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Negative

    hbox_neg <- gtkHBoxNew(FALSE, 5)

    fr_color2 <- gtkFrame()
    drawing_area2 <- gtkDrawingArea()
    drawing_area2$setSizeRequest(25, 20)
    drawing_area2$modifyBg("normal", e$color2)
    fr_color2$add(drawing_area2)

    button2 <- gtkButtonNewWithMnemonic("_Negatives values:")
    gSignalConnect(button2, "clicked", color_func2)

    hbox_neg$packStart(button2, FALSE, FALSE, 0)
    hbox_neg$packStart(fr_color2, FALSE, FALSE, 0)
    opt_table$attach(hbox_neg, left.attach = 0, 1, top.attach = 1, 2, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size text

    # add a horizontal layout
    hbox_text <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("Size text:")
    sizeT_ent <- gtkEntry()
    sizeT_ent$setSizeRequest(20, 20)
    sizeT_ent$insertText(10)
    gSignalConnect(sizeT_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_text$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_text$packStart(sizeT_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_text, left.attach = 0, 1, top.attach = 2, 3)

    #################################################

    PlotOk <- function(h, ...) {
        impact_window["visible"] <- FALSE
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        mod <- e$modW[[item]]
        mod.imp <- enaMTI(mod)$M
        dat.melt <- melt(as.matrix(mod.imp))
        low <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)
        high <- rgb(e$color2$red, e$color2$green, e$color2$blue, maxColorValue = 65535)

        plot.impact(dat.melt = dat.melt, zT = as.numeric(sizeT_ent$getText()),
            low.col = low, high.col = high, id.nam = item)
        impact_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) impact_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    impact_window$showAll()

}

#################################################

#' Incidence
#' @keywords internal

window_plot_inc <- function(h, ...) {

    plot.inc <- function(g.inc, col.1, col.2, zL, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(plot.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(incidence_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE

        }

        #################################################

        incidence_graphic <- gtkWindow(show = TRUE)
        incidence_graphic["title"] <- paste("Incidence:", id.nam)
        incidence_graphic$setSizeRequest(500, 400)
        incidence_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        incidence_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) incidence_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        #############################################################

        Sys.sleep(0.1)
        shapes <- c("circle", "square")
        colors <- c(col.1, col.2)
        plot.res %<a-% plot(g.inc, vertex.color = colors[V(g.inc)$type + #Package:igraph
            1], vertex.shape = shapes[V(g.inc)$type + 1], vertex.size = 10,
            vertex.label.degree = -pi/2, vertex.label.dist = 1.2, vertex.label.cex = zL)
        print(plot.res)

    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(incidence_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    color_func2 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(incidence_window)
        dialog$setPosition("center-on-parent")
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color2$red <- color[["red"]]
            e$color2$green <- color[["green"]]
            e$color2$blue <- color[["blue"]]
            drawing_area2$modifyBg("normal", e$color2)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)
    e$color2 <- list(red = 0, green = 0, blue = 65535)

    #################################################

    incidence_window <- gtkWindow(show = FALSE)
    incidence_window$setTransientFor(Window)
    incidence_window$setPosition("center-on-parent")
    incidence_window["title"] <- "Incidence"
    incidence_window$setSizeRequest(200, 170)
    incidence_window["border-width"] <- 2
    incidence_window$setModal(TRUE)
    incidence_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    incidence_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$datB))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Nodes 1

    hbox_nod1 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Nodes 1:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_nod1$packStart(button, FALSE, FALSE, 0)
    hbox_nod1$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_nod1, left.attach = 0, 1, top.attach = 0, 1,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Nodes 2

    hbox_nod2 <- gtkHBox(FALSE, 5)

    fr_color2 <- gtkFrame()
    drawing_area2 <- gtkDrawingArea()
    drawing_area2$setSizeRequest(25, 20)
    drawing_area2$modifyBg("normal", e$color2)
    fr_color2$add(drawing_area2)

    button2 <- gtkButtonNewWithMnemonic("_Nodes 1:")
    gSignalConnect(button2, "clicked", f = color_func2)

    hbox_nod2$packStart(button2, FALSE, FALSE, 0)
    hbox_nod2$packStart(fr_color2, FALSE, FALSE, 0)
    opt_table$attach(hbox_nod2, left.attach = 0, 1, top.attach = 1, 2,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Size nodes

    # add a horizontal layout
    hbox_sizeN <- gtkHBox(FALSE, 5)

    sizeN_label <- gtkLabel("Size text:")
    sizeN_ent <- gtkEntry()
    sizeN_ent$setSizeRequest(20, 20)
    sizeN_ent$insertText(1)
    gSignalConnect(sizeN_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_sizeN$packStart(sizeN_label, FALSE, FALSE, 0)
    hbox_sizeN$packStart(sizeN_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_sizeN, left.attach = 0, 1, top.attach = 2, 3)

    #################################################

    PlotOk <- function(h, ...) {
        incidence_window["visible"] <- FALSE
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        g.inc <- graph.incidence(e$datB[[item]])

        col.1 <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)
        col.2 <- rgb(e$color2$red, e$color2$green, e$color2$blue, maxColorValue = 65535)

        plot.inc(g.inc = g.inc, col.1 = col.1, col.2 = col.2, zL = as.numeric(sizeN_ent$getText()),
            id.nam = item)
        incidence_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) incidence_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    incidence_window$showAll()

}

#################################################

#' Centrality
#' @keywords internal

window_plot_centrality <- function(h, ...) {

    plot.cent <- function(comm1, cent, ylab, show.nodes.as, col, n.nod,
        id.nam, NAs.value) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(plot.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(centrality_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE
        }

        #################################################

        centrality_graphic <- gtkWindow(show = TRUE)
        centrality_graphic["title"] <- paste("Web by centrality:", id.nam)
        centrality_graphic$setSizeRequest(500, 400)
        centrality_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        centrality_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) centrality_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        ############################################

        Sys.sleep(0.1)
        ### print plot
        symbol.spec <- c(`1` = 19, `2` = 23)
        plot.res %<a-% PlotNPS(comm1, 1:n.nod, cent, are.values = TRUE, #Package:cheddar
            main = NULL, ylab = ylab, cex = 0.7, show.nodes.as = show.nodes.as,
            node.labels = 1:n.nod, col = col, frame.plot = FALSE, xaxt = "n",
            yaxt = "n", highlight.nodes = NULL, symbol.by = NAs.value,
            symbol.spec = symbol.spec)
        print(plot.res)
        axis(2, las = 1, tick = FALSE, cex.axis = 0.8)
    }

    #################################################

    res.centrality <- function(gr, dat, cent, norm) {

        if (cent %in% c("Bonacich Power", "Eigenvector", "Harary", "Information",
            "Load", "Strees")) {
            switch(cent,
                `Bonacich Power` = bonpow(dat, exponent = 1, tol = 1e-20), #Package:sna
                Eigenvector = evcent(dat),
                Harary = graphcent(dat),
                Information = infocent(dat),
                Load = loadcent(dat),
                Strees = stresscent(dat))
        } else if (norm == FALSE) {
            switch(cent, `In-Degree` = igraph::degree(gr, mode = "in", #Package:igraph
                normalized = FALSE),
            `Out-Degree` = igraph::degree(gr, mode = "out", normalized = FALSE),
            `Total-Degree` = igraph::degree(gr, mode = "total", normalized = FALSE),
            `In-Closeness` = igraph::closeness(gr, mode = "in", normalized = FALSE),
            `Out-Closeness` = igraph::closeness(gr, mode = "out", normalized = FALSE),
            `Total-Closeness` = igraph::closeness(gr, mode = "total", normalized = FALSE),
            Betweenness = igraph::betweenness(gr, normalized = FALSE))
        } else if (norm == TRUE) {
            switch(cent, `In-Degree` = igraph::degree(gr, mode = "in", normalized = TRUE),
            `Out-Degree` = igraph::degree(gr, mode = "out", normalized = TRUE),
                `Total-Degree` = igraph::degree(gr, mode = "total", normalized = TRUE),
                `In-Closeness` = igraph::closeness(gr, mode = "in", normalized = TRUE),
                `Out-Closeness` = igraph::closeness(gr, mode = "out", normalized = TRUE),
                `Total-Closeness` = igraph::closeness(gr, mode = "total", normalized = TRUE),
                Betweenness = igraph::betweenness(gr, normalized = TRUE))
        }
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(centrality_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)

    #################################################

    centrality_window <- gtkWindow(show = FALSE)
    centrality_window$setTransientFor(Window)
    centrality_window$setPosition("center-on-parent")
    centrality_window["title"] <- "Web by centrality"
    centrality_window$setSizeRequest(250, 180)
    centrality_window["border-width"] <- 2
    centrality_window$setModal(TRUE)
    centrality_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    centrality_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$COM.all))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 4, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Color heatmap

    hbox_t1 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Nodes:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_t1$packStart(button, FALSE, FALSE, 0)
    hbox_t1$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_t1, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Type centrality

    type_label <- gtkLabel("Centrality:")

    model_type <- rGtkDataFrame(c("Betweenness", "In-Degree", "Out-Degree",
        "Total-Degree", "In-Closeness", "Out-Closeness", "Total-Closeness",
        "Eigenvector", "Harary", "Information", "Load", "Strees", "Bonacich Power"))
    combobox_type <- gtkComboBox(model_type)
    combobox_type$setSizeRequest(114, 20)
    crt_type <- gtkCellRendererText()
    combobox_type$packStart(crt_type)
    combobox_type$addAttribute(crt_type, "text", 0)
    gtkComboBoxSetActive(combobox_type, 0)

    hbox_type <- gtkHBox()
    hbox_type$packStart(type_label, FALSE, FALSE)
    hbox_type$packStart(combobox_type, FALSE, FALSE)
    opt_table$attach(hbox_type, left.attach = 0, 1, top.attach = 1, 2,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Normalized

    norm_cb <- gtkCheckButton(label = "Normalized")
    hbox_norm <- gtkHBox()
    hbox_norm$packStart(norm_cb, FALSE, FALSE)
    opt_table$attach(hbox_norm, left.attach = 0, 1, top.attach = 2, 3,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Show nodes

    snodes_label <- gtkLabel("Show nodes as:")

    model_snodes <- rGtkDataFrame(c("Numbers", "Points"))
    combobox_snodes <- gtkComboBox(model_snodes)
    combobox_snodes$setSizeRequest(80, 20)
    crt_snodes <- gtkCellRendererText()
    combobox_snodes$packStart(crt_snodes)
    combobox_snodes$addAttribute(crt_snodes, "text", 0)
    gtkComboBoxSetActive(combobox_snodes, 0)

    hbox_snodes <- gtkHBox()
    hbox_snodes$packStart(snodes_label, FALSE, FALSE)
    hbox_snodes$packStart(combobox_snodes, FALSE, FALSE)
    opt_table$attach(hbox_snodes, left.attach = 0, 1, top.attach = 3, 4,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    PlotOk <- function(h, ...) {
        centrality_window["visible"] <- FALSE
        # Data
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        # Show nodes
        iter2 <- gtkComboBoxGetActiveIter(combobox_snodes)$iter
        item2 <- model_snodes$GetValue(iter2, 0)$value
        # Type
        iter3 <- gtkComboBoxGetActiveIter(combobox_type)$iter
        item3 <- model_type$GetValue(iter3, 0)$value

        base1 <- e$dat.all[[item]]
        comm1 <- e$COM.all[[item]]
        gr <- graph_from_adjacency_matrix(as.matrix(base1))
        n.nod <- dim(base1)[2]
        COL1 <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)

        if (item2 == "Numbers") {
            nodes.as <- "labels"
        } else {
            nodes.as <- "points"
        }
        Cent <- res.centrality(gr = gr, dat = base1, cent = item3, norm = norm_cb["active"])
        NAs.value <- rep(1, length(Cent))
        if (any(is.na(Cent))) {
            NAs.value[is.na(Cent) == TRUE] <- 2
            Cent[is.na(Cent) == TRUE] <- 0
        }
        if (norm_cb["active"] == FALSE) {
            ylabel <- item3
        } else {
            ylabel <- paste(item3, "Normalized")
        }

        plot.cent(comm1 = comm1, cent = Cent, ylab = ylabel, NAs.value = NAs.value,
            show.nodes.as = nodes.as, col = COL1, n.nod = n.nod, id.nam = item)
        centrality_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) centrality_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    centrality_window$showAll()

}

#################################################

#' Bipartite
#' @keywords internal

window_plot_bip <- function(h, ...) {

    plot.bip <- function(dat, col.Int, Lz, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(plot.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(bip_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE

        }

        #################################################

        bip_graphic <- gtkWindow(show = TRUE)
        bip_graphic["title"] <- paste("Bipartite:", id.nam)
        bip_graphic$setSizeRequest(500, 400)
        bip_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        bip_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) bip_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        Sys.sleep(0.1)
        plot.res %<a-% bipartite::plotweb(dat, text.rot = 90, col.interaction = col.Int,
            arrow = "down.center", labsize = Lz, ybig = 1.1, y.lim = c(-0.5,
                2.5)) #Package:bipartite
        print(plot.res)

    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(bip_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)

    #################################################

    bip_window <- gtkWindow(show = FALSE)
    bip_window$setTransientFor(Window)
    bip_window$setPosition("center-on-parent")
    bip_window["title"] <- "Bipartite"
    bip_window$setSizeRequest(200, 150)
    bip_window["border-width"] <- 2
    bip_window$setModal(TRUE)
    bip_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    bip_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$datB))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 2, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Color of interaction

    hbox_int <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Color of interaction:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_int$packStart(button, FALSE, FALSE, 0)
    hbox_int$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_int, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size text

    # add a horizontal layout
    hbox_sizeT <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("Size text:")
    sizeT_ent <- gtkEntry()
    sizeT_ent$setSizeRequest(20, 20)
    sizeT_ent$insertText(1)
    gSignalConnect(sizeT_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_sizeT$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_sizeT$packStart(sizeT_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_sizeT, left.attach = 0, 1, top.attach = 1, 2)

    #################################################

    PlotOk <- function(h, ...) {
        bip_window["visible"] <- FALSE
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        dat <- e$datB[[item]]
        col.Int <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)

        plot.bip(dat = dat, col.Int = col.Int, Lz = as.numeric(sizeT_ent$getText()),
            id.nam = item)
        bip_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) bip_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    bip_window$showAll()

}

#################################################

#' Module-Bipartite
#' @keywords internal

window_plot_module_bip <- function(h, ...) {

  plot.module.bip <- function(id.Bip, type.nodes, type.plot, col.grid,
                              id.nam, ...) {

    #################################################

    save.plot <- function(h, ...) {

      #################################################

      ok_func <- function(h, ...) {
        name_save <- name_ent$getText()
        folder_save <- folder_button$getFilename()
        if (nchar(name_save) > 1 & file.exists(folder_save)) {
          sav_window["visible"] <- FALSE
          iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
          item <- model_format$GetValue(iter, 0)$value
          if (item == "tiff") {
            nam <- paste(folder_save, "\\", name_save, ".tiff",
                         sep = "")
          }
          if (item == "jpeg") {
            nam <- paste(folder_save, "\\", name_save, ".jpg",
                         sep = "")
          }
          if (item == "png") {
            nam <- paste(folder_save, "\\", name_save, ".png",
                         sep = "")
          }
          if (item == "eps") {
            nam <- paste(folder_save, "\\", name_save, ".eps",
                         sep = "")
          }

          Format.plot(nam <- nam, type = item)
          print(plot.mod)
          dev.off()
          sav_window$destroy()
        }
      }

      #################################################

      sav_window <- gtkWindow(show = FALSE)
      sav_window$setTransientFor(module_graphic)
      sav_window$setPosition("center-on-parent")
      sav_window["title"] <- "Save plot ..."
      sav_window$setSizeRequest(320, 95)
      sav_window["border-width"] <- 2
      sav_window$setModal(TRUE)
      sav_window$setResizable(FALSE)

      tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
      sav_window$add(tab_sav)

      name_label <- gtkLabel("Name:")
      name_ent <- gtkEntry()
      name_ent$modifyText(GtkStateType["normal"], "blue")
      name_ent$insertText("Plot")
      gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

      hbox_sav1 <- gtkHBox(FALSE, 1)
      hbox_sav1$packStart(name_label, FALSE, FALSE)
      hbox_sav1$packStart(name_ent, TRUE, TRUE)

      folder_label <- gtkLabel("Folder:")
      folder_button <- gtkFileChooserButton("Select a file", "select-folder")

      hbox_sav2 <- gtkHBox(FALSE, 1)
      hbox_sav2$packStart(folder_label, FALSE, FALSE)
      hbox_sav2$packStart(folder_button, TRUE, TRUE)

      ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
      gSignalConnect(ok_button2, "clicked", f = ok_func)
      ok_button2$setSizeRequest(70, 25)
      cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
      gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
      cancel_button2$setSizeRequest(70, 25)

      hbox_sav3 <- gtkHBox(FALSE, 1)
      hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
      hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

      tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                     1, xoptions = c("fill"))
      tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                     2, xoptions = c("fill"), yoptions = c("shrink"))
      tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                     3, yoptions = c("shrink"))

      sav_window["visible"] <- TRUE

    }

    #################################################

    module_graphic <- gtkWindow(show = TRUE)
    module_graphic["title"] <- paste("Modularity:", id.nam)
    module_graphic$setSizeRequest(500, 400)
    module_graphic["border-width"] <- 0

    vbox1 <- gtkVBox(FALSE, 5)
    module_graphic$add(vbox1)

    format_label <- gtkLabel("Format:")
    model_format <- rGtkDataFrame(c("tiff", "jpeg", "png"))
    combobox_format <- gtkComboBox(model_format)
    combobox_format$setSizeRequest(55, 20)
    crt_format <- gtkCellRendererText()
    combobox_format$packStart(crt_format)
    combobox_format$addAttribute(crt_format, "text", 0)
    gtkComboBoxSetActive(combobox_format, 0)
    save_button <- gtkButton("Save", stock.id = "gtk-save")
    gSignalConnect(save_button, "clicked", f = save.plot)
    save_button$setSizeRequest(70, 25)
    quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
    gSignalConnect(quit_button, "clicked", f = function(h, ...) module_graphic$destroy())
    quit_button$setSizeRequest(70, 25)

    hbox1 <- gtkHBox(FALSE, 5)
    hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
    hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
    hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
    hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
    vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

    ### gtk-container-scrolled-device
    dev <- gtkDrawingArea()
    dev$setSizeRequest(1000, 500)
    dev$AddEvents(GdkEventMask["all-events-mask"])
    asCairoDevice(dev)

    ### gtk-container-scrolled-construct
    scroll <- gtkScrolledWindow()
    scroll$addWithViewport(dev)

    ### gtk-container-scrolled-key-press
    gSignalConnect(scroll, "key-press-event", function(scroll, event) {
      key <- event[["keyval"]]
      if (key == GDK_plus)
        zoomPlot(2) else if (key == GDK_minus)
          zoomPlot(0.5)
      TRUE
    })

    ### gtk-container-scrolled-window
    vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

    ############################################

    Sys.sleep(0.1)

    ### print plot
    Bip <- e$datB[[e$Modules.all[[id.Bip]][[3]]]]

    col.zero <- c(1:ncol(Bip))[apply(Bip, 2, sum) == 0]
    row.zero <- c(1:nrow(Bip))[apply(Bip, 1, sum) == 0]
    if (length(col.zero) > 0) {
      Bip <- Bip[, -c(col.zero)]
    }
    if (length(row.zero) > 0) {
      Bip <- Bip[-c(row.zero), ]
    }

    Mod.orderA <- e$Modules.all[[id.Bip]][[4]]
    Mod.orderB <- e$Modules.all[[id.Bip]][[5]]

    Modules.Row <- e$Modules.all[[id.Bip]][[1]]
    Modules.Col <- e$Modules.all[[id.Bip]][[2]]
    n.Modules <- max(Modules.Col[, 2])

    if (type.plot == "Heatmap") {

      Mat <- as.matrix(Bip)
      Mat[Mat == 0] <- NA
      dat.melt <- melt(Mat)
      x <- seq(0.2, 1, length.out = 10)
      x.cols <- seq_gradient_pal("white", col.grid)(x)
      cols <- c(x.cols[1], x.cols[10])

      xy <- matrix(0, ncol = 4, nrow = n.Modules, dimnames = list(NULL,
                                                                  c("xmin", "xmax", "ymin", "ymax")))
      xy <- as.data.frame(xy)
      for (i in 1:nrow(xy)) {
        x <- Modules.Col[, 1][Modules.Col[, 2] == i]
        y <- rev(Modules.Row[, 1][Modules.Row[, 2] == i])
        xy[i, 1] <- which(Modules.Col[, 1] == x[1]) - 0.5
        xy[i, 2] <- which(Modules.Col[, 1] == x[length(x)]) + 0.5
        xy[i, 3] <- which(rev(Modules.Row[, 1]) == y[1]) - 0.5
        xy[i, 4] <- which(rev(Modules.Row[, 1]) == y[length(y)]) +
          0.5
      }

      geom.1 <- geom_rect(data = xy, aes(xmin = xmin, xmax = xmax,
                                         ymin = ymin, ymax = ymax), fill = NA, col = "black", inherit.aes = F)

      plot.res <- ggplot(data = dat.melt, aes(x = X2, y = X1, fill = value)) +
        geom_tile(color = "white") + scale_fill_gradientn(colours = cols,
                                                          na.value = "white") + guides(fill = FALSE) + labs(x = NULL,
                                                                                                            y = NULL) + xlim(colnames(Mat)[Mod.orderB]) + ylim(rev(rownames(Mat)[Mod.orderA])) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "white", colour = "black"))

      theme.1 <- theme(axis.text.x = element_text(family = "Serif",
                                                  angle = 90, hjust = 1, vjust = 0.5, colour = "black", size = 10),
                       axis.text.y = element_text(colour = "black", size = 10),
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_rect(fill = "white", colour = "black"))

      if (type.nodes == "Text") {
        # Nodes text
        plot.mod <- plot.res + theme.1 + geom.1

      } else if (type.nodes == "Null") {
        # Null text
        theme.2 <- theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
                         axis.ticks = element_blank())
        plot.mod <- plot.res + theme.2 + geom.1

      } else if (type.nodes == "Id") {
        # Id text
        dat.melt2 <- dat.melt
        dat.melt2[, "X1"] <- as.character(match(dat.melt2$X1, rownames(Bip)))
        dat.melt2[, "X2"] <- as.character(match(dat.melt2$X2, colnames(Bip)))

        plot.mod <- ggplot(data = dat.melt2, aes(x = X2, y = X1,
                                                 fill = value)) + geom_tile(color = "white") + scale_fill_gradientn(colours = cols,
                                                                                                                    na.value = "white") + guides(fill = FALSE) + labs(x = NULL,
                                                                                                                                                                      y = NULL) + xlim(as.character(Mod.orderB)) + ylim(as.character(rev(Mod.orderA))) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black")) +
          theme.1 + geom.1

      }

    } else if (type.plot == "Network") {

      Bip2 <- as.matrix(Bip)
      Bip2 <- Bip2[, c(Mod.orderB)]
      Bip2 <- Bip2[c(Mod.orderA), ]


      # Incidence matrix
      gnet <- graph_from_incidence_matrix(as.matrix(Bip2))  #Package: igraph
      gr_bip <- gnet %>% get.edgelist %>% data.frame
      gr_bip <- data.frame(gr_bip, color = rep(1, dim(gr_bip)[1]))

      # Modules
      for (i in 1:n.Modules) {
        x1 <- as.character(Modules.Col[, 1][Modules.Col[, 2] ==
                                              i])
        x2 <- as.character(Modules.Row[, 1][Modules.Row[, 2] ==
                                              i])
        mod.length <- nrow(gr_bip[gr_bip[, 1] %in% x2 & gr_bip[,
                                                               2] %in% x1, ])
        gr_bip[gr_bip[, 1] %in% x2 & gr_bip[, 2] %in% x1, ][3] <- rep(i +
                                                                        1, mod.length)
      }

      # Parameters
      gnet <- gnet %>% set_edge_attr("color", value = gr_bip[, 3])
      pal.col <- c("#000000", rainbow(n.Modules))
      Nod.Col <- "black"
      Nod.Size <- 3
      Text.Col <- "black"
      Text.Size <- 3

      # Layout
      y.coord <- c(rep(1, dim(Bip2)[1]), rep(0, dim(Bip2)[2]))
      x.coord <- c(seq(0, 1, length.out = dim(Bip2)[1]), seq(0, 1,
                                                             length.out = dim(Bip2)[2]))
      lay <- create_layout(gnet, layout = "kk")  #Package: ggraph
      lay[, 1] <- x.coord
      lay[, 2] <- y.coord
      lay[, 8] <- c(rownames(Bip2), rep(NA, length(colnames(Bip2))))
      lay[, 9] <- c(rep(NA, length(rownames(Bip2))), colnames(Bip2))
      lay[, 10] <- c(Modules.Row[, 2], Modules.Col[, 2])
      lay[, 11] <- c(paste("A", 1:length(rownames(Bip2)), sep = ""),
                     rep(NA, length(colnames(Bip2))))
      lay[, 12] <- c(rep(NA, length(rownames(Bip2))), paste("B",
                                                            1:length(colnames(Bip2)), sep = ""))
      names(lay)[c(8, 9, 10, 11, 12)] <- c("name.row", "name.col",
                                           "Module", "id.row", "id.col")

      # Graph, Package:ggraph
      gg.res <- ggraph(gnet, "manual", node.position = lay) + geom_node_point(aes(color = factor(lay$Module)),
                                                                              show.legend = F, size = Nod.Size) + theme_graph(base_family = "Serif",
                                                                                                                              background = NA, foreground = NA, border = FALSE) + geom_edge_link(aes(edge_colour = as.factor(color)),
                                                                                                                                                                                                 arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                                                                                                                                                                                                 end_cap = circle(2, "mm"), show.legend = FALSE) + scale_edge_colour_manual(values = pal.col)

      if (type.nodes == "Null") {
        # Null text
        plot.mod <- gg.res

      } else if (type.nodes == "Id") {
        # Id text
        plot.mod <- gg.res + geom_node_text(aes(label = lay$id.col),
                                            color = Text.Col, size = Text.Size, nudge_x = 0, nudge_y = -0.02,
                                            hjust = TRUE, angle = 90) + geom_node_text(aes(label = lay$id.row),
                                                                                       color = Text.Col, size = Text.Size, nudge_x = 0, nudge_y = 0.02,
                                                                                       hjust = FALSE, angle = 90)

      } else if (type.nodes == "Text") {
        # Nodes text
        plot.mod <- gg.res + expand_limits(y = c(-0.5, 1.5)) +
          geom_node_text(aes(label = lay$name.col), color = Text.Col,
                         size = Text.Size, nudge_x = 0, nudge_y = -0.02, hjust = TRUE,
                         angle = 90) + geom_node_text(aes(label = lay$name.row),
                                                      color = Text.Col, size = Text.Size, nudge_x = 0, nudge_y = 0.02,
                                                      hjust = FALSE, angle = 90)

      }
    }
    print(plot.mod)
  }

  #################################################

  # Select color

  color_func1 <- function(h, ...) {
    dialog <- gtkColorSelectionDialogNew("Select color", show = F)
    dialog$setTransientFor(module_window)
    dialog$setPosition("center-on-parent")
    dialog$setModal(TRUE)
    select <- dialog$getColorSelection()
    select$setHasPalette(TRUE)
    response <- dialog$run()
    if (response == GtkResponseType["ok"]) {
      color <- select$getCurrentColor()$color
      e$color$red <- color[["red"]]
      e$color$green <- color[["green"]]
      e$color$blue <- color[["blue"]]
      drawing_area$modifyBg("normal", e$color)
    }
    dialog$destroy()
  }

  e$color <- list(red = 0, green = 0, blue = 65535)

  #################################################

  module_window <- gtkWindow(show = FALSE)
  module_window$setTransientFor(Window)
  module_window$setPosition("center-on-parent")
  module_window["title"] <- "Modularity"
  module_window$setSizeRequest(200, 160)
  module_window["border-width"] <- 2
  module_window$setModal(TRUE)
  module_window$setResizable(FALSE)

  #################################################

  vbox_win <- gtkVBox(FALSE, 5)
  module_window$add(vbox_win)

  hbox_data <- gtkHBox()
  data_label <- gtkLabel("Data:")
  model_data <- rGtkDataFrame(names(e$Modules.all))
  combobox_data <- gtkComboBox(model_data)
  combobox_data$setSizeRequest(114, 20)
  crt_data <- gtkCellRendererText()
  combobox_data$packStart(crt_data)
  combobox_data$addAttribute(crt_data, "text", 0)
  hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
  hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
  opt_frame <- gtkFrame("Options")
  vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
  vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
  opt_table <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
  opt_frame$add(opt_table)

  #################################################

  # Type plot

  plot_label <- gtkLabel("Plot:")

  model_plot <- rGtkDataFrame(c("Network", "Heatmap"))
  combobox_plot <- gtkComboBox(model_plot)
  combobox_plot$setSizeRequest(120, 20)
  crt_plot <- gtkCellRendererText()
  combobox_plot$packStart(crt_plot)
  combobox_plot$addAttribute(crt_plot, "text", 0)
  gtkComboBoxSetActive(combobox_plot, 0)

  hbox_plot <- gtkHBox(FALSE, 5)
  hbox_plot$packStart(plot_label, FALSE, FALSE)
  hbox_plot$packStart(combobox_plot, FALSE, FALSE)
  opt_table$attach(hbox_plot, left.attach = 0, 1, top.attach = 0, 1,
                   xoptions = c("fill"), yoptions = c("shrink"))

  #################################################

  # Color heatmap

  hbox_hm <- gtkHBox(FALSE, 5)

  fr_color <- gtkFrame()
  drawing_area <- gtkDrawingArea()
  drawing_area$setSizeRequest(25, 20)
  drawing_area$modifyBg("normal", e$color)
  fr_color$add(drawing_area)

  button <- gtkButtonNewWithMnemonic("_Heatmap color:")
  gSignalConnect(button, "clicked", f = color_func1)

  hbox_hm$packStart(button, FALSE, FALSE, 0)
  hbox_hm$packStart(fr_color, FALSE, FALSE, 0)
  opt_table$attach(hbox_hm, left.attach = 0, 1, top.attach = 1, 2, xoptions = c("fill"),
                   yoptions = c("shrink"))

  #################################################

  # Type nodes

  nodes_label <- gtkLabel("Nodes:")

  model_nodes <- rGtkDataFrame(c("Text", "Id", "Null"))
  combobox_nodes <- gtkComboBox(model_nodes)
  combobox_nodes$setSizeRequest(100, 20)
  crt_nodes <- gtkCellRendererText()
  combobox_nodes$packStart(crt_nodes)
  combobox_nodes$addAttribute(crt_nodes, "text", 0)
  gtkComboBoxSetActive(combobox_nodes, 0)

  hbox_nodes <- gtkHBox(FALSE, 5)
  hbox_nodes$packStart(nodes_label, FALSE, FALSE)
  hbox_nodes$packStart(combobox_nodes, FALSE, FALSE)
  opt_table$attach(hbox_nodes, left.attach = 0, 1, top.attach = 2, 3,
                   xoptions = c("fill"), yoptions = c("shrink"))

  #################################################

  PlotOk <- function(h, ...) {
    module_window["visible"] <- FALSE
    # Data
    iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
    item <- model_data$GetValue(iter, 0)$value
    # Type plot
    iter2 <- gtkComboBoxGetActiveIter(combobox_plot)$iter
    item2 <- model_plot$GetValue(iter2, 0)$value
    # Type nodes
    iter3 <- gtkComboBoxGetActiveIter(combobox_nodes)$iter
    item3 <- model_nodes$GetValue(iter3, 0)$value

    dat <- e$datB[[item]]
    col.grid <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)

    plot.module.bip(id.Bip = item, type.nodes = item3, type.plot = item2,
                    col.grid = col.grid, id.nam = item)
    module_window$destroy()
  }

  #################################################

  ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
  gSignalConnect(ok_button, "clicked", f = PlotOk)
  ok_button$setSizeRequest(64, 25)
  cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
  gSignalConnect(cancel_button, "clicked", f = function(h, ...) module_window$destroy())
  cancel_button$setSizeRequest(64, 25)

  hbox_buttons <- gtkHBox(FALSE, 5)
  hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
  hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
  vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
  gtkComboBoxSetActive(combobox_data, 0)
  module_window$showAll()

}

#################################################

#' Spanning Tree
#' @keywords internal

window_plot_span <- function(h, ...) {

    plot.span <- function(gnet, colN, colT, zN, zT, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(gg.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(span_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE
        }

        #################################################

        span_graphic <- gtkWindow(show = TRUE)
        span_graphic["title"] <- paste("Spanning Tree:", id.nam)
        span_graphic$setSizeRequest(500, 400)
        span_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        span_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) span_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        ############################################
        Sys.sleep(0.1)
        Nod.Col <- colN
        Nod.Size <- zN
        Text.Col <- colT
        Text.Size <- zT
        gg.res <- ggraph(gnet, layout = "kk") + geom_edge_link(arrow = arrow(type = "closed", #Package: ggraph
          length = unit(1.5, "mm")), end_cap = circle(2, "mm")) + geom_node_point(color = Nod.Col,
          size = Nod.Size) + geom_node_text(aes(label = name), color = Text.Col,
          size = Text.Size) + theme_graph() + theme_graph(base_family = "Serif",
          background = NA, foreground = NA, border = FALSE)
        print(gg.res)
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(span_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    color_func2 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(span_window)
        dialog$setPosition("center-on-parent")
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color2$red <- color[["red"]]
            e$color2$green <- color[["green"]]
            e$color2$blue <- color[["blue"]]
            drawing_area2$modifyBg("normal", e$color2)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)
    e$color2 <- list(red = 0, green = 0, blue = 65535)

    #################################################

    span_window <- gtkWindow(show = FALSE)
    span_window$setTransientFor(Window)
    span_window$setPosition("center-on-parent")
    span_window["title"] <- "Spanning Tree"
    span_window$setSizeRequest(220, 150)
    span_window["border-width"] <- 2
    span_window$setModal(TRUE)
    span_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    span_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$COM.all))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 2, columns = 2, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Color nodes

    hbox_t1 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Nodes:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_t1$packStart(button, FALSE, FALSE, 0)
    hbox_t1$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_t1, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size nodes

    # add a horizontal layout
    hbox_t2 <- gtkHBox(FALSE, 5)

    sizeN_label <- gtkLabel("Size node:")
    sizeN_ent <- gtkEntry()
    sizeN_ent$setSizeRequest(20, 20)
    sizeN_ent$insertText(5)
    gSignalConnect(sizeN_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_t2$packStart(sizeN_label, FALSE, FALSE, 0)
    hbox_t2$packStart(sizeN_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_t2, left.attach = 0, 1, top.attach = 1, 2)

    #################################################

    # Color text

    hbox_t3 <- gtkHBoxNew(FALSE, 5)

    fr_color2 <- gtkFrame()
    drawing_area2 <- gtkDrawingArea()
    drawing_area2$setSizeRequest(25, 20)
    drawing_area2$modifyBg("normal", e$color2)
    fr_color2$add(drawing_area2)

    button2 <- gtkButtonNewWithMnemonic("_Text:")
    gSignalConnect(button2, "clicked", color_func2)

    hbox_t3$packStart(button2, FALSE, FALSE, 0)
    hbox_t3$packStart(fr_color2, FALSE, FALSE, 0)
    opt_table$attach(hbox_t3, left.attach = 1, 2, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size text

    # add a horizontal layout
    hbox_t4 <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("Size text:")
    sizeT_ent <- gtkEntry()
    sizeT_ent$setSizeRequest(20, 20)
    sizeT_ent$insertText(5)
    gSignalConnect(sizeT_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_t4$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_t4$packStart(sizeT_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_t4, left.attach = 1, 2, top.attach = 1, 2)

    #################################################

    PlotOk <- function(h, ...) {
      span_window["visible"] <- FALSE
      iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
      item <- model_data$GetValue(iter, 0)$value
      base1 <- e$dat.all[[item]]
      gr <- graph_from_adjacency_matrix(as.matrix(base1))
      graph_mst <- mst(gr) #Package: igraph
      colN <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)
      colT <- rgb(e$color2$red, e$color2$green, e$color2$blue, maxColorValue = 65535)

      plot.span(gnet = graph_mst, colN = colN, colT = colT, zN = as.numeric(sizeN_ent$getText()),
                zT = as.numeric(sizeT_ent$getText()), id.nam = item)
      span_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) span_window)
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    span_window$showAll()

    #################################################

}

#################################################

#' Level
#' @keywords internal

window_plot_level <- function(h, ...) {

    plot.level <- function(COM, YL, LEV, MAIN, show.nodes.as, COL, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(plot.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(level_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE
        }

        #################################################

        level_graphic <- gtkWindow(show = TRUE)
        level_graphic["title"] <- paste("Web by level:", id.nam)
        level_graphic$setSizeRequest(500, 400)
        level_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        level_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) level_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        ############################################

        Sys.sleep(0.1)
        plot.res %<a-% PlotWebByLevel(COM, ylim = c(1, YL), level = LEV, #Package:cheddar
            main = MAIN, pch = 19, col = COL, show.nodes.as = show.nodes.as,
            highlight.nodes = NULL)
        print(plot.res)
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(level_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)

    #################################################

    level_window <- gtkWindow(show = FALSE)
    level_window$setTransientFor(Window)
    level_window$setPosition("center-on-parent")
    level_window["title"] <- "Web by level"
    level_window$setSizeRequest(250, 200)
    level_window["border-width"] <- 2
    level_window$setModal(TRUE)
    level_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    level_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$COM.all))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 4, columns = 1, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Color heatmap

    hbox_t1 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Color of interaction:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_t1$packStart(button, FALSE, FALSE, 0)
    hbox_t1$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_t1, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Level

    level_label <- gtkLabel("Level:")

    model_level <- rGtkDataFrame(c("ShortestTrophicLevel", "ShortWeightedTrophicLevel",
        "LongestTrophicLevel", "LongWeightedTrophicLevel", "ChainAveragedTrophicLevel",
        "PreyAveragedTrophicLevel"))
    combobox_level <- gtkComboBox(model_level)
    combobox_level$setSizeRequest(177, 20)
    crt_level <- gtkCellRendererText()
    combobox_level$packStart(crt_level)
    combobox_level$addAttribute(crt_level, "text", 0)
    gtkComboBoxSetActive(combobox_level, 0)

    hbox_level <- gtkHBox(FALSE, 5)
    hbox_level$packStart(level_label, FALSE, FALSE)
    hbox_level$packStart(combobox_level, FALSE, FALSE)
    opt_table$attach(hbox_level, left.attach = 0, 1, top.attach = 1, 2,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Show nodes

    snodes_label <- gtkLabel("Show nodes as:")

    model_snodes <- rGtkDataFrame(c("Numbers", "Points"))
    combobox_snodes <- gtkComboBox(model_snodes)
    combobox_snodes$setSizeRequest(80, 20)
    crt_snodes <- gtkCellRendererText()
    combobox_snodes$packStart(crt_snodes)
    combobox_snodes$addAttribute(crt_snodes, "text", 0)
    gtkComboBoxSetActive(combobox_snodes, 0)

    hbox_snodes <- gtkHBox(FALSE, 5)
    hbox_snodes$packStart(snodes_label, FALSE, FALSE)
    hbox_snodes$packStart(combobox_snodes, FALSE, FALSE)
    opt_table$attach(hbox_snodes, left.attach = 0, 1, top.attach = 2, 3,
        xoptions = c("fill"), yoptions = c("shrink"))

    #################################################

    # Limit

    limit_label <- gtkLabel("Limit of the y axis:")
    limit_ent <- gtkEntry()
    limit_ent$setSizeRequest(20, 20)
    limit_ent$insertText(5.8)
    gSignalConnect(limit_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_limit <- gtkHBox(FALSE, 5)
    hbox_limit$packStart(limit_label, FALSE, FALSE, 0)
    hbox_limit$packStart(limit_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_limit, left.attach = 0, 1, top.attach = 3, 4)

    #################################################

    PlotOk <- function(h, ...) {
        level_window["visible"] <- FALSE
        # Data
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        # Level
        iter2 <- gtkComboBoxGetActiveIter(combobox_level)$iter
        item2 <- model_level$GetValue(iter2, 0)$value
        # Show nodes
        iter3 <- gtkComboBoxGetActiveIter(combobox_snodes)$iter
        item3 <- model_snodes$GetValue(iter3, 0)$value

        COM <- e$COM.all[[item]]
        COL1 <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)
        if (item3 == "Numbers") {
            nodes.as <- "labels"
        } else {
            nodes.as <- "points"
        }

        plot.level(COM = COM, YL = as.numeric(limit_ent$getText()), LEV = item2,
            MAIN = item2, show.nodes.as = nodes.as, COL = COL1, id.nam = item)
        level_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) level_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    level_window$showAll()

}

#################################################

#' Network
#' @keywords internal

window_plot_net <- function(h, ...) {

    plot.net <- function(gnet, colN, colT, zN, zT, id.nam) {

        #################################################

        save.plot <- function(h, ...) {

            #################################################

            ok_func <- function(h, ...) {
                name_save <- name_ent$getText()
                folder_save <- folder_button$getFilename()
                if (nchar(name_save) > 1 & file.exists(folder_save)) {
                  sav_window["visible"] <- FALSE
                  iter <- gtkComboBoxGetActiveIter(combobox_format)$iter
                  item <- model_format$GetValue(iter, 0)$value
                  if (item == "tiff") {
                    nam <- paste(folder_save, "\\", name_save, ".tiff",
                      sep = "")
                  }
                  if (item == "jpeg") {
                    nam <- paste(folder_save, "\\", name_save, ".jpg",
                      sep = "")
                  }
                  if (item == "png") {
                    nam <- paste(folder_save, "\\", name_save, ".png",
                      sep = "")
                  }
                  if (item == "eps") {
                    nam <- paste(folder_save, "\\", name_save, ".eps",
                      sep = "")
                  }

                  Format.plot(nam <- nam, type = item)
                  print(gg.res)
                  dev.off()
                  sav_window$destroy()
                }
            }

            #################################################

            sav_window <- gtkWindow(show = FALSE)
            sav_window$setTransientFor(net_graphic)
            sav_window$setPosition("center-on-parent")
            sav_window["title"] <- "Save plot ..."
            sav_window$setSizeRequest(320, 95)
            sav_window["border-width"] <- 2
            sav_window$setModal(TRUE)
            sav_window$setResizable(FALSE)

            tab_sav <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
            sav_window$add(tab_sav)

            name_label <- gtkLabel("Name:")
            name_ent <- gtkEntry()
            name_ent$modifyText(GtkStateType["normal"], "blue")
            name_ent$insertText("Plot")
            gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)

            hbox_sav1 <- gtkHBox(FALSE, 1)
            hbox_sav1$packStart(name_label, FALSE, FALSE)
            hbox_sav1$packStart(name_ent, TRUE, TRUE)

            folder_label <- gtkLabel("Folder:")
            folder_button <- gtkFileChooserButton("Select a file", "select-folder")

            hbox_sav2 <- gtkHBox(FALSE, 1)
            hbox_sav2$packStart(folder_label, FALSE, FALSE)
            hbox_sav2$packStart(folder_button, TRUE, TRUE)

            ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
            gSignalConnect(ok_button2, "clicked", f = ok_func)
            ok_button2$setSizeRequest(70, 25)
            cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
            gSignalConnect(cancel_button2, "clicked", f = function(h, ...) sav_window$destroy())
            cancel_button2$setSizeRequest(70, 25)

            hbox_sav3 <- gtkHBox(FALSE, 1)
            hbox_sav3$packEnd(cancel_button2, FALSE, TRUE)
            hbox_sav3$packEnd(ok_button2, FALSE, TRUE)

            tab_sav$attach(hbox_sav1, left.attach = 0, 1, top.attach = 0,
                1, xoptions = c("fill"))
            tab_sav$attach(hbox_sav2, left.attach = 0, 1, top.attach = 1,
                2, xoptions = c("fill"), yoptions = c("shrink"))
            tab_sav$attach(hbox_sav3, left.attach = 0, 1, top.attach = 2,
                3, yoptions = c("shrink"))

            sav_window["visible"] <- TRUE

        }

        #################################################

        net_graphic <- gtkWindow(show = TRUE)
        net_graphic["title"] <- paste("Network:", id.nam)
        net_graphic$setSizeRequest(500, 400)
        net_graphic["border-width"] <- 0

        vbox1 <- gtkVBox(FALSE, 5)
        net_graphic$add(vbox1)

        format_label <- gtkLabel("Format:")
        model_format <- rGtkDataFrame(c("tiff", "jpeg", "png", "eps"))
        combobox_format <- gtkComboBox(model_format)
        combobox_format$setSizeRequest(55, 20)
        crt_format <- gtkCellRendererText()
        combobox_format$packStart(crt_format)
        combobox_format$addAttribute(crt_format, "text", 0)
        gtkComboBoxSetActive(combobox_format, 0)
        save_button <- gtkButton("Save", stock.id = "gtk-save")
        gSignalConnect(save_button, "clicked", f = save.plot)
        save_button$setSizeRequest(70, 25)
        quit_button <- gtkButton("Quit", stock.id = "gtk-quit")
        gSignalConnect(quit_button, "clicked", f = function(h, ...) net_graphic$destroy())
        quit_button$setSizeRequest(70, 25)

        hbox1 <- gtkHBox(FALSE, 5)
        hbox1$packStart(format_label, expand = FALSE, fill = FALSE)
        hbox1$packStart(combobox_format, expand = FALSE, fill = FALSE)
        hbox1$packStart(save_button, expand = FALSE, fill = FALSE)
        hbox1$packStart(quit_button, expand = FALSE, fill = FALSE)
        vbox1$packStart(hbox1, expand = FALSE, fill = FALSE)

        ### gtk-container-scrolled-device
        dev <- gtkDrawingArea()
        dev$setSizeRequest(1000, 500)
        dev$AddEvents(GdkEventMask["all-events-mask"])
        asCairoDevice(dev)

        ### gtk-container-scrolled-construct
        scroll <- gtkScrolledWindow()
        scroll$addWithViewport(dev)

        ### gtk-container-scrolled-key-press
        gSignalConnect(scroll, "key-press-event", function(scroll, event) {
            key <- event[["keyval"]]
            if (key == GDK_plus)
                zoomPlot(2) else if (key == GDK_minus)
                zoomPlot(0.5)
            TRUE
        })

        ### gtk-container-scrolled-window
        vbox1$packStart(scroll, expand = TRUE, fill = TRUE)

        Sys.sleep(0.1)
        Nod.Col <- colN
        Nod.Size <- zN
        Text.Col <- colT
        Text.Size <- zT
        gg.res <- ggraph(gnet, layout = "kk") + geom_edge_link(arrow = arrow(type = "closed", #Package: ggraph
          length = unit(1.5, "mm")), end_cap = circle(2, "mm")) + geom_node_point(color = Nod.Col,
          size = Nod.Size) + geom_node_text(aes(label = name), color = Text.Col,
          size = Text.Size) + theme_graph() + theme_graph(base_family = "Serif",
          background = NA, foreground = NA, border = FALSE)
        print(gg.res)
    }

    #################################################

    # Select color

    color_func1 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(net_window)
        dialog$setPosition("center-on-parent")
        dialog$setModal(TRUE)
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color$red <- color[["red"]]
            e$color$green <- color[["green"]]
            e$color$blue <- color[["blue"]]
            drawing_area$modifyBg("normal", e$color)
        }
        dialog$destroy()
    }

    color_func2 <- function(h, ...) {
        dialog <- gtkColorSelectionDialogNew("Select color", show = F)
        dialog$setTransientFor(net_window)
        dialog$setPosition("center-on-parent")
        select <- dialog$getColorSelection()
        select$setHasPalette(TRUE)
        response <- dialog$run()
        if (response == GtkResponseType["ok"]) {
            color <- select$getCurrentColor()$color
            e$color2$red <- color[["red"]]
            e$color2$green <- color[["green"]]
            e$color2$blue <- color[["blue"]]
            drawing_area2$modifyBg("normal", e$color2)
        }
        dialog$destroy()
    }

    e$color <- list(red = 0, green = 0, blue = 65535)
    e$color2 <- list(red = 0, green = 0, blue = 65535)

    #################################################

    net_window <- gtkWindow(show = FALSE)
    net_window$setTransientFor(Window)
    net_window$setPosition("center-on-parent")
    net_window["title"] <- "Network"
    net_window$setSizeRequest(200, 150)
    net_window["border-width"] <- 2
    net_window$setModal(TRUE)
    net_window$setResizable(FALSE)

    #################################################

    vbox_win <- gtkVBox(FALSE, 5)
    net_window$add(vbox_win)

    hbox_data <- gtkHBox()
    data_label <- gtkLabel("Data:")
    model_data <- rGtkDataFrame(names(e$mod.all))
    combobox_data <- gtkComboBox(model_data)
    combobox_data$setSizeRequest(114, 20)
    crt_data <- gtkCellRendererText()
    combobox_data$packStart(crt_data)
    combobox_data$addAttribute(crt_data, "text", 0)
    hbox_data$packStart(data_label, expand = FALSE, fill = FALSE)
    hbox_data$packStart(combobox_data, expand = FALSE, fill = FALSE)
    opt_frame <- gtkFrame("Options")
    vbox_win$packStart(hbox_data, expand = FALSE, fill = FALSE)
    vbox_win$packStart(opt_frame, expand = TRUE, fill = TRUE)
    opt_table <- gtkTable(rows = 2, columns = 2, homogeneous = TRUE)
    opt_frame$add(opt_table)

    #################################################

    # Color nodes

    hbox_t1 <- gtkHBox(FALSE, 5)

    fr_color <- gtkFrame()
    drawing_area <- gtkDrawingArea()
    drawing_area$setSizeRequest(25, 20)
    drawing_area$modifyBg("normal", e$color)
    fr_color$add(drawing_area)

    button <- gtkButtonNewWithMnemonic("_Nodes:")
    gSignalConnect(button, "clicked", f = color_func1)

    hbox_t1$packStart(button, FALSE, FALSE, 0)
    hbox_t1$packStart(fr_color, FALSE, FALSE, 0)
    opt_table$attach(hbox_t1, left.attach = 0, 1, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size nodes

    # add a horizontal layout
    hbox_t2 <- gtkHBox(FALSE, 5)

    sizeN_label <- gtkLabel("Size node:")
    sizeN_ent <- gtkEntry()
    sizeN_ent$setSizeRequest(20, 20)
    sizeN_ent$insertText(5)
    gSignalConnect(sizeN_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_t2$packStart(sizeN_label, FALSE, FALSE, 0)
    hbox_t2$packStart(sizeN_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_t2, left.attach = 0, 1, top.attach = 1, 2)

    #################################################

    # Color text

    hbox_t3 <- gtkHBoxNew(FALSE, 5)

    fr_color2 <- gtkFrame()
    drawing_area2 <- gtkDrawingArea()
    drawing_area2$setSizeRequest(25, 20)
    drawing_area2$modifyBg("normal", e$color2)
    fr_color2$add(drawing_area2)

    button2 <- gtkButtonNewWithMnemonic("_Text:")
    gSignalConnect(button2, "clicked", color_func2)

    hbox_t3$packStart(button2, FALSE, FALSE, 0)
    hbox_t3$packStart(fr_color2, FALSE, FALSE, 0)
    opt_table$attach(hbox_t3, left.attach = 1, 2, top.attach = 0, 1, xoptions = c("fill"),
        yoptions = c("shrink"))

    #################################################

    # Size text

    # add a horizontal layout
    hbox_t4 <- gtkHBox(FALSE, 5)

    sizeT_label <- gtkLabel("Size text:")
    sizeT_ent <- gtkEntry()
    sizeT_ent$setSizeRequest(20, 20)
    sizeT_ent$insertText(5)
    gSignalConnect(sizeT_ent, "insert-text", after = TRUE, f = function(h,
        ...) {
        text <- h$getText()
        if (nzchar(gsub("[[:digit:]]", "", text))) {
            h$setText("")
            h$setText(gsub("[^[:digit:]]", "", text))
        }
    })

    hbox_t4$packStart(sizeT_label, FALSE, FALSE, 0)
    hbox_t4$packStart(sizeT_ent, FALSE, FALSE, 0)
    opt_table$attach(hbox_t4, left.attach = 1, 2, top.attach = 1, 2)

    #################################################

    PlotOk <- function(h, ...) {
        net_window["visible"] <- FALSE
        iter <- gtkComboBoxGetActiveIter(combobox_data)$iter
        item <- model_data$GetValue(iter, 0)$value
        g.net0 <- e$mod.all[[item]]
        g.mod <- as.matrix(g.net0, attrname = "flow")
        g.mod[g.mod>0] <- 1
        if(dim(g.mod)[1] == dim(g.mod)[2]){
          gnet <- graph_from_adjacency_matrix(g.mod) #Package: igraph
        }else{
          gnet <- graph_from_incidence_matrix(g.mod)
        }
        colN <- rgb(e$color$red, e$color$green, e$color$blue, maxColorValue = 65535)
        colT <- rgb(e$color2$red, e$color2$green, e$color2$blue, maxColorValue = 65535)

        plot.net(gnet = gnet, colN = colN, colT = colT, zN = as.numeric(sizeN_ent$getText()),
            zT = as.numeric(sizeT_ent$getText()), id.nam = item)
        net_window$destroy()
    }

    #################################################

    ok_button <- gtkButton("Ok", stock.id = "gtk-ok")
    gSignalConnect(ok_button, "clicked", f = PlotOk)
    ok_button$setSizeRequest(64, 25)
    cancel_button <- gtkButton("Cancel", stock.id = "gtk-cancel")
    gSignalConnect(cancel_button, "clicked", f = function(h, ...) net_window$destroy())
    cancel_button$setSizeRequest(64, 25)

    hbox_buttons <- gtkHBox(FALSE, 5)
    hbox_buttons$packEnd(cancel_button, expand = FALSE, fill = FALSE)
    hbox_buttons$packEnd(ok_button, expand = FALSE, fill = FALSE)
    vbox_win$packStart(hbox_buttons, expand = FALSE, fill = FALSE)
    gtkComboBoxSetActive(combobox_data, 0)
    net_window$showAll()

    #################################################

}


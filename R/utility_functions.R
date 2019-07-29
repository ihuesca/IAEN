# Functions#########################################

#' Delete symbols in gtext
#' @keywords internal

delete.elements <- function(h, ...) {
    text <- h$getText()
    if (nzchar(gsub("[[:alnum:]]", "", text))) {
        h$setText("")
        h$setText(gsub("[^[:alnum:]]", "", text))
    }
}

#################################################

#' Repead name
#' @keywords internal

name.rep <- function(n0, ...) {
    nn <- c(names(e$datU), names(e$datW), names(e$datB))
    if (any(nn == n0)) {
        n2 <- paste(n0, "(1)", sep = "")
        if (any(nn == n2)) {
            j <- 0
            i <- 2
            while (j == 0) {
                if (any(nn == n2)) {
                  n2 <- paste(n0, "(", i, ")", sep = "")
                  i <- i + 1
                } else {
                  j <- 1
                }
            }
        }
        return(n2)
    } else {
        return(n0)
    }
}

#################################################

#' Insert page in notebook
#' @keywords internal

gtkNotebookInsertPageWithCloseButton <-
	function(object, child, label.text="", position=-1){
		icon <- gtkImage( pixbuf =
			object$renderIcon( "gtk-close" , "button" , size = "menu" ))
		closeButton<-gtkButton( )
		closeButton$setImage(icon)
		closeButton$setRelief("none")
		##
		label<-gtkHBox( )
		label$packStart(gtkLabel(label.text))
		label$packEnd(closeButton)
		##
		gSignalConnect(closeButton , "clicked", function ( button ){
		index<-object$pageNum( child )
		object$removePage( index )
	})
	object$insertPage(child, label, position)
	N<-gtkNotebookGetNPages(object)
	object["page"] <- N-1

}

#################################################

#' Zoom for plots
#' @keywords internal

zoomPlot <- function(x = 2) {
    allocation <- device$getAllocation()$allocation
    device$setSizeRequest(allocation$width * x, allocation$height * x)
    updateAdjustment <- function(adjustment) {
        adjustment$setValue(x * adjustment$getValue() + (x - 1) * adjustment$getPageSize()/2)
    }
    updateAdjustment(scrolled$getHadjustment())
    updateAdjustment(scrolled$getVadjustment())
}

#################################################

#' Spinner start
#' @keywords internal

SpinStart <- function(h, ...) {
    spinner$start()
    spi_hbox["visible"] <- TRUE
}

#' Spinner stop
#' @keywords internal

SpinStop <- function(h, ...) {
    e$run.spin[e$run.spin == TRUE][1] <- FALSE
    if (any(e$run.spin == TRUE)) {
        NULL
    } else {
        spi_hbox["visible"] <- FALSE
        spinner$stop()
    }
}

#################################################

#' Format for plots
#' @keywords internal

Format.plot <- function(nam, type, ...) {
    switch(type, tiff = tiff(filename = nam, width = 3200, height = 3200,
        family = "Times", compression = "zip", pointsize = 21, units = "px",
        type = "cairo", res = 300), jpeg = jpeg(filename = nam, width = 3200,
        height = 3200, units = "px", family = "Times", pointsize = 21,
        quality = 100, res = 300, restoreConsole = TRUE, type = "cairo"),
        png = png(filename = nam, width = 3200, height = 3200, units = "px",
            family = "Times", pointsize = 21, res = 300, restoreConsole = TRUE,
            type = "cairo"), eps = postscript(file = nam, width = 12, height = 8,
            paper = "special", family = "Times", horizontal = TRUE))
}

#################################################

#' Export data
#' @keywords internal

export_data_func <- function(h, ...) {

    if (Notebook["page"] == 1 & gtkNotebookGetNPages(data.note) > 0 | Notebook["page"] ==
        2 & gtkNotebookGetNPages(result.note) > 0) {

        #################################################

        ok_func <- function(h, ...) {
            name_export <- name_ent$getText()
            folder_export <- folder_button$getFilename()

            if (nchar(name_export) > 1 & file.exists(folder_export)) {

                dirr <- paste(folder_export, "\\", name_export, ".xlsx",
                  sep = "")
                export_window["visible"] <- FALSE

                if (Notebook["page"] == 2) {
                  id <- gtkNotebookGetCurrentPage(result.note) + 1
                  export_base <- e$Result[[id]]
                  write_xlsx(export_base, dirr) #Package:writexl
                }

                if (Notebook["page"] == 1) {
                  id <- gtkNotebookGetCurrentPage(data.note) + 1
                  export_data <- e$dat.all[[id]]
                  write_xlsx(export_data, dirr) #Package:writexl
                }

                export_window$destroy()
            }
        }

        #################################################

        export_window <- gtkWindow(show = FALSE)
        export_window$setTransientFor(Window)
        export_window$setPosition("center-on-parent")
        export_window["title"] <- "Export file ..."
        export_window$setSizeRequest(320, 95)
        export_window["border-width"] <- 2
        export_window$setModal(TRUE)
        export_window$setResizable(FALSE)

        tab_exp <- gtkTable(rows = 3, columns = 1, homogeneous = TRUE)
        export_window$add(tab_exp)
        name_label <- gtkLabel("Name:")
        name_ent <- gtkEntry()
        name_ent$modifyText(GtkStateType["normal"], "blue")
        name_ent$insertText("Data")
        gSignalConnect(name_ent, "insert-text", after = TRUE, f = delete.elements)
        hbox_exp1 <- gtkHBox(FALSE, 1)
        hbox_exp1$packStart(name_label, FALSE, FALSE)
        hbox_exp1$packStart(name_ent, TRUE, TRUE)

        folder_label <- gtkLabel("Folder:")
        folder_button <- gtkFileChooserButton("Select a folder", "select-folder")

        hbox_exp2 <- gtkHBox(FALSE, 1)
        hbox_exp2$packStart(folder_label, FALSE, FALSE)
        hbox_exp2$packStart(folder_button, TRUE, TRUE)

        ok_button2 <- gtkButton("Ok", stock.id = "gtk-ok")
        gSignalConnect(ok_button2, "clicked", f = ok_func)
        ok_button2$setSizeRequest(70, 25)
        cancel_button2 <- gtkButton("Cancel", stock.id = "gtk-cancel")
        gSignalConnect(cancel_button2, "clicked", f = function(h, ...) export_window$destroy())
        cancel_button2$setSizeRequest(70, 25)
        hbox_exp3 <- gtkHBox(FALSE, 1)
        hbox_exp3$packEnd(cancel_button2, FALSE, TRUE)
        hbox_exp3$packEnd(ok_button2, FALSE, TRUE)

        tab_exp$attach(hbox_exp1, left.attach = 0, 1, top.attach = 0, 1,
            xoptions = c("fill"))
        tab_exp$attach(hbox_exp2, left.attach = 0, 1, top.attach = 1, 2,
            xoptions = c("fill"), yoptions = c("shrink"))
        tab_exp$attach(hbox_exp3, left.attach = 0, 1, top.attach = 2, 3,
            yoptions = c("shrink"))

        export_window["visible"] <- TRUE

        #################################################
    }
}

#################################################

#' About
#' @keywords internal

About_func <- function(h, ...) {

    window_about <- gtkWindow(show = FALSE)
    window_about$setTransientFor(Window)
    window_about$setPosition("center-on-parent")
    window_about["title"] <- "IAEN"
    window_about$setSizeRequest(257, 330)
    window_about$setModal(TRUE)
    window_about$setResizable(FALSE)

    vbox_about <- gtkVBox()
    window_about$add(vbox_about)
    event_about <- gtkEventBox()
    vbox_about$packStart(event_about, expand = TRUE, fill = TRUE)
    event_about$modifyBg(GtkStateType["normal"], "white")
    vbox_about2 <- gtkVBox()
    event_about$add(vbox_about2)

    Ab1 <- gtkLabel("IAEN")
    Ab2 <- gtkLabel("Introduction to the analysis \n of ecological networks")
    Ab2$setJustify("center")
    Ab3 <- gtkLabel("Version 1.0")
    Ab4 <- gtkLabel("Universidad Veracruzana \n Instituto de Investigaciones Biol\u00F3gicas")
    Ab4$setJustify("center")
    Ab5 <- gtkLabel("Authors:")
    Ab6 <- gtkLabel("Abarca Arenas, L.G. \n Huesca Dom\u00EDnguez, I.")
    Ab6$setJustify("center")
    Ab7 <- gtkLabel("Contacts:")
    Ab8 <- gtkLabel("gabarca@uv.mx \n ishuesca@uv.mx")
    Ab8$setJustify("center")
    Ab9 <- gtkLabel("General Public License (GPL)")
    Ab10 <- gtkLabel("GUI")

    sapply(list(Ab1, Ab2, Ab3, Ab4, Ab5, Ab6, Ab7, Ab8, Ab9, Ab10), vbox_about2$packStart)

    pango.font.about <- pangoFontDescriptionNew()

    font.desc(obj = pango.font.about, weight = "bold", size = 14)
    Ab1$modifyFont(pango.font.about)
    Ab1$modifyFg(GtkStateType["normal"], "midnightblue")

    font.desc(obj = pango.font.about, weight = "bold", size = 10)
    Ab2$modifyFont(pango.font.about)
    Ab2$modifyFg(GtkStateType["normal"], "midnightblue")

    font.desc(obj = pango.font.about, style = "oblique", size = 8)
    Ab3$modifyFont(pango.font.about)

    font.desc(obj = pango.font.about, weight = "bold", size = 8)
    Ab4$modifyFont(pango.font.about)

    font.desc(obj = pango.font.about, weight = "bold", size = 8)
    Ab5$modifyFont(pango.font.about)

    font.desc(obj = pango.font.about, size = 8)
    Ab6$modifyFont(pango.font.about)

    font.desc(obj = pango.font.about, weight = "bold", size = 8)
    Ab7$modifyFont(pango.font.about)

    font.desc(obj = pango.font.about, size = 8)
    Ab8$modifyFont(pango.font.about)
    Ab9$modifyFont(pango.font.about)
    Ab10$modifyFont(pango.font.about)

    window_about$showAll()
}

#################################################


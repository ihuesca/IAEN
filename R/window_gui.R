#' @title Introduction to the analysis of ecological networks
#'
#' @author Luis Gerardo Abarca-Areanas, Israel Huesca-Dominguez
#'
#' @description The IAEN package is a tool that provides a graphical user interface (GUI), allows data management performing different types of analyses used in ecological networks. Includes visualization and editing of graphics, methods for binary, weighted and bipartite ecological networks. You can also simulate binary networks using some classic models, using the networks topology, n number of repetitions can be produced, and use functions employed in networks of small worlds.
#'
#' You can learn more about this package at:
#' https://github.com/ihuesca/IAEN/
#'
#' @details
#'
#' IAEN is a package with a graphical interface dedicated to analysis of ecological networks.
#'
#' Note: IAEN is free software and comes with ABSOLUTELY NO WARRANTY.
#'
#'
#' @param h Not usable
#' @param ... Not usable
#' @return IAEN is a graphic interface
#' @examples \dontrun{
#' ##Install package
#' library(IAEN)
#' ##Call the package
#' IAEN()
#' }
#'
#' @references
#' Cohen, J. E., & Newman, C. M. (1985). A stochastic theory of community food webs I.
#' Models and aggregated data. Proceedings of the Royal society of London. Series B.
#' Biological sciences, 224(1237), 421-448.
#'
#' Dame, R. F., & Patten, B. C. (1981). Analysis of energy flows in an intertidal oyster reef.
#' Marine Ecology Progress Series, 5(2), 115-124.
#'
#' Dormann, C. F. (2011). How to be a specialist? Quantifying specialisation in pollination
#' networks. Network Biology, 1(1), 1-20.
#'
#' Erds, P., & Rényi, A. (1960). On the evolution of random graphs. Publ. Math. Inst.
#' Hungar. Acad. Sci, 5, 17-61.
#'
#' Goldwasser, L., & Roughgarden, J. (1993). Construction and Analysis of a Large Caribbean
#' Food Web: Ecological Archives E074-001. Ecology, 74(4), 1216-1233.
#'
#' GTK+ (2007). The Gimp Tool Kit. url: http://www.gtk.org/.
#'
#' Lawrence, M., & Temple Lang, D. (2010). RGtk2: A graphical user interface toolkit for R.
#' Journal of Statistical Software, 37(8), 1-52.
#'
#' Ulanowicz, R. E., & Puccia, C. J. (1990). Mixed trophic impacts in ecosystems.
#' Coenoses, 7-16.
#'
#' Watts, D. J., & Strogatz, S. H. (1998). Collective dynamics of ‘small-world’networks.
#' Nature, 393(6684), 440.
#'
#' Williams, R. J., & Martinez, N. D. (2000). Simple rules yield complex food webs.
#' Nature, 404(6774), 180.
#'
#' @export IAEN
#' @import enaR
#' @import network
#' @import readxl
#' @import writexl
#' @import ggplot2
#' @import ggraph
#' @import reshape
#' @import pryr
#' @import cairoDevice
#' @import scales
#' @import gplots
#' @importFrom stats sd var aggregate runif as.dendrogram hclust dist
#' @importFrom tnet closeness_w betweenness_w as.tnet degree_w
#' @importFrom igraph betweenness closeness degree graph_from_adjacency_matrix mean_distance transitivity centr_betw centr_clo centr_degree all_simple_paths sample_gnm as_adjacency_matrix mst get.adjacency graph.incidence V set_edge_attr graph_from_incidence_matrix "%>%" get.edgelist
#' @importFrom sna evcent graphcent infocent bonpow loadcent stresscent reachability is.connected isolates
#' @importFrom bipartite networklevel plotweb grouplevel specieslevel nestedrank nestedcontribution H2fun dfun NOS computeModules
#' @importFrom grDevices colorRampPalette dev.off jpeg png postscript rainbow rgb tiff
#' @importFrom graphics axis plot
#' @importFrom utils read.table

##### GUI#####

IAEN <- function(h, ...) {

    cmd <- "require(RGtk2)"
    eval(parse(text=cmd))

    if (! RGtk2::gtkInit()){
      stop("RGtk2 was not loaded.\n",
           "The execution of IAEN () requires a window system.\n",
           "Install it appropriately according to your operating system.\n")}
    
    
    ## environmet#######################################

    .GlobalEnv$e <- new.env()

    #assign("dat1", matrix(), env = e)
    #assign("datB", list(), env = e)
    #assign("datU", list(), env = e)
    #assign("datW", list(), env = e)
    #assign("modW", list(), env = e)
    #assign("dat.all", list(), env = e)
    #assign("COM.all", list(), env = e)
    #assign("Modules.all", list(), env = e)
    #assign("otro", list(), env = e)
    #assign("Result", list(), env = e)
    #assign("datWinGrid", c(), env = e)
    #assign("mod0", 0, env=e)
    #assign("mod1", 0, env=e)
    e$dat1 <- matrix()
    e$datB <- list()
    e$datU <- list()
    e$ datW <- list()
    e$modW <- list()
    e$dat.all <- list()
    e$COM.all <- list()
    e$Modules.all <- list()
    e$otro <- list()
    e$Result <- list()
    e$datWinGrid <- c()
    e$mod0 <- 0
    e$mod1 <- 0

    # Run Spinner
    #assign("run.spin", c(FALSE), env = e)
    e$run.spin <- c(FALSE)

    # cont
    #assign("cont", 0, env = e)
    e$cont <- 0

    # Format file
    #assign("FileName", list(), env = e)
    #assign("FilterName", list(), env = e)
    #assign("BaseName", list(), env = e)
    e$FileName <- list()
    e$FilterName <- list()
    e$BaseName <- list()

    # File#####################################################

    fImport <- gtkImageMenuItem(label = "Import")
    fImport$setImage(gtkImageNewFromStock("gtk-open", size = GtkIconSize["menu"]))
    fQuit.Action <- gtkAction(label = "Quit", stock.id = "gtk-quit")
    fQuit <- fQuit.Action$CreateMenuItem()
    gSignalConnect(fQuit, "activate", function(...) {
        Window$destroy()
    })
    .GlobalEnv$fExport.Action <- gtkAction(label = "Export", stock.id = "gtk-save")
    gSignalConnect(fExport.Action, "activate", f = export_data_func)
    .GlobalEnv$fExport <- fExport.Action$CreateMenuItem()

    # Data#####################################################

    dI.W.Action <- gtkAction(label = "Weighted", stock.id = "gtk-new")
    gSignalConnect(dI.W.Action, "activate", f = wW)
    dI.W <- dI.W.Action$CreateMenuItem()
    dI.U.Action <- gtkAction(label = "Unweighted", stock.id = "gtk-new")
    gSignalConnect(dI.U.Action, "activate", f = wU)
    dI.U <- dI.U.Action$CreateMenuItem()
    dI.B.Action <- gtkAction(label = "Bipartite", stock.id = "gtk-new")
    gSignalConnect(dI.B.Action, "activate", f = wB)
    dI.B <- dI.B.Action$CreateMenuItem()

    # Statistics################################################

    sW.Action <- gtkAction(label = "Weighted matrix", stock.id = "gtk-yes")
    gSignalConnect(sW.Action, "activate", f = statW_func)
    .GlobalEnv$sW <- sW.Action$CreateMenuItem()
    sU.Action <- gtkAction(label = "Unweighted matrix", stock.id = "gtk-yes")
    gSignalConnect(sU.Action, "activate", f = statU_func)
    .GlobalEnv$sU <- sU.Action$CreateMenuItem()
    sB.Action <- gtkAction(label = "Bipartite matrix", stock.id = "gtk-yes")
    gSignalConnect(sB.Action, "activate", f = statB_func)
    .GlobalEnv$sB <- sB.Action$CreateMenuItem()

    # Simulation################################################

    Sim.Action <- gtkAction(label = "Simulations", stock.id = "gtk-yes")
    gSignalConnect(Sim.Action, "activate", f = simulation_func)
    Sim <- Sim.Action$CreateMenuItem()

    # Graph#####################################################

    Graph.Net <- gtkImageMenuItem(label = "Network")
    Graph.Net$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.Hm <- gtkImageMenuItem(label = "Heat map")
    Graph.Hm$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.UM <- gtkImageMenuItem(label = "Unweighted matrix")
    Graph.UM$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.WM <- gtkImageMenuItem(label = "Weighted matrix")
    Graph.WM$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.BM <- gtkImageMenuItem(label = "Bipartite Matrix")
    Graph.BM$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))

    Graph.Cent <- gtkImageMenuItem(label = "Centrality")
    Graph.Cent$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.WebL <- gtkImageMenuItem(label = "Web By Level")
    Graph.WebL$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.ST <- gtkImageMenuItem(label = "Spanning Tree")
    Graph.ST$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))

    Graph.Imp <- gtkImageMenuItem(label = "Impact")
    Graph.Imp$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))

    Graph.NetB <- gtkImageMenuItem(label = "Network")
    Graph.NetB$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.PW <- gtkImageMenuItem(label = "Plot Web")
    Graph.PW$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))
    Graph.Mod <- gtkImageMenuItem(label = "Modularity")
    Graph.Mod$setImage(gtkImageNewFromStock("gtk-yes", size = GtkIconSize["menu"]))

    Graph1.Action <- gtkAction(label = "ggraph", stock.id = "gtk-yes")
    gSignalConnect(Graph1.Action, "activate", f = window_plot_net)
    .GlobalEnv$Graph1 <- Graph1.Action$CreateMenuItem()
    Graph2.Action <- gtkAction(label = "ggplot/heatmap.2", stock.id = "gtk-yes")
    gSignalConnect(Graph2.Action, "activate", f = window_plot_heatmap)
    .GlobalEnv$Graph2 <- Graph2.Action$CreateMenuItem()

    Graph3.Action <- gtkAction(label = "PlotNPS", stock.id = "gtk-yes")
    gSignalConnect(Graph3.Action, "activate", f = window_plot_centrality)
    .GlobalEnv$Graph3 <- Graph3.Action$CreateMenuItem()
    Graph4.Action <- gtkAction(label = "PlotWebByLevel", stock.id = "gtk-yes")
    gSignalConnect(Graph4.Action, "activate", f = window_plot_level)
    .GlobalEnv$Graph4 <- Graph4.Action$CreateMenuItem()
    Graph5.Action <- gtkAction(label = "mst", stock.id = "gtk-yes")
    gSignalConnect(Graph5.Action, "activate", f = window_plot_span)
    .GlobalEnv$Graph5 <- Graph5.Action$CreateMenuItem()

    Graph7.Action <- gtkAction(label = "ggplot", stock.id = "gtk-yes")
    gSignalConnect(Graph7.Action, "activate", f = window_plot_impact)
    .GlobalEnv$Graph7 <- Graph7.Action$CreateMenuItem()

    Graph8.Action <- gtkAction(label = "graph.incidence", stock.id = "gtk-yes")
    gSignalConnect(Graph8.Action, "activate", f = window_plot_inc)
    .GlobalEnv$Graph8 <- Graph8.Action$CreateMenuItem()
    Graph9.Action <- gtkAction(label = "plotweb", stock.id = "gtk-yes")
    gSignalConnect(Graph9.Action, "activate", f = window_plot_bip)
    .GlobalEnv$Graph9 <- Graph9.Action$CreateMenuItem()
    Graph10.Action <- gtkAction(label = "plotModuleWeb", stock.id = "gtk-yes")
    gSignalConnect(Graph10.Action, "activate", f = window_plot_module_bip)
    .GlobalEnv$Graph10 <- Graph10.Action$CreateMenuItem()

    # Help######################################################

    Aboutt.Action <- gtkAction(label = "About", stock.id = "gtk-about")
    gSignalConnect(Aboutt.Action, "activate", f = About_func)
    Aboutt <- Aboutt.Action$CreateMenuItem()

    # Menu######################################################

    ##### Menu#####

    Menu.bar <- gtkMenuBar()

    ##### Bar#####

    File.mb <- gtkMenuItemNewWithMnemonic(label = "_File")
    Menu.bar$append(File.mb)

    Statistics.mb <- gtkMenuItemNewWithMnemonic(label = "_Statistics")
    Menu.bar$append(Statistics.mb)

    Simulations.mb <- gtkMenuItemNewWithMnemonic(label = "_Simulations")
    Menu.bar$append(Simulations.mb)

    Graph.mb <- gtkMenuItemNewWithMnemonic(label = "_Graphs")
    Menu.bar$append(Graph.mb)

    Help.mb <- gtkMenuItemNewWithMnemonic(label = "_Help")
    Menu.bar$append(Help.mb)

    ##### Sub Menu#####

    File.menu1 <- gtkMenu()
    File.mb$setSubmenu(File.menu1)
    File.menu2 <- gtkMenu()
    fImport$setSubmenu(File.menu2)

    Statistics.menu1 <- gtkMenu()
    Statistics.mb$setSubmenu(Statistics.menu1)

    Simulations.menu1 <- gtkMenu()
    Simulations.mb$setSubmenu(Simulations.menu1)

    Graph.menu1 <- gtkMenu()
    Graph.mb$setSubmenu(Graph.menu1)
    Graph.menu2 <- gtkMenu()
    Graph.UM$setSubmenu(Graph.menu2)
    Graph.menu3 <- gtkMenu()
    Graph.WM$setSubmenu(Graph.menu3)
    Graph.menu4 <- gtkMenu()
    Graph.BM$setSubmenu(Graph.menu4)

    Graph.menu5 <- gtkMenu()
    Graph.Net$setSubmenu(Graph.menu5)
    Graph.menu6 <- gtkMenu()
    Graph.Hm$setSubmenu(Graph.menu6)
    Graph.menu7 <- gtkMenu()
    Graph.Cent$setSubmenu(Graph.menu7)
    Graph.menu8 <- gtkMenu()
    Graph.WebL$setSubmenu(Graph.menu8)
    Graph.menu9 <- gtkMenu()
    Graph.ST$setSubmenu(Graph.menu9)
    Graph.menu11 <- gtkMenu()
    Graph.Imp$setSubmenu(Graph.menu11)
    Graph.menu12 <- gtkMenu()
    Graph.NetB$setSubmenu(Graph.menu12)
    Graph.menu13 <- gtkMenu()
    Graph.PW$setSubmenu(Graph.menu13)
    Graph.menu14 <- gtkMenu()
    Graph.Mod$setSubmenu(Graph.menu14)

    Help.menu1 <- gtkMenu()
    Help.mb$setSubmenu(Help.menu1)

    ##### Items#####

    items1 <- list(fImport, fExport, gtkSeparatorMenuItem(), fQuit)
    sap.it1 <- sapply(items1, function(i) {
        File.menu1$append(i)
    })
    items2 <- list(dI.W, dI.U, dI.B)
    sap.it2 <- sapply(items2, function(i) {
        File.menu2$append(i)
    })

    items3 <- list(sW, sU, sB)
    sap.it3 <- sapply(items3, function(i) {
        Statistics.menu1$append(i)
    })

    items4 <- list(Sim)
    sap.it4 <- sapply(items4, function(i) {
        Simulations.menu1$append(i)
    })

    items5 <- list(Graph.Net, Graph.Hm, gtkSeparatorMenuItem(), Graph.UM,
        Graph.WM, Graph.BM)
    sap.it5 <- sapply(items5, function(i) {
        Graph.menu1$append(i)
    })
    items6 <- list(Graph.Cent, Graph.WebL, Graph.ST)
    sap.it6 <- sapply(items6, function(i) {
        Graph.menu2$append(i)
    })
    items7 <- list(Graph.Imp)
    sap.it7 <- sapply(items7, function(i) {
        Graph.menu3$append(i)
    })
    items8 <- list(Graph.NetB, Graph.PW, Graph.Mod)
    sap.it8 <- sapply(items8, function(i) {
        Graph.menu4$append(i)
    })

    Graph.menu5$append(Graph1)
    Graph.menu6$append(Graph2)
    Graph.menu7$append(Graph3)
    Graph.menu8$append(Graph4)
    Graph.menu9$append(Graph5)
    Graph.menu11$append(Graph7)
    Graph.menu12$append(Graph8)
    Graph.menu13$append(Graph9)
    Graph.menu14$append(Graph10)

    items9 <- list(Aboutt)
    sap.it9 <- sapply(items9, function(i) {
        Help.menu1$append(i)
    })

    ###### Window#####

    .GlobalEnv$Window <- gtkWindow()
    Window["title"] <- "IAEN"
    Window$setSizeRequest(727, 500)
    vbox <- gtkVBox()
    Window$add(vbox)
    vbox$packStart(Menu.bar, FALSE, FALSE)

    ##### Toolbar#####

    dI.W.Tool <- dI.W.Action$createToolItem()
    dI.U.Tool <- dI.U.Action$createToolItem()
    dI.B.Tool <- dI.B.Action$createToolItem()
    .GlobalEnv$fExport.Action.Tool <- fExport.Action$createToolItem()
    Aboutt.Action.Tool <- Aboutt.Action$createToolItem()
    fQuit.Action.Tool <- fQuit.Action$createToolItem()
    gSignalConnect(fQuit.Action.Tool, "clicked", function(...) {
        Window$destroy()
    })

    toolbar <- gtkToolbar()
    toolbar$add(dI.W.Tool)
    toolbar$add(dI.U.Tool)
    toolbar$add(dI.B.Tool)
    toolbar$add(fExport.Action.Tool)
    toolbar$add(gtkSeparatorToolItem())
    toolbar$setStyle("both")
    toolbar$add(Aboutt.Action.Tool)
    toolbar$add(gtkSeparatorToolItem())
    toolbar$add(fQuit.Action.Tool)

    vbox$packStart(toolbar, FALSE, FALSE)

    ##### Present#####

    p.vbox <- gtkVBox()
    p <- gtkEventBox()
    p.vbox$packStart(p, expand = TRUE, fill = TRUE)
    p$modifyBg(GtkStateType["normal"], "powderblue")
    p2.vbox <- gtkVBox()
    p$add(p2.vbox)

    p2.vbox$setBorderWidth(21)

    Lab1 <- gtkLabel()
    Lab1$setJustify("center")
    Lab1$setMarkup("IAEN \n\n Introduction to the analysis of ecological networks")
    Lab2 <- gtkLabel("Version: 0.1.0")
    Lab3 <- gtkLabel("Universidad Veracruzana \n Instituto de Investigaciones Biol\u00F3gicas")
    Lab3$setJustify("center")
    Lab4 <- gtkLabel("GUI")

    Lab5 <- gtkLabel("Imports: RGtk2, cairoDevice, enaR,  cheddar,  bipartite, readxl, writexl, network, igraph,
\t\t sna, tnet,\n ggplot2, ggraph,  gplots, scales, reshape, pryr")
    Lab5$setJustify("center")

    sap.Lab <- sapply(list(Lab1, Lab2, Lab3, Lab4), p2.vbox$packStart,
        expand = TRUE, fill = TRUE)

    p2.vbox$packEnd(Lab5)

    pango.font.des <- pangoFontDescriptionNew()

    .GlobalEnv$font.desc <- function(obj, weight = "normal", style = "normal", size = 12,
        family = "serif", ...) {
        obj$setWeight(weight)
        obj$setStyle(style)
        obj$setSize(size * PANGO_SCALE)
        obj$setFamily(family)
    }

    font.desc(obj = pango.font.des, weight = "bold", size = 17)
    Lab1$modifyFont(pango.font.des)
    Lab1$modifyFg(GtkStateType["normal"], "midnightblue")

    font.desc(obj = pango.font.des, style = "oblique")
    Lab2$modifyFont(pango.font.des)

    font.desc(obj = pango.font.des, weight = "bold")
    Lab3$modifyFont(pango.font.des)

    font.desc(obj = pango.font.des)
    Lab4$modifyFont(pango.font.des)

    font.desc(obj = pango.font.des, size = 9)
    Lab5$modifyFont(pango.font.des)

    ##### Data Notebook#####

    .GlobalEnv$data.note <- gtkNotebook()
    data.note["scrollable"] <- TRUE

    ##### Result#####

    r.hbox <- gtkHBox()

    r.frame <- gtkFrameNew()
    r.frame$setSizeRequest(140, 100)

    .GlobalEnv$t.store <- gtkTreeStore("character")
    .GlobalEnv$tree_view <- gtkTreeView(t.store)
    tree_view$insertColumnWithAttributes(0, "", gtkCellRendererText(),
        text = 0)
    column <- tree_view$getColumn(0)
    column$setClickable(TRUE)
    tree_view["headers-visible"] <- FALSE
    gSignalConnect(tree_view, "cursor-changed", f = function(h, ...) {
        selection <- tree_view$getSelection()
        sel_path <- selection$getSelectedRows()$retval
        sel_row <- sel_path[[1]]$getIndices()
        result.note["page"] <- sel_row
    })

    r.frame$add(tree_view)
    r.hbox$packStart(r.frame, expand = FALSE, fill = FALSE)

    .GlobalEnv$result.note <- gtkNotebook()
    result.note["scrollable"] <- TRUE
    result.note["show-tabs"] <- FALSE
    result.note["tab-pos"] <- "left"

    r.hbox$add(result.note)

    ##### Notebook#####

    .GlobalEnv$Notebook <- gtkNotebook()
    Notebook$insertPage(p.vbox, gtkLabel("Present"))
    Notebook$insertPage(data.note, gtkLabel("Data"))
    Notebook$insertPage(r.hbox, gtkLabel("Result"))

    vbox$add(Notebook)

    ##### Spinner#####

    spi_frame <- gtkFrame()
    spi_frame$setSizeRequest(300, 21)
    vbox$packStart(spi_frame, expand = FALSE, fill = FALSE)
    .GlobalEnv$spi_hbox <- gtkHBox()
    spi_frame$add(spi_hbox)
    .GlobalEnv$spinner <- gtkSpinner()
    spi_hbox$packStart(spinner, expand = FALSE, fill = FALSE)
    run_label <- gtkLabel(" Running ...")
    spi_hbox$packStart(run_label, expand = FALSE, fill = FALSE)
    spi_hbox["visible"] <- FALSE

    #####

    sapply(list(Graph1, Graph2, Graph3, Graph4, Graph5, Graph7, Graph8,
        Graph9, Graph10, sW, sU, sB, fExport, fExport.Action.Tool), function(i) i["sensitive"] <- FALSE)

    Window["visible"] <- TRUE

}

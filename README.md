# IAEN
The IAEN package is a tool that provides a graphical user interface (GUI), allows data management performing different types of analyses used in ecological networks. Includes visualization and editing of graphics, methods for binary, weighted and bipartite ecological networks. You can also simulate binary networks using some classic models, using the networks topology, n number of repetitions can be produced, and use functions employed in networks of small worlds.

Note: we recommend having R, R studio and rtools installed, for stable operation.

## Install on Windows

```bash
#Previously install devtools and its dependencies.

install.packages("devtools", dependencies=TRUE)
library(devtools)

#Then you need to install the RGtk2 package, after load it to install the GTK + plug-in. 
#It is recommended to use R-project for installation. 
#Restart R-project after installation.

install.packages("RGtk2", depend=TRUE)
library(RGtk2)

# Install cairoDevice package

install.packages("cairoDevice")
library(cairoDevice)

#Install IAEN from github using devtools.

devtools::install_github("ihuesca/IAEN")
library(IAEN)
```

## Load package

```bash
library(RGtk2)
library(IAEN)
```

## Manuals

You can learn more about IAEN here[(Spanish version)]( https://ihuesca.github.io/IAEN-Manual/) [(English version)](https://ihuesca.github.io/IAEN-MANUAL-En/).

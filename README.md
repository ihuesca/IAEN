# IAEN
The IAEN package is a tool that provides a graphical user interface (GUI), allows data management performing different types of analyses used in ecological networks. Includes visualization and editing of graphics, methods for binary, weighted and bipartite ecological networks. You can also simulate binary networks using some classic models, using the networks topology, n number of repetitions can be produced, and use functions employed in networks of small worlds.

## Install on Windows

```bash
#Previously you need to install the RGtk2 package, after load it to install the GTK + plug-in. 
#It is recommended to use R-studio for installation. 
#Restart R-studio after installation.

install.packages("RGtk2")
library(RGtk2)

#Install devtools and its dependencies.

install.packages("devtools", dependencies=TRUE)
library(devtools)

#Install IAEN from github using devtools.

devtools::install_github("ihuesca/IAEN")
```

## Manuals

You can learn more about IAEN [here (Spanish version)](https://ihuesca.github.io/IAEN-spanish/).

# msir

### Installation ###

*msir* depends on the latest version of *MALDIquant* (that is not on CRAN yet):

```
if (!require("devtools")) {
    install.packages("devtools")
    library("devtools")
}
install_github("sgibb/MALDIquant")
install_github("sgibb/MALDIquantForeign")
install_bitbucket("sgibb/msir")

```

### Usage ###
```
library("msir")

msirGUI()
```

Please find the modified MSI.R scripts in `inst/scripts`


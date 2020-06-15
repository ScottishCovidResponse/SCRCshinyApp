# SCRCshinyApp


To install the app:
```{r}
library(devtools)
install_github("soniamitchell/SCRCshinyApp")
```

To run the app:

```{r}
library(SCRCshinyApp)
launchApp()
```

Note that the app currently works from a static dataset that will not 
automatically updated.

To update the dataset prior to running the app:

```{r}
launchApp(refresh = TRUE)
```

Note that this is dependent on the SCRCshinyApp package being installed:

```{r}
install_github("ScottishCovidResponse/SCRCdataAPI")
```

# ei-app-RxC

An R Shiny app that allows the user to run RxC ecological inference analysis to determine whether or not there was racially polarized voting in a particular election. Users can choose any number of candidates or demographic groups to run their analyses, though they should be careful to ensure that they include the complete set of both for each analysis (i.e. proportions of votes for each candidate must sum to one, as must the proportions of demographic groups). The app can be accessed on the web at https://vrdi.shinyapps.io/ei-app-rxc/.

Required to run analysis:
- Precinct-level data on vote totals by candidate for a given election (as percentages)
- Precinct-level demographic data

Output:
- Homogeneous precinct analysis results
- Goodman's regression results
- Ecological inference analysis results
- Text interpretation of results
- PDF report containing results and interpretation

Files:
- ExpertWitnessTemplate.docx : template for expert witness report on racially polarized voting
- report.Rmd	: PDF report of results
- server.R : R shiny file containing functions to run analysis and create plots
- ui.R	: R shiny file creating user interface
- www/userGuide.pdf : guide to using the app, including an example

More information in the user guide.

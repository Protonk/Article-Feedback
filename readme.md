# Article feedback research 

Created to look at the value of project quality assessments (GA/FA/etc) in light of broad user feedback available from the [Article feedback tool](http://en.wikipedia.org/wiki/Wikipedia:Article_Feedback_Tool).

Eventually we want to model the relationship between marginal changes in project quality assessment and user feedback.

## Components

Currently:

 - feed.import.R 
 	- Ingests the feedback data and the list of project rated articles. 
 	- Code is not yet written to save results to disk so unless the RData file or resulting df are saved you will need to call the API for each session.
 - feed.plots.R, feed.Animations.R, feed.ggplot.R
 	- For now, most of the big stuff is in here. Plots for summary stats and some other things
 - feed.reg.R 
 	- Simple linear regression, an aux regression and some binomial/multinomial regressions
 - mc.unique.plot.R
 	- Some fiddling about to model the lower end of the count distribution

## Quality

All of this stuff is coming from my personal use. I've tried to comment where possible and make sensible decisions but I make no guarantees.

It is also not written in the style of an R package. The code expects you to run each script as needed (after the import script) in order to load the functions and objects into your environment. Then you can plot or model as needed using the supplied functions/objects.

## License

- All code is released under a CC-BY-SA license, as described and linked in license.md

## Packages

This will eventually be automated with a call to sessionInfo()

### Required

- ggplot2 For plotting
- boot for some bootstrapping (not strictly required but it makes the code easier)
- Animation for saving animations (also requires ImageMagick)
- MASS for a few regression models and utility functions
- rjson for reading from Wikipedia API as needed

### Used for miscellany (or not used inside the script proper)

- xtable for printing tables

## Individual script notes

### feed.import.R

Some things of note:

- Contains variables which are specific to where you download those files. Or you can download them from w/in the program. Each of these is hard coded. This isn't completely optimal but it makes it easy on my end. 
- The two functions buildFeedDf() and applyFactors() default to remote = FALSE, meaning that they expect the supplied files to be available in the same file path before running. The import script calls both of them as it loads, so you have to edit the source before running it in order for it to work. 
- Setting applyFactors(remote = TRUE) is perfectly reasonable, as the article lists are tiny. But the article feedback dump is large and if you run the import script multiple times it needlessly burdens the wikimedia servers.
- Remote code now calls the MediaWiki API directly rather than grabbing a flat text file somewhere. 

### feed.ggplot.R
	
- Most of these use the full dataset without decimation so they will take a while to render. 

### feed.reg.R

- Two basic linear regressions. The first is a simple regression comparion rating average to other variables. The second is an auxillary regression meant to back out the influence of article length on rating average.
- The simple model is checked against a bootstrapped regression for a rough test of error structure.
- Summary stats and a table of differences between assessed articles are also produced.
- Two proportional odds models are fitted/bootstrapped in order to better illustrate the relationship between length, count and rating average on likelihood of assessment. Plots are produced in this script due to the overhead in fitting the function repeatedly.
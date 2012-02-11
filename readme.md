# Article feedback research 

Created to look at the value of project quality assessments (GA/FA/etc) in light of broad user feedback available from the [Article feedback tool](http://en.wikipedia.org/wiki/Wikipedia:Article_Feedback_Tool).

Eventually we want to model the relationship between marginal changes in project quality assessment and user feedback.

## Components

Currently:

 - feed.import.R 
 	- Ingests the feedback data and the list of project rated articles. 
 - API.get.R
 	- Currently a staging place for code which grabs info directly from Wikipedia API
 - feed.plots.R, feed.Animations.R, feed.ggplot.R
 	- For now, most of the big stuff is in here. Plots for summary stats and some other things
 - feed.reg.R 
 	- A start at some exploratory regresssions
 - mc.unique.plot.R
 	- Some fiddling about to model the lower end of the count distribution

## Quality

All of this stuff is coming from my personal use. I've tried to comment where possible and make sensible decisions but I make no guarantees.

It is also not written in the style of an R package. The code expects you to run each script as needed (after the import script) in order to load the functions and objects into your environment. Then you can plot or model as needed using the supplied functions/objects.

## Packages

This will eventually be automated with a call to sessionInfo()

### Required

- ggplot2 For plotting
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

#### API.get.R

- This file does not use the version of rjson from CRAN. That should work *ok* but will throw a lot of needless warnings because readLines() doesn't like the formatting of the API responses.
- It also does not work with RJSONIO (which has functions of the same name) because RJSONIO mangles special characters.

	
### feed.ggplot.R
	
- Most of these use the full dataset without decimation so they will take a while to render. 


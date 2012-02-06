# Article feedback research 

Created to look at the value of project quality assessments (GA/FA/etc) in light of broad user feedback available from the [Article feedback tool](http://en.wikipedia.org/wiki/Wikipedia:Article_Feedback_Tool).

Eventually we want to model the relationship between marginal changes in project quality assessment and user feedback.

## Components

Currently:

 - feed.import.R 
 	- Ingests the feedback data and the list of project rated articles. Contains variables which are specific to where you download those files. Or you can download them from w/in the program
 
 - feed.plots.R, feed.Animations.R, feed.ggplot.R
 	- For now, most of the big stuff is in here. Plots for summary stats and some other things
 - feed.reg.R 
 	- A start at some exploratory regresssions
 - mc.unique.plot.R
 	- Some fiddling about to model the lower end of the count distribution

## Quality

All of this stuff is coming from my personal use. I've tried to comment where possible and make sensible decisions but I make no guarantees.


# East River Forest Structure Paper: TODO

## Analysis
- Take out the gap-filled area
- Rerun everything with LS-derived tree crown map
- Species analysis
- Fix soil variables
	- K is wrong (Ksat instead???)
	- OOM differences across map units... 
- Export all results to eastriver Drive
- Export all data to ESS DIVE

## Tables, Figures, Numbers, Equations
- EQ: Write allometric equation and coefficients for methods
- FIG: Partial effects plots for ALL significant variables in GAM
- FIG: Partial dependence plots for significant variables in GBM
- NUM: Median and s.d. density at inventory sites
- NUM: Mean basal area at inventory sites
- FIG: Comparison of PC density, original NEON vs discretized waveform
- NUM: Elevation maximum density, height, etc.
- NUM: TWI values 
- CAP: Update Fig. 7 caption
- FIG: Figure with detected trees and CHM underlay
- FIG: Map with missing flightpath and gap-filled showing density discrepancy
- FIG: Re-create structure maps without gap-fill

## Draft 4 Revisions

### Abstract
- Update abstract, address abstract-specific comments

### Introduction
- Background on species for species analysis [need to do some reading, but can be short]
- Make sure intro sets up expectations you reflect on in discussion. E.g., if you say the study revealed unexpected results in some way, make sure these paragraphs set up those expectations, e.g., the water-energy balance framework
	
### Methods
- Mention variables dropped from models for multicollinearity!! (TWI, etc.)
- Mention the missing flightpath and that we attempted to gap-fill but density results were unreasonable
	- Show map with missing flightpath and gap-fill but showing density discrepancy
- Fix citations
- A sentence on what deviance explained means in GAM:
	"The deviance generalizes the Residual Sum of Squares (RSS) of the linear model. The generalization is driven by the likelihood and its equivalence with the RSS in the linear model... deviance is defined through the difference of the log-likelihoods between the fitted model, ℓ(^β), and the saturated model, ℓs." (https://bookdown.org/egarpor/PM-UC3M/glm-deviance.html)
- Mention lidR and lidRplugins implementations of some algorithms


### Conclusions
- LARA (in abstract): In conclusion, return to this point--"Quantifying the drivers of fine-scale heterogeneity... is an essential foundation for understanding how these systems will respond to synoptic environmental change..."--but say what you think your results reaaly say about each of these things. How do your results suggest these forests might change given plausible climate change? Can differences in height/density/BA be reliably detected using these methods and the error in your estimates? What does your analysis say about where in the watershed there may be key conservation values or management opportunities (e.g. for species, carbon, water, etc.). [In short, provide more detail on this important argument you briefly raise in abstract]



# DONE

## Analysis
- Run LayerStacking on full domain

## Draft 4 completed tasks

## Draft 3 completed tasks
X TAB: Make table of abiotic explanatory variables: Name, Category, Units, Source
X TAB: Make table of LayerStacking params for methods
X TAB: Make table of LayerStacking
X FIG: Figure with explanatory variables

## Prior drafts completed tasks
X Edit protocols for analysis of LiDAR waveform and canopy segmentation
X Read LiDAR papers
X Read papers on forest structure from RMBL drive
X Run TPI at large scale



NOTES
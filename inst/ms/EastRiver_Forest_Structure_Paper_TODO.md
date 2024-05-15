# East River Forest Structure Paper: TODO

## Analysis
- Zip data from savio and egress to GCS
- Export key data to ESS DIVE

## Draft 5 Revisions

### Abstract
- Update abstract, address abstract-specific comments
- In the abstract, you might want to point out the direction of correlations (more snow --> higher density?). It would be important for evaluating the impacts of changing climate. 

### Introduction
- You might want to include the GEDI mission somewhere (https://gedi.umd.edu/). This study could be an important benchmark for this global mapping project. 
	
### Methods
- A sentence on what deviance explained means in GAM:
	"The deviance generalizes the Residual Sum of Squares (RSS) of the linear model. The generalization is driven by the likelihood and its equivalence with the RSS in the linear model... deviance is defined through the difference of the log-likelihoods between the fitted model, ℓ(^β), and the saturated model, ℓs." (https://bookdown.org/egarpor/PM-UC3M/glm-deviance.html)
- Mention lidR and lidRplugins implementations of some algorithms

### Results
- [HARUKO] Figure 5. I might have missed, but I wonder why aspects are not included; folded into SWE? 
- FIG: Fig 5, Fig 6 update BA units to m^2 ha^-1
- NUM: sd of 95P height and QMD in full domain

### Conclusions
- LARA (in abstract): In conclusion, return to this point--"Quantifying the drivers of fine-scale heterogeneity... is an essential foundation for understanding how these systems will respond to synoptic environmental change..."--but say what you think your results reaaly say about each of these things. How do your results suggest these forests might change given plausible climate change? Can differences in height/density/BA be reliably detected using these methods and the error in your estimates? What does your analysis say about where in the watershed there may be key conservation values or management opportunities (e.g. for species, carbon, water, etc.). [In short, provide more detail on this important argument you briefly raise in abstract]

## Tables, Figures, Numbers, Equations


### Maybes
- FIG: Comparison of PC density, original NEON vs discretized waveform
- FIG: Map with missing flightpath and gap-filled showing density discrepancy
- TAB: make a table with AIC and/or REML values for each model selection approach (assuming one of the gam selection approaches actually works)

---

# DONE

## Analysis
- Run LayerStacking on full domain
- Take out the gap-filled area
- Rerun everything with LS-derived tree crown map
- Fix soil variables
	- K is wrong (Ksat instead???)
	- OOM differences across map units... 
- Variable and model selection in GAM/GBM
	1. Run with all possible variables
	2. Run with double penalty on all possible variables
	3. Run with no penalty on uncorrelated variables (drop)
	4. Run with double penalty on uncorrelated variables (drop)
	5. Run with shrinkage on uncorrelated variables (drop)
- Species analysis

## Draft 4 completed tasks

### Introduction
- Background on species for species analysis [need to do some reading, but can be short]
- Introduction: The first paragraph and the first sentence might be better to have the high-level statements (leading to why this is important, why you are studying this. What’s the point of studying forest structure..…). Yes, forests are important but you might want to articulate more unless you submit this to forestry journals (a big part of ET <- Fadji/Erica's paper). Let’s say “climate change is impacting forests….”
- Controlling factors: you need to separate water limitation and temperature limitation. This paper talks about the water-limited vs temperature limited. Pelletier, J. D., Barron‐Gafford, G. A., Gutiérrez‐Jurado, H., Hinckley, E. L. S., Istanbulluoglu, E., McGuire, L. A., ... & Tucker, G. E. (2018). Which way do you lean? Using slope aspect variations to understand Critical Zone processes and feedbacks. Earth Surface Processes and Landforms, 43(5), 1133-1154.
- In my zonation paper, I also discuss the impact of aspects (north facing - more conifer), which is consistent with their water-limited condition. 
- You might want to highlight the significance of this paper and why this type of studies has not been done (e.g., ground-truthing is hard; conifers are on steep slopes etc). 
- Make sure intro sets up expectations you reflect on in discussion. E.g., if you say the study revealed unexpected results in some way, make sure these paragraphs set up those expectations, e.g., the water-energy balance framework
- In general, you might want to be careful about causality vs correlations. In the title, for example, I feel that soil and snow are actually the factors controlled by topography. I think it's okay to stretch a bit and make arguments, but you might want to be careful. 


### Methods
- Mention that we buffered 100 m from edges of acquisition area, 100 m from edges of roads, 100 m from crested butte and mount crested butte and removed trees in buffer
- Mention the missing flightpath and that we attempted to gap-fill but density results were unreasonable
	- Show map with missing flightpath and gap-fill but showing density discrepancy
- Some notes on topography, soil types, geology types, with percentages
- Describe variable/model selection in GAM/GBM
- Mention variables dropped from models for multicollinearity!! (TWI, etc.)

### Tables, Figures, Numbers, Equations
- EQ: Write allometric equation and coefficients for methods
- NUM: Median and s.d. density at inventory sites
- NUM: Mean basal area at inventory sites
- FIG: Figure with detected trees and CHM underlay
- FIG: Re-create structure maps without gap-fill
- TAB: Table 8: significant geology variables
- CAP: Update Fig. 7 caption
- [HARUKO] Figure 4. I assume that mapping these forest structure metrics is a tremendous achievement. You might want to include the maps of these metrics. Also, people who work on remote sensing tend to love looking at maps. 
- FIG S3: Partial effects plots for ALL significant variables in GAM
- NUM: Elevation, maximum density, height, etc.
- NUM: TWI values 

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
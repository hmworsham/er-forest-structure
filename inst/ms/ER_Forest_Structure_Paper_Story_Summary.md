# FOREST STRUCTURE PAPER
## STORY SUMMARY

1. This paper is the first to evaluate a complement of conifer stand structural characteristics on a spatially continuous basis, incorporating end-members on state-factor gradients along which forest stands grow
2. Questions
	A. Which topographic, climate, and substrate variables exert the strongest influence on tree maximum stem height, diameter distribution, total number density, and basal area?
	B. To what extent do these factors interact to mediate the stated stand structural properties?
3. Major findings:
	A. Elevation has the strongest influence on all structure measures, except basal area
		- Evident in both GAM and GBM results
		- Elevation effect is strongly nonlinear, with unimodal maximum around 2800-3000 m
	B. Highest density correlates with mid-elevation SW slopes
	C. Greatest QMD and height appear at mid-elevation ridgetops
	D. Soil factors are more important in GBM than either climate or topographic factors
		- AWC, OM, geologic substrate
		- Then climatic water deficit
4. Climate, topography, or substrate?
	A. Consensus: abiotic constraints are important for explaining variability in forest structure from scales ranging from hillslope to continent
	B. At broad scales, climate factors (namely, the orthogonal vectors of E_A and CWD (E_P - E_A)) consistently emerge as the strongest constraints
	C. However, at finer scales, such as that of a watershed or basin, synoptic climate appears to have less of an influence than other locally varying factors. Why? 
		- Because at landscape (~ 10^3 to 10^6 m2) and watershed scales (~ 10^2 to 10^3 m2), microtopographic variation influences:
			- precipitation input (via orography, wind sheltering, rain shadow effects)
			- surface and subsurface storage, which can lag precipitation input on timescales from months to decades
			- radiation, which drives atmospheric demand ... 
			- ... all of which modify site energy and water balance in ways that trees must respond to physiologically
		- Because variability in soil properties directly drives plant-available water (parent material, depth, e.g.) and nutrient availability (parent material, composition, pH)
5. Relationship to prior science:
	- General trends in forest structure and composition and covariance with state factors
		- Stem diameter, basal, area, and growth rates tend to decline with elevation, with temperature as the key limiting control. The same properties also decline from valley to ridge positions, and from polar to equatorial exposures, perhaps as a result of these factors’ influence on water supply and deficit
		- Lydersen and North (2012) reported contrary findings in a Sierra Nevada mixed conifer forest, where upper slopes had both the highest quadratic mean diameter (QMD) and the tallest trees. 
		- Kane et al. (2015), furthermore, found that topography explained little variance in forest structure in a domain with a frequent fire return interval.
		- We find contrary evidence, with higher density at midslopes
	- Responses in maximum tree height to topographic, edaphic, lithologic, and climate factors at landscape/watershed scale (Fricker et al. 2019)
		- Nonlinear elevational control on maximum tree height
		- Maximum tree height lowest at lower elevations, but also declines at upper ends
		- Varies with scale
		- Climate (CWD and MAP) explain most variance in maximum tree height distribution
		- We find elevation to be stronger explainer
	- Few directly address microclimatic heterogeneity in high-elevation complex terrain, and none account for state-variable interactions
		- We find significant explanatory power in interactions, particularly between elevation and SWE
		- Typical elevation-related lapse rate means lower T up high
		- But cold-air pooling in system means low minima down low, too
		- Sweet spot of T in mid-elevational range
		- Further corroborated by snow dynamics
		- High SWE, low elevation suggests low temp >> low density, QMD, height
		- Interestingly, aspect and heat load don't exert interactive influence on elevation, though they could if we looked at different spatial scales
6. Other reflections
	- The inflection of the elevation:density and elevation:maximum height curves corresponds approximately to the dividing line between montane and subalpine zones, at around 2700–2900 m elevation. To a crude approximation, this suggests that there exists a zone of preferable conditions supporting stand density, basal area, and height growth occurring around this elevational range.
7. Uncertainties and questions
	- Is elevation an index for temperature here? Are we conflating elevation and sensible heat?
	- Elevation and CWD are strongly anticorrelated... 
	- Only explaining half of variance
		- Missing: disturbance history, individual tree life histories
	- We don't have other climate information
		- Low confidence in PRISM at this scale, and in approaches for downscaling 800 m to smaller scales with fidelity

# Misc notes
A. Historically, temperature and moisture have been the most common axes in gradient analysis, measured directly (instrumentation) or indirectly through elevation or latitude, moisture via P, radiation index or drought index (Stephenson 1998)
	B. Stephenson (1990, 1998) and Urban (2000) noted that T and moisture, on their own, are less relevant to vegetation productivity than are interactions between the overall water and energy balance
		- Temperature and moisture covary closely along gradients such as elevation and latitude: T increases and P decreases with increasing elevation, but T also partly drives evaporative demand, modifying P's contribution to plant-available moisture
		- Neither gives a reliable approximation of how climate matters physiologically to plants: how much water and energy are available, how much demand exceeds availability, or how much available water might go unused
		- Instead, they argue for evaluating vegetation characteristics along orthogonal axes of potential evapotranspiration (PE) and climatic water deficit (PE-AE), which better answer these plant-relevant questions
C. [At broad scales] at high elevations, E_A varies little with elevation, and so may not be a strong control on its own (Stephenson 1998; Armstrong and Stidd 1967; Whittaker and Niering 1975; Major 1977)

# Sources

# URBAN (2000)
## TAKEAWAY (VIA STEPHENSON):
temperature and 'moisture' (indexed as topographic moisture scalars) aren't the quantities of interest for understanding vegetation distribution. More relevant to plant response is supply and demand: climatic moisture deficit (PET-AET) and actual evapotranspiration (AET)
	- WHY? Partly because temperature and moisture covary tightly along same gradients >>> T decreases while P increases with increasing elevation, but T also affects evaporative demand, so influences P's contribution to soil moisture
	- "T" and "Moisture" labels do little to elucidate the underlying abiotic gradient complex
- With the Stephenson framework, can isolate specific factors of physical template as they affect water balance

- Physical factors governing water supply operate at 3 spatial scales (and the scales correspond, respectively, to P, drainage, and soil depth)
- Factors governing demand operate at 2 spatial scales (elevation and microtopography)
	- Soil depth most variable over small spatial extents, and has biggest impact on supply
	- At larger scales, microtopography plays more important role


# STEPHENSON (1990)
Ecologists should be using actual evapotranspiration (E_A, really 'standard ET') and climatic water defificit to try and explain plant distributions
	- RATHER THAN measures of energy supply (T, PET, radiation) and water supply (P) 
	- Though they're straightforward to measure, they fail in at least three respects:
		- Integrated over year, they may not capture seasonal mismatch (times of high radiation, low P, when plants rely on soil subsidy or face stress)
		- Assumes independence of their effects -- false: for plants to use water, must be energy; to use energy, must be water
		- [MW EDITORIAL]: they're also correlated along gradients, they may over or underestimate availability to plants when overlayed on a physical landscape... what do I mean by that? When you look at P alone, a lot of that input may not be available to plants depending on soil holding capacity (what's actually available to plants) AND confoundingly, on radiation/T, which drives evaporation

TAKEAWAY: interactions between water and energy matter
This can be resolved by looking at water balance -- energy/water supply and demand -- as orthogonal axes of variability
"how much usable energy and water are available simultaneously to plants, how much evaporative demand is not met by available water, and how much water is unusable excess."

DIRECT IMPORTANCE TO PLANTS: Actual evapotranspiration and deficit measure aspects of climate that are of direct physiological importance to plants.
	- E_A = the simultaneous availability of energy and water and, thus, a plant's ability to fix carbon. 
	- Net primary production (NPP) has been found to correlate positively with E_A. 
	- In contrast, deficit is related to heat stress that cannot be regulated by transpiration and metabolic costs that cannot be met by active photosynthesis
Actual evapotranspiration and surplus additionally measure aspects of climate that are of indirect physiological importance to plants. On well-drained soils, surplus measures the potential for soil leaching; both the high acidity (Arkley 1967) and the loss of fertility associated with leaching might act to reduce primary production

In west, E_A changes little with elevation.... ???? (on a regional-to-continental scale???)

"net primary production (NPP)...  increased with elevation in the Santa Catalina Mountains in the western United States (Whittaker and Niering 1975)... The increase in NPP in the western United States (where actual evapotranspiration changes little with elevation) may have resulted from decreasing maintenance respiration associated with the decrease in deficit with elevation, although the structure of the vegetation may also have been a factor (Whittaker and Niering 1975).""

"Some prior studies, while focusing attention on the two aspects of climate most important to plants (energy and water), share an important problem: they could not distinguish between climates that were similar in annual energy and water supplies but different in their seasonal timin"


DELCOURT (1982)
Different physical and biological processes influence the vegetational patterns observed at each spatial-temporal scale
At microscale (10^0:5*10^3 y, 1:10^6 m2): "Effects of topographic position, aspect, substrate, as well as water and nutrient availability are among the most important for the establishment and succession of species populations at this micro-scale." 
	Macroclimate variability also matters at this scale
	... in contrast to macro-scale, where glacial/interglacial climatic changes and soil formation are the big drivers of veg distribution and its change


SCALING: explanations of forest pattern must be scale-specific and must invoke different proximate explanatory variables at different scales (an empirical demonstration of a general ‘scaling principle’ suggested by Wiens 1989)


URBAN: Studies over the past few decades show a strong consensus on the relative importance of abiotic constraints in ex- plaining gradients over hillslopes and mountainsides. Consistently, temperature and moisture emerge as the principal axes of direct and indirect gradient analyses. In the case of indirect gradient analysis, these fac- tors are often inferred rather than measured directly: temperature is indexed as elevation or latitude, and soil moisture as an exposure index or drought scalar (Stephenson 1998).


Interpretation of gradient response is confounded by a fundamental lack of independence among envi- ronmental factors. In particular, the primary physical gradients of temperature and soil moisture are them- selves correlated and thus not easily separable. For example, temperature decreases while precipitation in- creases with increasing elevation. Further, temperature is identified as a separate axis in gradient studies but it directly affects evaporative demand and so cannot be isolated from the soil moisture gradient. Thus, while it is easy to label an ordination axis ‘temperature’ or ‘moisture’ these labels do little to elucidate the underlying abiotic gradient complex.



Stephenson (1990, 1998) found it useful to con- sider the interactions of water supply versus demand as they effect vegetation distribution. He suggested that temperature and ‘moisture’ (indexed as topo- graphic moisture scalars) should be abandoned as the primary axes of species response in vegetation stud- ies. He argued that actual evapotranspiration (AET) and climatic moisture deficit (PET−AET) would be more relevant to plant response, and he illustrated the utility of this framework across scales from montane to continental gradients. This framework is especially useful for our purposes because it allows us to isolate specific factors of the physical template as these effect the water balance.

Our scaling analyses imply that the physical fac- tors governing water supply operate at three different spatial scales corresponding to precipitation, drainage, and soil depth. By contrast, factors governing water demand (temperature, radiation) vary at two scales (elevation and microtopography). The characteristic scaling of the physical template is such that soil depth is the factor that is most variable over small spatial extents. At larger extents, variation in micro- topography (slope, aspect, convergence) plays a more pronounced role in governing the water balance (although soil depth continues to vary at these scales). At the scale of the Sierran range, variation in tem- perature and precipitation as governed by elevation and orography come into play as the primary con- straints on the system. Note, however, at this largest scale that fine-scale variability cannot be resolved lo- gistically. Conversely, at small scales temperature and precipitation are essentially constant and hence con- tribute little to an explanation of vegetation at these scales. Thus, explanations of forest pattern must be scale-specific and must invoke different proximate ex- planatory variables at different scales (an empirical demonstration of a general ‘scaling principle’ sug- gested by Wiens 1989). Importantly, the phenomenon being explained also varies with scale: temperature and precipitation provide a suitable explanation for the location of the mixed conifer zone but offer little to resolve the local distributions of tree species within this zone. Likewise, soils and microtopography help explain species distributions locally but these expla- nations cannot be extrapolated easily across larger, landscape-scale gradients.

Stephenson (1990, 1998) also suggested that ac- knowledging the different effects of supply versus demand components of the water balance was im- portant because tree species respond differentially to these components. In fact, he argued that supply and demand represented nearly orthogonal vectors of response. For example, at middle elevations in the Kaweah Basin sites on deep soils support white fir forest; sites that are droughty because of shallow soils (low water supply) support Jeffrey pine, while sites that are droughty due to southerly aspect (high evap- orative demand) instead support ponderosa pine (Fig- ures 5 and 6 in Stephenson 1998). Although our model uses different estimates of supply and demand (e.g., a Priestley Taylor PET as compared to Stephenson’s earlier use of the Thornthwaite estimate), it repro- duces the same qualitative result. To illustrate this, we graphed the water balance as an elevation gradient in a space defined by AET and water deficit. A change in water supply was simulated as a change in water stor- age, for a 50-cm versus a 150-cm soil. Change in water demand was illustrated with a change from northern to southern aspect for a 33% slope. These two vectors are indeed nearly orthogonal (Figure 8). Again, this implies that changes in the water balance mediated by changes in water demand as compared to water sup- ply could elicit qualitatively different responses in the vegetation.


A model that incorporates the effective compo- nents of the water balance can provide a robust frame work and appreciation for the way in which these com- ponents interact to generate the spatiotemporal pattern of the physical template of montane systems. This framework attends the effective components of the en- vironmental gradient complex and recognizes these at their characteristic spatiotemporal scales.


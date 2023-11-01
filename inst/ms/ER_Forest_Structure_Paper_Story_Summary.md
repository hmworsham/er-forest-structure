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





One strain of thinking in ecology holds that ecosystem development is a function of five state factors: topography, parent material, climate, organisms, and time [@jenny_derivation_1961; @amundson_state_1997]. Forest stand structure and composition are two emergent properties of ecosystem development that can be evaluated in terms of continuous variables observed at a given point in time. Quantifying the relationships between an ecosystem's structure and composition and its underlying state factors is a fundamental concern for ecology and biochemistry [Vitousek and Matson 1991 [TODO: fix cite]]. While general relationships are acknowledged between (on the one hand) topographic, edaphic, lithologic, and climate state variables and (on the other) mature forest structure and composition, few studies have quantified the relative importance of state factors to stand structure and composition, or their potential interactive effects, particularly in subalpine domains. Several studies of the spatial variability of stand density, size distribution, and species composition in montane and subalpine forests have produced inconsistent results [e.g. Underwood et al. 2010; @lydersen_topographic_2012 [TODO: other citations]]. These inconsistencies suggest that at least some inferences about these relationships reflect an insufficient reckoning of how state factors interact to affect the hydrologic and energetic conditions in which plants grow [@lookingbill_empirical_2004]. Moreover, only a handful of papers have used spatially continuous estimates of stand structure to capture the full range of variability in either state factors or emergent ecosystem properties. This has been especially difficult to achieve in high-elevation, mountainous terrain because end-members on topographic and climate gradients are often inaccessible for field measurement. This limitation may now be partially overcome with active remote sensing technologies, such as light detection and ranging (LiDAR) ([TODO: cite]). Because biogeochemical fluxes between forests and the atmosphere are influenced by stand structure and composition, measuring these characteristics and their underlying environmental drivers is a central objective for forest ecology, conservation, and management [Waring and Running 1998].

## 1.1 Variability in forest structure and composition

### 1.1.1 Forest structure and topography

Topographic properties such as elevation, slope, hillslope position, curvature, and aspect substantially influence local microclimate and soil moisture variability [Dobrowski 2011].  As a result, they directly and indirectly constrain trees’ growing environments, influencing demographic rates and exposure to disturbance, and ultimately shaping stand structure, composition, and function [@kane_water_2015]. Even in low-diversity forests, physiognomy can vary dramatically with small changes in position. This variability is often especially pronounced in mountainous terrain, owing to the potential for large changes in elevation, slope, and aspect over small horizontal distances [Dobrowski 2011].

In complex terrain, pronounced gradients of insolation, precipitation input, and subsurface water distribution influence plant demographic processes, including productivity, biomass accumulation, and recruitment and mortality. Topography can also influence disturbance frequency and magnitude, drought and temperature stress, wind exposure, in turn influencing biotic structure and composition. General trends are assumed [@mcnab_terrain_1989; @mcnab_topographic_1993], *but quantitative reporting is limited in the literature* [TODO: verify]. In general, topographic factors can  be important determinants of forest structure and composition, and elevation, aspect, and position on the hillslope matter most. Stem diameter, basal, area, and growth rates decline with elevation, with temperature as the key limiting control. The same properties also decline from valley to ridge positions, and from polar to equatorial exposures, perhaps as a result of these factors' influence on soil moisture and vapor pressure deficit [@mcnab_terrain_1989; @mcnab_topographic_1993; @bolstad_forests_2018]. 

While these patterns may describe general relationships, there is evidence that they can vary in both magnitude and direction across watersheds and landscapes [Kelsey et al. 2018 [TODO: fix cite]]. @lydersen_topographic_2012 reported contrary findings in a Sierra Nevada mixed conifer forest, where upper slopes had both the highest quadratic mean diameter (QMD) and the tallest trees. @kane_water_2015, furthermore, found that topography explained little variance in forest structure in a domain with a frequent fire return interval.

### 1.1.2. Forest composition and topography
Topography may also exert control on species mix [Rowe and Sheard 1981; Barnes et al. 1982; Bailey 1988 [TODO: fix cites]]. Species affinities for certain topographic positions may be attributable to functional strategies developed in response to variability in radiative (Monin et al. 2007; White and Millet 2008) and hydrologic [Whittaker 1956, Day and Monk 1988; Hawthorne and Miniaat 2018 [TODO: fix cites]] regimes. In subalpine forests of the southern Rocky Mountains (SRM), some clear topography-driven controls on species occurrence exist. Engelmann spruce (*Picea engelmannii*) and subalpine fir (*Abies lasiocarpa*) tend to co-occur in high densities throughout the subalpine zone (2700–-3000 m a.s.l.) and only sparsely in the upper montane zone (1850-2900 m a.s.l.). At middle and high elevations up to treeline, the longer-lived spruce is often the canopy dominant (70 percent of canopy basal area), while fir may occupy up to the same proportion of the understory (Alexander et al. 1984). Near treeline, pure spruce stands are common, while fir often dominate the canopy in the lower end of the subalpine zone, particularly in xeric topographic positions (Alexander 1987). Douglas fir (*Pseudotsuga menziesii*) tend to dominate mesic sites, including north-facing toe-slopes and high-elevation south-facing slopes. Lodgepole pine (*Pinus contorta*) also occurs on dry, southerly upper slopes in the lower range of the subalpine zone, and abundantly throughout the montane zone, particularly on south-facing slopes and steep slopes of all aspects (Veblen 1986). 

### 1.1.3. Forest structure and soil
Soil properties also matter for structural development. Parent material in the top 10 cm of soil explained a greater share of variation in the abundance of trees across global biomes than any other single factor, including climate and topography (Delgado-Baquerizo et al. 2020). Parent material is also a significant explainer of ecosystem productivity. [TODO: more on soil and forest structure from conifer forests in montane, subalpine zones].

### 1.1.4. Forest composition and soil 
[TODO: more on soils from conifer forests in montane, subalpine zones, focusing on soil composition, available water capacity, organic matter]

### 1.1.5. Forest structure, composition, and climate
In forest ecosystems, gradient analysis has consistently identified temperature and water balance as the strongest abiotic factors explaining vegetation species distributions and emergent properties such as canopy structure and carbon density (Veblen 1986, Urban et al. 2000, Hessberg et al. 2007, Holden and Jolly 2011). Elevation-driven lapse rate is assumed to exert the strongest control on tree growth and stature, as well as biomass accummulation. Conifer height tends to decline with temperature and with precipitation (Swensen and Weiser, Hulshof et al. 2015). Yet, temperature and precipitation can be extremely heterogeneous in mountain domains with high topographic relief, and do not uniformly track elevation. A site's temperature and radiation regimes may be further regulated by exposure angle and shading by adjacent landforms, while its precipitation regime may be regulated by orogenic cloud formation, which can decouple local precipitation patterns from regional patterns [TODO: cite]. Moreover, variability in wind exposure leads to snow redistribution, yielding patterns of accumulation and melt that may differ from snowfall patterns ([TODO: citation, see ASO, Deems et al.]). [TODO: more on compositional relationships with climate].

## 1.2 Gaps and motivations
The majority of gradient analyses use elevation, convergence, or landscape position as proxies for temperature and soil moisture (Stephenson 1998; Ng et al. 2020). A smaller subset of studies have deployed more complex metrics that combine factors such as elevation, hillslope position, aspect, and slope into quasi-independent climate-proxy variables. Urban and colleagues (2000) used elevation, slope aspect, topographic convergence, and soil depth to model a “physical template” describing the light, temperature, and soil moisture regime of a Sierra Nevada domain, and then examined the sensitivity of model-estimated forest stand basal area, fuel loading, and canopy depth to the topographic inputs. Underwood and colleagues (2010) used elevation, slope, aspect, solar radiation, and topographic wetness to divide a Sierra Nevada domain into “landscape management units” representing nine clusters of topographic variability, and examined variation in stem and species density across those units. Their effort relied on data collected *in situ* from 164 transects. 

First, while plot- and transect-based data can provide reliable estimates of aboveground forest structure and composition when scaled up to a stand, these data are by nature limited in spatial extent and do not represent the full range of state-factor variability that may constrain the distribution of vegetation across a landscape (Hurtt et al. 2004, Antonarakis et al. 2011, @lydersen_topographic_2012, Antonarakis et al. 2014). Even within mature, close-canopied forests, characteristics such as stand density, age-class distribution, allometry, species composition, and species dominance can have wide variance. Efforts to scale these properties up to a watershed from plot observations (or plot-benchmarked models) alone can yield substantial error terms. Therefore, prior work may have failed to capture important dimensions of co-variability. Kane et al. (2015) and Bolstad et al. (2018) are the only studies we have identified that evaluate spatially continuous measures of topography and forest structure, although more results have been reported from tropical forests (e.g. Chadwick and Asner 2015, Jucker et al. 2018). 

A further concern is that the statistical modeling strategies used in prior studies are unable to quantify potential interactions among topographic variables. Factors such as elevation, hillslope, and curvature work together to define a site’s edaphic, radiative, and moisture environments. Failing to account for these interactions may amount to a significant oversight. In part because of these limitations, ecology still lacks a complete accounting of how forest physiognomy co-varies with state factors.

LiDAR integrated with field sampling [and hyperspectral remote sensing] holds promise for overcoming some of these limitations. Advances in active remote sensing, including in Light Detection and Ranging (LiDAR), have opened up new opportunities for characterizing forest structure on a continuous basis for a wide array of scientific and management applications (Mallet and Bretar 2009). In particular, over the past five years a profusion of full waveform LiDAR datasets from aerial and satellite platforms, and emerging open-source libraries for cleaning and processing the data, has enabled more accurate estimates of forest structure than those from discrete-return acquisitions (Zhou and Popescu 2017). Like discrete-return points, waveform data can be used to delineate individual canopy trees and to estimate individual-scale characteristics such as stem diameter, stem height, stem volume, and crown volume (Dalponte et al. 2011, Jucker et al. 2017). Waveforms can also be processed to generate continuous estimates of forest structure parameters at the pixel-grid scale; these parameters include aboveground biomass, leaf-area index, total number density, and diameter-class distribution. While some researchers have eschewed individual tree object--based approaches because of the difficulty of characterizing subcanopy structure with dicrete-return data, a profusion of new algorithms aimed at waveform and hyper-dense point clouds has made it increasingly possible to achieve individual-based structure estimates. Using the full waveforms appears to improve the accuracy of both object-oriented and continuous-estimate methods over discrete-return estimates, particularly for characterizing mid-canopy and sub-canopy structure. Integrating waveforms with imaging spectrometry and calibrating remote sensing against *in situ* stem diameter and height measurements yields further accuracy improvements (Antonarakis et al. 2011, Jucker et al. 2017).




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


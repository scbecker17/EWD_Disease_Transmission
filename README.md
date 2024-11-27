# Becker, et al. 2024 (Ecology Letters)

Investigating transmission of a recently emerged fungal disease (Nannizziopsis barbata) among a population of Eastern Water Dragons (Intellagama lesueurii) using spearate host behaviours to model direct (social) and indirect (environmental) disease transmission.

Data comes from obersvational surveys of a populaiton of dragons recording the GPS locations of where individual dragons were sighted on specific dates at specific times. 

Analysis is undertaken using the R-INLA package to account for spatial and temporal autocorrelation in the data.

The code was initially written by Greg Albery and Sam Becker.

## Script structure:

- All the code can be found in the `R` folder.
  
- Initial raw data from obervational surveys (Dragon_Sight_Data.csv), and data ready for INLA analysis (INLA_Data.csv) can be found in the `Data` folder.

- The scripts are numbered in order and contain the following:

1a. Generate host spatial behavioural metric to model indirect transmission of fungal disease.
1b. Generate host social behavioural metric to model direct transmission of fungal disease.
2. Run INLA model to investigate how each of the host behavioural metrics explain fungla disease transmission in this population.
3. Produce figures.

If you have any questions at all about the approach, feel free to email [Sam Becker](mailto:sam.becker@uq.edu.au).

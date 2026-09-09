# Code and Data Repository for "The Potential Impact of Personalized AI Assistants on Collective Intelligence"

This repository contains the code and data required to reproduce the analyses presented in the manuscript.

Running the R scripts in the `Simulations/` folder generates `.RData` files containing the simulation results. The scripts in the `Plots/` folder use these simulation outputs to generate the figures presented in the main text and Supplementary Information, which are saved in the `Figures/` folder.

---

## Folder Structure

### `Figures/`

Contains figures used in the main text and Supplementary Information.

### `Functions/`

Contains R scripts defining the functions used in the simulations.

### `Paper/`

Contains the `.tex`, `.pdf`, and `.bib` files for the manuscript, Supplementary Information, and references.

### `Plots/`

Contains R scripts used to generate figures from the simulation results stored in the `Simulations/` folder.

### `Simulations/`

Contains R scripts for running the simulations and the resulting simulation outputs. The scripts in this folder call the functions defined in the `Functions/` folder.

---

## Functions

### `Chatbot_AI.R`

Contains functions for simulations using the chatbot AI model. In-place updating and periodic resynchronization are implemented to reduce computational cost.

### `Omniscient_AI.R`

Contains functions for simulations using the omniscient AI model. In-place updating and periodic resynchronization are implemented to reduce computational cost.

### `Without_AI.R`

Contains functions for simulations without AI. In-place updating and periodic resynchronization are implemented to reduce computational cost.

---

## Simulations

### `Figure2,S1,S3,S5_simulation.R`

Runs the simulations for Figures 2, S1, S3, and S5. The script performs eight simulations using different combinations of AI models and aggregation rules.

### `Figure3_simulation.R`

Runs the simulations for Figure 3. The script performs 144 simulation runs across different values of lambda (the strength of the incentive and penalty associated with AI use) and AI bias.

### `Figure4_simulation.R`

Runs the simulations for Figure 4. The script performs one simulation for each of the three incentive structures—feedback, niche-expert, and balanced—using the chatbot AI model.

### `FigureS2,S5_simulation.R`

Runs the simulations for Figures S2 and S5. The script performs 52 simulation runs across AI bias values ranging from -0.6 to 0.6 under four combinations of AI models and incentive structures.

### `FigureS4_simulation.R`

Runs the simulations for Figure S4. The script performs 228 simulation runs: four simulations for the trajectory plots and 224 simulations for the interest-diversity heatmaps. Simulations vary the mutation rate, initial belief standard deviation, and AI bias.

### `FigureS6_simulation.R`

Runs the simulations for Figure S6. The script performs 39 simulation runs under three incentive structures (feedback, niche-expert, and balanced), with AI bias varying from -0.6 to 0.6.

### `FigureS7_simulation.R`

Runs the simulations for Figure S7. The script performs nine simulation runs under the balanced incentive structure, with the balancing weight varying from 0.1 to 0.9.

### `FigureS8_simulation.R`

Runs the simulations for Figure S8. The script performs 390 simulation runs under the balanced incentive structure, with AI bias varying from -0.6 to 0.6. For each bias level, 30 replicate simulations are performed with random initialization.

# An interactive gaze-data analysis tool for quantifying nystagmus
The application allows the user to upload an EyeLink data file (.edf) and perform analysis.
It allows the user to rescale data and then analyse sections which will output saccadic frequency and amplitude.
1. The user can then select which parts of the data file they would like to plot. Trial, eye, and axis of movement.
2. The user can first define +-10 degrees of visual angle with 2 sliders so that the data can be rescaled from raw pixel values into degrees of visual angle.
3. The user can then define a region of the plot to analyse.
4. Once a region is defined the user can use the "analyse" button to output frequency and average amplitude of saccades.

# Further work
1. Add ability to switch between trials and perform analysis - The calculation for different files was not working as intended when originally implemented.
2. Add ability for user to define a velocity/acceleration threshold for saccade detection.
3. Add slow phase detection and calculate whether accelerating/decellerating.
4. Adding a "back" or "undo" button for better user experience.
5. Make application run in offline environments.  

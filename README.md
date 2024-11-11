# SA-VME-Spatial-Prioritization
South African VME Spatial Prioritization

Four types of VME datasets: VME_indicator, VME_catch, VME_visual, and PotentialVME_visual. 

The challenge I’m facing with running PrioritizR is that I’m getting identical outputs for each solution, and presolve checks indicate that "more features are needed because most planning units don’t contain any features". Unfortunately, this is all the VME data we currently have, so I’m hoping there might be a workaround. 

At the recent IMCC workshop, it was recommended that each cell should have a value, which led me to consider adding the Marine Ecosystem Map (MEM.shp) with a 0% target. However, I haven’t managed to incorporate this just yet. 

I was also wondering if current protection shouldn't be considered? The MPA shp file is also currently in the data folder but I haven't incorperated it into the code as of yet.

I’ve also included a few coral mound data points derived from sonar scans in the data folder; however, I haven’t successfully integrated them into the analysis yet.

You'll also find a tiff file for a habitat suitability model (LophReef_ModelData.tif) for desmophyllum I predicted off the west coast of South Africa, do you have any examples code/resources on how to incorporate model data from a tif file?

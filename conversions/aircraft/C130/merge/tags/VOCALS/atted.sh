#!/bin/sh

#
# For empty vars, where c6 file didn't exist, add basic attributes
# so ncplot, etc will not bomb trying to read file.
#
for file in `ls -1 VOCALSrf0[6-9].nc VOCALSrf1[0-9].nc`
do
    echo $file
    ncatted -a units,A1DCR_RPI,c,c,"counts" $file
    ncatted -a long_name,A1DCR_RPI,c,c,"Fast 2DC Raw Accumulation, Round Particles (per cell)" $file
    ncatted -a CellSizes,A1DCR_RPI,c,f,"5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 205, 215, 225, 235, 245, 255, 265, 275, 285, 295, 305, 315. , 325, 335, 345, 355, 365, 375, 385, 395, 405, 415, 425, 435, 445, 455, 465, 475, 485, 49 5, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 605, 615, 625, 635" $file
    ncatted -a units,C1DCR_RPI,c,c,"#/L" $file
    ncatted -a long_name,C1DCR_RPI,c,c,"2D-C Concentration, Round Particles (per cell)" $file
    ncatted -a CellSizes,C1DCR_RPI,c,f,"5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 205, 215, 225, 235, 245, 255, 265, 275, 285, 295, 305, 315.  f, 325, 335, 345, 355, 365, 375, 385, 395, 405, 415, 425, 435, 445, 455, 465, 475, 485, 49 5, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 605, 615, 625, 635" $file
    ncatted -a units,A1DCA_RPI,c,c,"counts" $file
    ncatted -a long_name,A1DCA_RPI,c,c,"Fast 2DC Raw Accumulation, All Particles (per cell)" $file
    ncatted -a CellSizes,A1DCA_RPI,c,f,"5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 205, 215, 225, 235, 245, 255, 265, 275, 285, 295, 305, 315.  f, 325, 335, 345, 355, 365, 375, 385, 395, 405, 415, 425, 435, 445, 455, 465, 475, 485, 49 5, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 605, 615, 625, 635" $file
    ncatted -a units,C1DCA_RPI,c,c,"#/L" $file
    ncatted -a long_name,C1DCA_RPI,c,c,"2D-C Concentration, All Particles (per cell)" $file
    ncatted -a CellSizes,C1DCA_RPI,c,f,"5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 205, 215, 225, 235, 245, 255, 265, 275, 285, 295, 305, 315.  f, 325, 335, 345, 355, 365, 375, 385, 395, 405, 415, 425, 435, 445, 455, 465, 475, 485, 49 5, 505, 515, 525, 535, 545, 555, 565, 575, 585, 595, 605, 615, 625, 635" $file
    ncatted -a units,DBAR1DCR_RPI,c,c,"um" $file
    ncatted -a long_name,DBAR1DCR_RPI,c,c,"2D-C Mean Particle Diameter, round particles" $file
    ncatted -a units,PLWC1DCR_RPI,c,c,"g/m3" $file
    ncatted -a long_name,PLWC1DCR_RPI,c,c,"2D-C Liquid Water Content, round particles" $file
    ncatted -a units,CONC1DCR_RPI,c,c,"#/L" $file
    ncatted -a long_name,CONC1DCR_RPI,c,c,"2D-C Total Concentration, round particles" $file
    ncatted -a units,DBAR1DCA_RPI,c,c,"um" $file
    ncatted -a long_name,DBAR1DCA_RPI,c,c,"2D-C Mean Particle Diameter, all particles" $file
    ncatted -a units,PLWC1DCA_RPI,c,c,"g/m3" $file
    ncatted -a long_name,PLWC1DCA_RPI,c,c,"2D-C Liquid Water Content, all particles" $file
    ncatted -a units,CONC1DCA_RPI,c,c,"#/L" $file
    ncatted -a long_name,CONC1DCA_RPI,c,c,"2D-C Total Concentration, all particles" $file
done

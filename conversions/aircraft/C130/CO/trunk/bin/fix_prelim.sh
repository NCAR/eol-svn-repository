#! /bin/csh

foreach file (*prelim.dat)
    echo "$file\n"
    sed -e "s/#Time/Time/" $file > $file.2
    sed -e "s/CO_ppbv/COMR_AL/" $file.2 > $file.3
    sed -e "s/-999/-32767/" $file.3 > $file
    rm $file.2 $file.3
end

set xrange [0:2.75]
set yrange [-10:60]
set xlabel "Alpha" 0,0
set ylabel "Percent Flagged Dubious" 0,0
set title "Wind Direction Sensitivity Tests - STORMWAVE 950401 0600-1055"
set xtics (0,0.25,0.5,0.75,1.0,1.25,1.5,1.70)
set label "Test4 - 50.5% Dubious (alpha=0.25) [~27deg]" at 0.3,50.5
set label "Test3 - 33.4% Dubious (alpha=0.50) [~50deg]" at 0.6,33.4
set label "Test2 - 22.6% Dubious (alpha=0.75) [~80deg]" at 0.8,22.6
set label "Est - 18% Dubious (alpha=0.85) [~90deg]" at 0.9,18.0
set label "Test1 - 7.9% Dubious (alpha=1.22) [~130deg]" at 1.26,8.3
set arrow 1 from 1.70,0.0 to 1.90,0.0
set label "UNDEFINED" at 1.95,0.0
set nokey
plot "flagged_dubious.dat" using 1:2 with linespoints

#!/bin/tcsh

echo "linking events..."
./link_events.sh ml_read dmg_merged_ml zith_move zith9_dev
echo ""
echo "linking sites..."
./link_sites.sh ml_read dmg_merged_ml zith_move zith9_dev
echo ""
echo "linking categories..."
./link_category.sh ml_read dmg_merged_ml zith_move zith9_dev
echo ""
echo "linking platforms..."
./link_platform.sh ml_read dmg_merged_ml zith_move zith9_dev
echo ""
echo "done."



cat smalldata | sed -e 's/#[0-9]* @ \([0-9]*\),\([0-9]*\): \([0-9]*\)x\([0-9]*\)/\1 \2 \3 \4/' | awk '{ print "(" $1, ($1 + $3), - $2, - ($2 + $4) ")"}'
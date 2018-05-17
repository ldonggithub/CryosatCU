
    export YEAR=`cat sISvat.ctr`
  if [ -f Output__ETH.ferret.nc ]; then
    rm -f Output__ETH.ferret.nc
  fi

  if [ -f ETH_${YEAR}.gif ]; then
    rm -f ETH_${YEAR}.gif
  fi

    export      GRAPH=ETHplt
    echo "go "${GRAPH}".JNL" $1 >& ${GRAPH}.jnl
    echo "exit"                 >> ${GRAPH}.jnl
    TOUCH                          ${GRAPH}.jnl
    ferret -gif                  < ${GRAPH}.jnl
    rm                               ferret.jnl*

  if [ -f Output__ETH.ferret.nc ]; then
    TOUCH Output__ETH.ferret.nc
  fi

  if [ -f ETH_${YEAR}.stat ]; then
    TOUCH ETH_${YEAR}.stat
  fi

  if [ -f ETH_${YEAR}.gif ]; then
    TOUCH ETH_${YEAR}.gif
    xv    ETH_${YEAR}.gif
  fi

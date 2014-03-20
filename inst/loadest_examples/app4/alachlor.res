#
#  Residual output file
#
#  notes
#  -----
#  DTIME     decimal time minus "center" of decimal time
#  LN(CFLOW) natural log of (uncentered) streamflow
#  F         flag indicating observation is censored (C) or uncensored (U)
#  CCONC     observed concentration for F=U; 1/2 of the observed concentration for F=C
#  CCONCAML  estimated concentration
#  YHATC     estimated natural log of concentration
#  CLOAD     observed load for F=U; 1/2 of the observed load for F=C (units dependent on ULFLAG)
#  CLOADAML  estimated load (units dependent on ULFLAG)
#  YHAT      estimated natural log of load (where load is in kg/d)
#  RESID     difference between observed and estimated values of log load (or log concentration)
#  Z         z-score for residual
#
#
#DATE    TIME  DTIME        LN(CFLOW)   F CCONC       CCONCAML     YHATC       CLOAD       CLOADAML     YHAT         RESID        Z
#
19921019 1200 -1.00107E+00  8.36404E+00 U 1.60000E-02 1.15291E-02 -5.06029E+00 3.70225E-01 2.66772E-01 -2.70932E+00  9.25128E-01  8.57103E-01
19921118 1145 -9.19130E-01  1.03546E+01 U 4.70000E-02 2.70969E-02 -4.22179E+00 7.96005E+00 4.58920E+00  1.19705E-01  1.16418E+00  1.02714E+00
19921221 1130 -8.28995E-01  8.95803E+00 U 2.60000E-02 8.69981E-03 -5.32708E+00 1.08964E+00 3.64602E-01 -2.38212E+00  1.67742E+00  1.35996E+00
19930112 1145 -7.68771E-01  1.05891E+01 U 4.70000E-02 3.44879E-02 -3.97840E+00 1.00641E+01 7.38491E+00  5.97641E-01  9.20791E-01  7.80818E-01
19930218 1300 -6.67258E-01  9.13884E+00 U 1.30000E-02 2.72966E-02 -4.18565E+00 6.52802E-01 1.37071E+00 -1.05987E+00 -1.57153E-01 -2.75496E-01
19930330 1400 -5.57555E-01  9.94750E+00 U 3.40000E-02 1.59378E-01 -2.46433E+00 3.83278E+00 1.79665E+01  1.47011E+00 -9.17063E-01 -9.38732E-01
19930412 1400 -5.21939E-01  1.03514E+01 U 2.50000E-02 2.99712E-01 -1.82849E+00 4.22059E+00 5.05984E+01  2.50982E+00 -1.86039E+00 -1.35996E+00
19930426 1300 -4.83697E-01  9.83628E+00 U 1.80000E-02 2.96383E-01 -1.85342E+00 1.81553E+00 2.98940E+01  1.96979E+00 -2.16396E+00 -1.51269E+00
19930503 1400 -4.64405E-01  9.72913E+00 U 1.50000E-01 3.18686E-01 -1.78386E+00 1.35922E+01 2.88776E+01  1.93221E+00 -1.13261E-01 -1.08947E-01
19930510 1500 -4.45112E-01  9.80918E+00 U 7.00000E-02 3.81834E-01 -1.60302E+00 6.87160E+00 3.74830E+01  2.19309E+00 -1.05624E+00 -1.12441E+00
19930517 1430 -4.25991E-01  9.65503E+00 U 9.30000E-02 3.81591E-01 -1.60724E+00 7.82521E+00 3.21079E+01  2.03472E+00 -7.67915E-01 -7.08829E-01
19930524 1300 -4.06984E-01  9.12151E+00 U 7.40000E-01 2.86150E-01 -1.89840E+00 3.65209E+01 1.41222E+01  1.21004E+00  1.59730E+00  1.23364E+00
19930601 1300 -3.85067E-01  8.75306E+00 U 1.70000E-01 2.35269E-01 -2.09080E+00 5.80418E+00 8.03260E+00  6.49189E-01  3.18843E-01  2.75496E-01
19930607 1500 -3.68400E-01  9.24956E+00 U 5.40000E-01 3.44196E-01 -1.71706E+00 3.02911E+01 1.93076E+01  1.51943E+00  1.10088E+00  9.38732E-01
19930614 1400 -3.49336E-01  9.18605E+00 U 2.10000E+00 3.32442E-01 -1.75258E+00 1.10550E+02 1.75007E+01  1.42040E+00  2.49452E+00  2.01952E+00
19930621 1400 -3.30158E-01  9.44145E+00 U 1.20000E+00 3.89281E-01 -1.59061E+00 8.15531E+01 2.64559E+01  1.83777E+00  1.77293E+00  1.51269E+00
19930628 1230 -3.11151E-01  9.12260E+00 U 6.70000E-01 3.00688E-01 -1.85305E+00 3.31023E+01 1.48559E+01  1.25649E+00  1.45257E+00  1.12441E+00
19930706 1400 -2.89062E-01  9.96176E+00 U 2.80000E-01 4.85211E-01 -1.34462E+00 3.20171E+01 5.54823E+01  2.60407E+00  7.16558E-02  1.08947E-01
19930712 1430 -2.72567E-01  9.31470E+00 U 2.80000E-01 2.93323E-01 -1.87260E+00 1.67637E+01 1.75613E+01  1.42903E+00  5.99636E-01  6.40341E-01
19930719 1400 -2.53446E-01  9.32367E+00 U 1.20000E-01 2.64085E-01 -1.97582E+00 7.24916E+00 1.59533E+01  1.33479E+00 -1.44447E-01 -2.19332E-01
19930726 1430 -2.34210E-01  8.71440E+00 U 5.50000E-02 1.52108E-01 -2.53087E+00 1.80663E+00 4.99641E+00  1.70463E-01 -3.69547E-01 -3.90753E-01
19930802 1430 -2.15032E-01  8.41848E+00 U 3.40000E-02 1.05980E-01 -2.88558E+00 8.30741E-01 2.58946E+00 -4.80165E-01 -4.95819E-01 -5.11531E-01
19930809 1600 -1.95683E-01  8.17752E+00 U 2.70000E-02 7.51317E-02 -3.21973E+00 5.18444E-01 1.44265E+00 -1.05528E+00 -3.92188E-01 -4.50314E-01
19930816 1300 -1.76847E-01  8.84072E+00 U 3.10000E-02 1.01335E-01 -2.93286E+00 1.15539E+00 3.77681E+00 -1.05196E-01 -5.40913E-01 -6.40341E-01
19930823 1330 -1.57612E-01  1.00774E+01 U 3.70000E-02 1.90582E-01 -2.26121E+00 4.74971E+00 2.44651E+01  1.80317E+00 -1.03563E+00 -1.02714E+00
19930830 1330 -1.38434E-01  8.80986E+00 U 5.40000E-02 6.69770E-02 -3.34284E+00 1.95145E+00 2.42041E+00 -5.46039E-01  4.24065E-01  4.50314E-01
19930907 1400 -1.16459E-01  1.00301E+01 U 1.80000E-02 1.20241E-01 -2.72628E+00 2.20387E+00 1.47220E+01  1.29078E+00 -1.29111E+00 -1.23364E+00
19930920 1400 -8.08429E-02  8.93853E+00 C 1.00000E-03 3.93097E-02 -3.87037E+00 4.11001E-02 1.61563E+00 -9.44908E-01 -2.78564E+00 -2.01952E+00
19931012 1400 -2.05689E-02  9.48797E+00 U 1.90000E-02 3.10926E-02 -4.10040E+00 1.35275E+00 2.21370E+00 -6.25493E-01  1.37083E-01  1.63877E-01
19931118 1150  8.05536E-02  1.13840E+01 U 2.20000E-02 5.20601E-02 -3.50298E+00 1.04304E+01 2.46821E+01  1.86791E+00 -3.13732E-01 -3.32563E-01
19931123 1200  9.42713E-02  1.16351E+01 U 4.80000E-02 5.75252E-02 -3.38280E+00 2.92555E+01 3.50610E+01  2.23927E+00  3.46250E-01  3.32563E-01
19931130 1330  1.13621E-01  1.03189E+01 U 2.30000E-02 2.37358E-02 -4.35702E+00 3.75888E+00 3.87913E+00 -5.11808E-02  5.84757E-01  5.74740E-01
19931215 1215  1.54574E-01  9.87303E+00 U 2.10000E-02 1.67828E-02 -4.70593E+00 2.19740E+00 1.75613E+00 -8.45970E-01  8.42700E-01  7.08829E-01
19940112 1245  2.31343E-01  9.35010E+00 U 8.00000E-03 1.43195E-02 -4.84128E+00 4.96222E-01 8.88206E-01 -1.50425E+00  1.29680E-02  1.01007E-07
19940215 1230  3.24465E-01  9.51783E+00 U 1.10000E-02 3.33810E-02 -4.00247E+00 8.06901E-01 2.44865E+00 -4.97714E-01 -5.07387E-01 -5.74740E-01
19940305 1215  3.73752E-01  9.50301E+00 U 1.20000E-02 5.50736E-02 -3.50792E+00 8.67310E-01 3.98049E+00 -1.79735E-02 -9.14931E-01 -8.57103E-01
19940411 1300  4.75207E-01  1.01425E+01 U 5.80000E-02 2.53565E-01 -2.00115E+00 7.94603E+00 3.47385E+01  2.12829E+00 -8.46163E-01 -7.80818E-01
19940509 1330  5.51977E-01  1.04073E+01 U 3.70000E-01 5.56741E-01 -1.20374E+00 6.60569E+01 9.93961E+01  3.19048E+00  2.09491E-01  2.19332E-01
19940523 1330  5.90333E-01  9.36734E+00 U 3.00000E-01 3.36414E-01 -1.73708E+00 1.89320E+01 2.12299E+01  1.61719E+00  5.33111E-01  5.11531E-01
19940606 1430  6.28803E-01  8.62694E+00 U 1.70000E-01 2.20497E-01 -2.15433E+00 5.11648E+00 6.63629E+00  4.59547E-01  3.82374E-01  3.90753E-01
19940620 1430  6.67159E-01  8.38480E+00 U 8.50000E-02 1.85691E-01 -2.32329E+00 2.00808E+00 4.38684E+00  4.84503E-02 -1.41816E-01 -1.63877E-01
19940705 1330  7.08141E-01  8.75147E+00 U 6.70000E-01 2.17351E-01 -2.17671E+00 2.28392E+01 7.40914E+00  5.61700E-01  1.77623E+00  1.71205E+00
19940718 1500  7.43929E-01  8.30894E+00 U 7.20000E-02 1.31790E-01 -2.66675E+00 1.57669E+00 2.88600E+00 -3.70877E-01  3.56603E-02  5.43755E-02
19940827 1130  8.53118E-01  7.56528E+00 C 1.00000E-03 2.91467E-02 -4.12205E+00 1.04099E-02 3.03414E-01 -2.56984E+00 -2.56458E+00 -1.71205E+00
19940912 1230  8.97068E-01  7.53369E+00 U 9.00000E-03 1.75900E-02 -4.61183E+00 9.07763E-02 1.77417E-01 -3.09121E+00 -9.86967E-02 -5.43755E-02
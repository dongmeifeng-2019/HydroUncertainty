del *.mod
del *.obj
ifort variables_v1.f90 inputdata_v1.f90 outputdata_v1.f90 excesscalc_v2.f90 qgroundwater_kw_v1.f90 qsubsurface_kw_v1.f90 qsurface_kw_v1.f90 qchannel_mc_v1.f90 hillsloperouting_v1.f90 riverrouting_v1.f90 errorcal_v2.f90 main_v1.f90 -o run.exe
  

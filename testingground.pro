pro inflections, array,  WIDTH=width, res
  local_min, array, 15, mins
  res = []
  local_max, array, 15, maxs
  
  dslice = median(deriv(mins))
  stop
  
  for i=0,mins.length-1 do begin
    if i eq mins.length -1 then continue

    di = mins[i+1] - mins[i] ; spacing between indecies of detected local minima
    tolerance = 3 ; deviation in slice width across all slices (not always 20pixels wide, somtimes 22 wide)
    
    !null =  where(abs(mins[i+1] - mins[i]) eq [dslice-tolerance:dslice+tolerance], count)
    if count gt 0 then res = [res, mins[i], mins[i+1]]
  endfor
  
  
  
  i=plot(array)
  i=plot(res, array[res], 'r*', /overplot)
end 

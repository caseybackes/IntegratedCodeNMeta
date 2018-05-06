pro local_min, array, dx, mins
  mins = []
  for x=0,array.length-dx-1 do begin
    ;if total(array[x] lt array[x-width:x-1]) eq width and total(array[x] lt array[x+1:x+width]) eq width then begin
    if array[x] eq min(array[(x-dx lt 0? 0:x-dx): (x+dx gt array.length-1? array.length-1:x+dx)]) then mins =[mins,x]
  endfor
  
end 
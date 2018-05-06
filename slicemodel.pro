PRO gfunct, X, A, F, pder
;  bx = EXP(A[1] * X)
;  F = A[0] * bx + A[2]
; A = [max, mean, sigma, +constant]
  bx = exp(-(x-A[1])^2/A[2])
  F = A[0] * bx + A[3]
;  IF N_PARAMS() GE 4 THEN $
;    pder = [[bx], [A[0] * X * bx], [replicate(1.0, N_ELEMENTS(X))]]
end

pro slice_peaks, array, pks
  local_max,array, 15, maxs
  dslice =  median(deriv(maxs))
  tolerance = 3 ; deviation in slice width across all slices (not always 20pixels wide, somtimes 22 wide)
  pks = []
  for i=0,maxs.length-1 do begin
    if i eq maxs.length-1 then break

    di = maxs[i+1] - maxs[i] ; spacing between indecies of detected local maxima
    if di gt dslice-tolerance and di lt dslice+tolerance then pks = [pks, maxs[i], maxs[i+1]]
    
  endfor
  pks = pks[uniq(pks)]
end
pro slicemodel, array, WIDTH=width, res

  
  
  local_min, array, 15, mins
  res = []
  local_max, array, 15, maxs

  dslice = median(deriv(mins))
  ;stop

  for i=0,mins.length-1 do begin
    if i eq mins.length-1 then break

    di = mins[i+1] - mins[i] ; spacing between indecies of detected local minima
    tolerance = 3 ; deviation in slice width across all slices (not always 20pixels wide, somtimes 22 wide)

    !null =  where(abs(di) eq [dslice-tolerance:dslice+tolerance], count)
    if count gt 0 then res = [res, mins[i], mins[i+1]]
  endfor

  for i = 0, maxs.length-1 do begin
    if i eq maxs.length-1 then break
    
    di = maxs[i+1] - maxs[i]
    tolerance = 3; deviation in slice width across all slices. 
    !null = where(abs(di) eq [dslice-tolerance:dslice+tolerance], count)
    if count gt 0 then res = [res, maxs[i], maxs[i+1]]

  endfor
  res = res[uniq(res)]
  i=plot(array)
  i=plot(res, array[res], 'r*', /overplot)


  if array[res[0]] gt array[res[1]] then print, "upper bound of top slice is not detected"
  
  
  sm = array[res[1]:res[2]]
  stop



  x = findgen(sm.length)
  y = sm
  weights = 1.0/sqrt(y)
  A =  [max(sm)-min(sm),sm.length/2.0,30,min(sm) ]; = [max, mean, sigma, +constant] 
  yfit = gaussfit(x,sm, A, NTERMS = 4)
  bestline = A[0]*exp(-(x-A[1])^2/A[2])+ A[3]
  !null = plot(bestline, /overplot)
  


end



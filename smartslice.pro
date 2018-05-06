pro smartslice, image_array, slice_width, slice_bounds
  

  imgsize = image_array.dim
  xsize = imgsize[0]
  ysize = imgsize[1]
  slice_indices = [] ; an array to contain slice indices
  ;array = median(image_array[xsize/2-5:xsize/2+5,*], dim = 1)
  array = median(image_array, dim = 1)




  ; Remove non-zero bias of noise around spectral image area  
  array -=min(array)
  
  ; Get rid of any remaining noise in the slice profile array
  ;array = smooth(smooth(array,2),2)
  array =array/(max(array))
  fit =[]
  sb = []
  for slice_width=18,45 do begin
    
  
    x = findgen(slice_width *10+1)
    model =  -0.5*cos(2.0*!PI*(x mod slice_width)/slice_width)+0.5
  
    model_slice_bounds = slice_width*findgen(11)
    model_topo = model-cos(2*!PI*x/(max(x)))*.5+1
    ; account for brightness variance across spatial latitudes
    model_norm = model_topo/max(model_topo)*max(array)
    
    
    r2 = []
    for i=0,array.length - model_norm.length do begin
      r2 = [r2,RSS(array[i:i+model_norm.length-1], model_norm)]
      ;p = plot(array[i:i+model_norm.length-1], 'r')
      ;p = plot(model_norm, 'b', /overplot)
      ;stop
    endfor
    !null = min(r2, min_r2_index)
    offset = where(r2 eq min(r2))
    offset = offset[0]
    model_slice_bounds = model_slice_bounds +offset
  
    ;p = plot(array)
    ;p = plot([offset:offset+model_norm.length],model_norm, /overplot, 'r--', title =string("R Sqrd = " + string(r2(where(r2 eq min(r2))))))
    ;p = plot(model_slice_bounds, array[model_slice_bounds],'r*', /overplot)
    ;print, "Slice bounds are: "
    ;print, model_slice_bounds
    fit = [fit,r2[min_r2_index]]
    sb=[[sb],[[model_slice_bounds]]]
    ;stop
 endfor
 ;stop
 slice_bounds = sb[*,where(fit eq min(fit))]
 slice_width = 18+where(fit eq min(fit))
 print, 'slice bounds found to be '
 print, slice_bounds
 print, 'with slice width of ', slice_width
 
end
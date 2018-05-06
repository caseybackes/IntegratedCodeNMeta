;+
; image_array is the reduced and flat image
; NAME        : 
;   slicebounds
; 
; PURPOSE     : 
;   allow user to define where the slice boundaries are in the reduced spectral slice image
;   by clicking on an interactive plot window that shows the mean of the image in the spectral 
;   direction (shows mean slices). IDL finds the local minimum data point within a
;   range of uncertainty (10 pixels) in order to get the most accurate position for the slice boundary. 
;   
; EXPLANATION: 
;   NONE
;
;-





pro slicebounds, image_array, slice_indices
imgsize = image_array.dim
xsize = imgsize[0]
ysize = imgsize[1]
slice_indices = [] ; an array to contain slice indices
;array = median(image_array[xsize/2-5:xsize/2+5,*], dim = 1)
array = median(image_array, dim = 1)

window, 1
;plot, array, yrange = [median(array)-2*stddev(array),median(array)+2*stddev(array)],title= "Define slice boundaries: left click = bound, right click = FINAL SLICE BOUND"
plot, array, title= "Define slice boundaries: left click = bound, right click = FINAL SLICE BOUND", $ 
  yrange = [0,2*median(array[array.length*.3:array.length*.6])]
xyouts,100,20,"Left click : Approximate min - IDL finds best min wihtin 10 pixels.", /device
xyouts,100,10,"Right click: Exact slice boundary", /device

print, "Select the local minima that are aprx evenly spaced. IDL will automatically find the true local min at each spot."
for n = 0, 10 do begin ; there are no more than 10 slices in any given image, 
  cursor, index, notused, /data, /down ; y component of click location is discarded.
  
  ; for a 'right click', the slice boundary is taken as the exact point (integer x coord) selected.
  if !mouse.button eq 4 then begin 
    slice_bound = round(index) ;descrete integer pixel location
    ; make a line at that spot
    vline, slice_bound
    ; add it to the slice indices array
    slice_indices = [slice_indices, slice_bound]
    ; and if this iteration is the last one needed, !mouse.button will eq 4, and we can exit the for loop
    ;if !mouse.button eq 4 then break 
  endif
  
  ; for a 'left click', the slice boundary is searched for within 10 pixels left and right 
  ; of the point (x coord) selected. 
  if !mouse.button eq 1 then begin 
    ; index value will be float, discretize pixel locations by rounding 
    index= round(index)
    ; get the pixel location of the local minimum
    !null= min(array[index-5:index+5], local_min_position) 
    ; readjust for location within entire observed slice plot
    slice_bound = index - 5 + local_min_position
    ; make a line at that spot
    vline, slice_bound
    ; append that index to the slice indices array
    slice_indices = [slice_indices, slice_bound]
  endif else if !mouse.button eq 2 then break
  
endfor
wdelete,1
return
end

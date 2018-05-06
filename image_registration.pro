pro image_registration, image_array, sheared_image , transformation_matrix
  
  ; initial values:
  original_img = image_array ; for comparison to final sheared (registered) image
  image_size = image_array.dim
  image_width = image_size[0]
  image_height = image_size[1]
;  cost_function_x = []
;  cost_function_y = []

  
  ; determine the transformation coefficients for the 'warp_tri' function
  ;; determine the dx coefficients
  
;  for i =0,20 do begin
;    left_edge = median(image_array[0:50,slice_bounds[0]:slice_bounds[-1]],dim=1)
;    right_edge = median(image_array[-50:-1,slice_bounds[0]:slice_bounds[-1]],dim=1)
;    cost_function_y = [cost_function_y, RSS(left_edge, right_edge)]
;    if i gt 3 then begin
;      decent  =-1.0* (cost_function_y[-1] - cost_function_y[-2])
;      ;print,"neg gradient of cost_function_y = ", decent
;      ddy = decent/(mean(cost_function_y[-3:-1]))
;      dy +=ddy*3
;    endif else dy+=.5
;    xi= [0,image_width - 1, image_width-1, 0 ]
;    xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
;    yi= [0,0,image_height-1, image_height-1]
;    yo= [dy, 0, image_height-1, image_height-1+dy ]
;    image_array = warp_tri(xo,yo,xi,yi, original_img)    
;  endfor
;  
;  for i = 0,20 do begin
;    top_slice = median(image_array[*,slice_bounds[-2]:slice_bounds[-1]], dim = 2)
;    bottom_slice = median(image_array[*,slice_bounds[0]:slice_bounds[1]], dim = 2)
;    cost_function_x = [cost_function_x, RSS(top_slice, bottom_slice)]
;    if i gt 1 then begin
;
;      ;;make the dx determination as good as the dy
;      decent = -1.0*(cost_function_x[-1] - cost_function_x[-2])
;      ddx = decent/mean(cost_function_x[-3:-1])
;      ; apply the change to dx (ddx) in the way of similar triangles
;      dx+=(1.0*ddx/(1.0*(slice_bounds[-1]-slice_bounds[0])))*slice_bounds[-1]
;      ;stop
;
;    endif else dx +=float(where(top_slice eq max(top_slice)) - where(bottom_slice eq max(bottom_slice)))
;    xi= [0,image_width - 1, image_width-1, 0 ]
;    xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
;    yi= [0,0,image_height-1, image_height-1]
;    yo= [dy, 0, image_height-1, image_height-1+dy ]
;    image_array = warp_tri(xo,yo,xi,yi, original_img)
;  endfor
;  
  
;  for i =0,20 do begin
;    
;    ;identify the left and right edges of the image along the spatial direction, only looking in the 
;    ;window of the slice region (not the noise region above and below the good data)
;    left_edge = median(image_array[0:50,slice_bounds[0]:slice_bounds[-1]],dim=1)
;    right_edge = median(image_array[-50:-1,slice_bounds[0]:slice_bounds[-1]],dim=1)
;    
;    ;identify where the emission line is in the top and bottom slice by taking
;    ;the mean across the spatial direction (up/down) of the top slice, and do the same for the bottom slice.
;    top_slice = median(image_array[*,slice_bounds[-2]:slice_bounds[-1]], dim = 2)
;    bottom_slice = median(image_array[*,slice_bounds[0]:slice_bounds[1]], dim = 2)
;    cost_function_x = [cost_function_x, RSS(top_slice, bottom_slice)]
;    cost_function_y = [cost_function_y, RSS(left_edge, right_edge)]
;
;    
;    ;;; determine the ddx amount
;    if i gt 1 then begin
;      ;ddx = float(where(top_slice eq max(top_slice)) - where(bottom_slice eq max(bottom_slice)))
;      ;;;; apply the similar triangles method to determine shear coefficient dx
;      ;dx +=(1.0*ddx/(1.0*(slice_bounds[-1]-slice_bounds[0])))*slice_bounds[-1]
;      
;      ;;make the dx determination as good as the dy
;      decent = -1.0*(cost_function_x[-1] - cost_function_x[-2])
;      ddx = decent/mean(cost_function_x[-3:-1])
;      dx+=ddx
;      ;stop
;      
;    endif else dx +=float(where(top_slice eq max(top_slice)) - where(bottom_slice eq max(bottom_slice)))
;    ;;; determine the ddy amount
;    ; determine derivative of the cost_function_y at the last two points
;    if i gt 3 then begin
;      decent  =-1.0* (cost_function_y[-1] - cost_function_y[-2])
;      ;print,"neg gradient of cost_function_y = ", decent
;      ddy = decent/(mean(cost_function_y[-3:-1]))
;      dy +=ddy*3
;    endif
;
;    ; shear the image by some amount
;    xi= [0,image_width - 1, image_width-1, 0 ]
;    xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
;    yi= [0,0,image_height-1, image_height-1]
;    yo= [dy, 0, image_height-1, image_height-1+dy ]
;    image_array = warp_tri(xo,yo,xi,yi, original_img)
;    ;stop
;    ;p.erase
;
;  endfor

dy = -5.0
dx = 0
dy_array = [dy]
r_sqrd = []

right = mean(image_array[image_width-10,*,*], dim = 1)
left = mean(image_array[0:10,*], dim = 1)
r_sqrd = [r_sqrd, RSS(left, right)]



while dy lt 11.0 do begin
  shear, image_array,0,dy,res
  left = mean(res[0:10,*], dim = 1)
  right = mean(res[image_width-10,*,*], dim = 1)
  r_sqrd = [r_sqrd, RSS(left, right)]
  dy+=0.1
  dy_array = [dy_array, dy]

endwhile
stop

; where did the best fit occur within the stated dy bounds? 
!null = min(r_sqrd, index)
dy = dy_array[index]

; with the best determined dy shift, find the best dx shift



xi= [0,image_width - 1, image_width-1, 0 ]
xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
yi= [0,0,image_height-1, image_height-1]
yo= [dy, 0, image_height-1, image_height-1+dy ]

sheared_image = warp_tri(xo,yo,xi,yi, image_array)
transformation_matrix = reform([xi,xo,yi,yo],4,4)
sheared_image = warp_tri(transformation_matrix[0],yo,xi,yi, image_array)


print, "[dx,dy] = ", dx, dy
atv, sheared_image
stop

end
  

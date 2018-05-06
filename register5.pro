pro register5, image_array, slice_bounds, sheared_image , transformation_matrix

original_img = image_array ; for comparison to final sheared (registered) image
image_size = image_array.dim
image_width = image_size[0]
image_height = image_size[1]



; we need a part of the spectrum to center on for the alignment of the spectral rows
window, 1
plot, mean(image_array, dim = 2),$ 
  title = "Click on D2 emsn or other faint feature in spectrum"
cursor, x,notused,/data,/down
wdelete,1

ddy = 0.00 & dx = 0   ; arbitary numbers that are not zero
ddx = 0.00 & dy = 0 
;while ddx ne '' and ddy ne '' do begin
w1 = window(dimensions = [1400,500]) 
w1.window.setcurrent
repeat begin
  
  ; show the left and right edges of the image
  left_edge= mean(image_array[0:image_width/10,slice_bounds[0]:slice_bounds[-1]], dim = 1)
  right_edge = mean(image_array[image_width-image_width/10:image_width-10, slice_bounds[0]:slice_bounds[-1]], dim = 1)
  ;residuals = [] & for i=0,left_edge.length-1 do residuals= [residuals,(left_edge[i]-right_edge[i])^2]
  ;residuals = total(residuals)
  
  ;;supercedes lines 27,28:
  residuals_lr = RSS(left_edge, right_edge)
  
  plot_left_edge = plot(left_edge, color = 'red', name = 'left edge', title = string("Slice Alignment :current RSS ="+string(residuals_lr)), layout = [2,2,1], /current)
  plot_right_edge = plot(right_edge, color = 'blue', name = 'right edge', /overplot, layout = [2,2,1])
  
  pleg = legend(target = [plot_left_edge, plot_right_edge], position = [730,450], /device)

  ;show the top and bottom slices (may later decide to include a middle slice)
  top_slice =    mean(image_array[x-20:x+20,slice_bounds[-1]-20:slice_bounds[-1]], dim = 2)
  bottom_slice = mean(image_array[x-20:x+20,slice_bounds[0]: slice_bounds[1]], dim = 2)
  
  residuals_tb = RSS(top_slice, bottom_slice)

  plot_top_slice = plot(top_slice, color = 'blue', name = 'top slice', title= "Spectral Alignment :current RSS ="+string(residuals_tb), /current, layout = [2,2,3])
  plot_btm_slice = plot(bottom_slice, color = 'red', name = 'bottom slice', /overplot, layout = [2,2,3])
  pleg2 = legend(target = [plot_top_slice, plot_btm_slice], position =[750,200], /device )
  
;  if isa(image_array) then i = image(image_array, $
;    min_value= mean(image_array)-2*stddev(image_array),$
;     max_value = mean(image_array)+2*stddev(image_array), /current, $
;     layout = [2,1,2])
;  if not isa(image_array) then i = image(original_img, $
;     min_value= mean(original_img)-2*stddev(original_img),$
;     max_value = mean(original_img)+2*stddev(original_img), /current, $
;     layout = [2,1,2])
;  
  
  ; enter the adjustment values of dy to shear the left edge of the image up and down - aligns slices to be more horizontal
  ; enter the adjustment values of dx to shear the bottom edge of the image left and right - aligns spectral lines to be vertical 
  read, ddy, prompt = "Shift the SLICE alignment (RED line TOP plot) by how much?  "
  read, ddx, prompt = "Shift the SPECTRAL alignment (RED line BOTTOM plot) by how much?  "
  ;if isa(plot_right_edge) then plot_right_edge.delete
  w1.erase
  
  ; adjust the dx,xy (using a 'similar triangles' method)
  dx +=(1.0*ddx/(1.0*(slice_bounds[-1]-slice_bounds[0])))*slice_bounds[-1]
  dy +=ddy

  
  ; generate the transformation matrix that acts as the argument for the 'wap_tri' 
  ; procedure that shears the image; 
  xi= [0,image_width - 1, image_width-1, 0 ]
  xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ] 
  yi= [0,0,image_height-1, image_height-1]
  yo= [dy, 0, image_height-1, image_height-1+dy ] 
  ;if dx eq 0.0 and dy eq 0.0 then break
 
  ; apply transformation matrix to the original image. If applied iteratively, the interpolation between iterations 
  ; will continue add up and the data that remains will be far from the original (not visually noticable, but not 
  ; as accurate as we want it).  So we apply the updated tranformation to the original image, and update this 
  ; transformation with each iteration - as though we are saying "oops, i meant to shear by 1 more in the x do up date the 
  ; shear to 3.5 in the x."
  image_array = warp_tri(xo,yo,xi,yi, original_img)
  if isa(im) then im.erase
  im = image(image_array[*,slice_bounds[0]:slice_bounds[-1]], $
    min_value= mean(image_array)-2*stddev(image_array),$
     max_value = mean(image_array)+2*stddev(image_array), /current, $
     layout = [2,1,2])
  ;foreach sb,slice_bounds do po = plot(findgen(image_width,1), fltarr(image_height)+sb, /current, layout= [2,1,2], /overplot)
endrep until (ddx eq 0 and ddy eq 0 )

transformation_matrix = [  [xo],[yo],[xi],[yi]  ]
sheared_image = image_array ; has been registered a few lines above
mindisplayval = mean(sheared_image[*,slice_bounds[0]:slice_bounds[-1]]) - 2*stddev(sheared_image[*,slice_bounds[0]:slice_bounds[-1]])
maxdisplayval = mean(sheared_image[*,slice_bounds[0]:slice_bounds[-1]]) + 2*stddev(sheared_image[*,slice_bounds[0]:slice_bounds[-1]])
;im = image(original_img, title = "Before Registration", layout = [1,2,1], min_value = mindisplayval, max_value = maxdisplayval)
;im2 = image(sheared_image, title = "After Registration", layout = [1,2,2], /current,min_value = mindisplayval, max_value = maxdisplayval)

closewin
return
end


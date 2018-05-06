pro imreg_x, image_array, slice_indices,imregd, transformation_matrix
  dims = image_array.dim
  image_width = dims[0]
  image_height = dims[1]

  dy = 0
  dx = -10
  dx_array = [dx]
  r_sqrd = []

  top = median(image_array[*, slice_indices[-2]:slice_indices[-1]], dim = 2)
  bottom = median(image_array[*, slice_indices[0]:slice_indices[1]], dim = 2)
  

  r_sqrd = [r_sqrd, RSS(top, bottom)]
  while dx lt 10 do begin
    shear, image_array,dx,0,res
    top = median(res[*, slice_indices[-2]:slice_indices[-1]], dim = 2)
    bottom = median(res[*, slice_indices[0]:slice_indices[1]], dim = 2)
    r_sqrd = [r_sqrd, RSS(top, bottom)]
    dx+=0.1
    dx_array = [dx_array, dx]

  endwhile
  

  ; where did the best fit occur within the stated dx bounds?
  !null = min(r_sqrd, index)
  dx = dx_array[index]
  
  xi= [0,image_width - 1, image_width-1, 0 ]
  xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
  yi= [0,0,image_height-1, image_height-1]
  yo= [dy, 0, image_height-1, image_height-1+dy ]

  sheared_image = warp_tri(xo,yo,xi,yi, image_array)
  transformation_matrix = reform([[xo],[yo],[xi],[yi]], 4,4)


  top = mean(sheared_image[*, slice_indices[-2]:slice_indices[-1]], dim = 2)
  bottom = mean(sheared_image[*, slice_indices[0]:slice_indices[1]], dim = 2)
  plt = plot(top)
  plt = plot(bottom, /overplot, 'r', title = "Best found fit: red is bottom slice")

  print, "[dx,dy] = ", dx, dy
  atv, sheared_image
  ;stop

end
pro imreg_y, image_array, imregd, transformation_matrix
  dims = image_array.dim
  image_width = dims[0]
  image_height = dims[1]
  
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
  ;stop

  ; where did the best fit occur within the stated dy bounds?
  !null = min(r_sqrd, index)
  dy = dy_array[index]

  ; with the best determined dy shift, find the best dx shift



  xi= [0,image_width - 1, image_width-1, 0 ]
  xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
  yi= [0,0,image_height-1, image_height-1]
  yo= [dy, 0, image_height-1, image_height-1+dy ]

  sheared_image = warp_tri(xo,yo,xi,yi, image_array)
  transformation_matrix = reform([[xo],[yo],[xi],[yi]], 4,4)

  print, "[dx,dy] = ", dx, dy
  atv, sheared_image
  

end
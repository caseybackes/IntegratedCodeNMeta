pro shear, img, dx,dy, result

  image_dims = img.dim
  image_width = image_dims[0]
  image_height = image_dims[1]
  

  xi= [0,image_width - 1, image_width-1, 0 ]
  xo= [xi[0]+dx, xi[1]+dx, xi[2],xi[3] ]
  yi= [0,0,image_height-1, image_height-1]
  yo= [dy, 0, image_height-1, image_height-1+dy ]
  result = warp_tri(xo,yo,xi,yi, img)
end
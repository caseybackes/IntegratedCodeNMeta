function flatfield, img, skyimg
   
  size= img.dim
  x_size=size[0]
   
  sky_summed_over_spectra=transpose(median(skyimg, dim=1))
  flat_field_array=sky_summed_over_spectra
   
  ;build flat field
  for k=1,x_size-1 do flat_field_array=[flat_field_array,sky_summed_over_spectra]
   
  ;divide original image by synthetic flat field image
  img/= flat_field_array
   
  return, img
end
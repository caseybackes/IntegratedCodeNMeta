pro fit_spectral_wavelength, img, mins
  dims = img.dim
  img_width = dims[0]
  img_height = dims[1]
  spectrum = mean(img, dim =2)
  mins = []
  
  for i=15,img_width-16 do begin
    if spectrum[i] le min(spectrum[i-15:i-1]) then begin
      if spectrum[i] le min(spectrum[i+1:i+15]) then mins = [mins, i]
    endif
  endfor
end
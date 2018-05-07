pro fit_spectral_wavelength, img, mins, ABSORPTION_LINE_WIDTH = absorption_line_width
  if not keyword_set(ABSORPTION_LINE_WIDTH)then absorption_line_width = 8 
  ;print, "width of search set to ", absorption_line_width," pixels of spectral data."
  ;stop
  dims = img.dim
  img_width = dims[0]
  img_height = dims[1]
  spectrum = mean(img, dim =2)
  mins = []
  
  for i=absorption_line_width,img_width-absorption_line_width-1 do begin
    ; if spectral brightness in question at particular wavelength is less than that of its neighbors to the left:
    
    if spectrum[i] le min(spectrum[i-absorption_line_width:i-1]) then begin
      if spectrum[i] le min(spectrum[i+1:i+absorption_line_width]) then begin
        ;stop
        mins = [mins, i]
      endif
    endif
  endfor

  ; the Fraunhofer line will certainly be detected in the sky spectrum. We want to exclude that absorption
  ; line from those identified in the sky spectrum, leaving only the water lines. We know the exact wavelengths
  ; of the telluric (Earth) waterlines, but the fraunhofer line is from the solar spectrum, which can vary 
  ; in wavelength due to Earth's motion (radial distance). If that motion is enough to move the fraunhofer 
  ; wavelength an appreciable amount from its true wavelength, I dont know, but because it isnt constant, we 
  ; exclude it and use only the water lines. I believe Rosemary also used the same argument agains the iron line, 
  ; claiming that it was from the solar spectrum, not a feature of atmospheric absorption. 
  absorption_wavelengths = float([$
      5885.9771, $  ; h2o
      5886.3363, $  ; h2o
      5887.2228, $  ; h2o
      5887.6600, $  ; h2o
      5888.7063, $  ; h2o
      5891.1760, $  ; h2o
      5891.6556, $  ; h2o
      5892.3937, $  ; h2o
      5892.8794] )  ; Iron 

  ; we can now test the best linear fit between the absorption wavelengths and columns of absorption lines 
  ; in our sky spectrum we have certainly found the fraunhofer line. we want to remove that from testing 
  ; a fit to the absorption lines. 
  
  likely_fline = where(spectrum[mins] eq min(spectrum[mins]))
  remove, likely_fline, mins
  l = findgen(mins.length)
  stop
  chsq = []
  for i=1,4 do begin
    coeff=linfit(absorption_wavelengths[l],mins, CHISQR = r)
    chsq = [chsq,r]
    print,"l = ", l
    print, "chisqr = ", r
    l = l+i
  endfor
  plt = plot(chsq, title="X2 fit as a function of delta el")
  ;coeff=linfit(absorption_wavelengths[l],mins, CHISQR = r)




  coeff_polyline = poly_fit(absorption_wavelengths[l],mins, 2)
  
  
  
  for i=0,mins.length-1 do begin
    
  endfor
  
  
  
  
  plt = plot(spectrum)
  plt = plot(mins, spectrum[mins], /overplot, 'r*', title = "Identified absorption lines w/Frahnhofer")


end
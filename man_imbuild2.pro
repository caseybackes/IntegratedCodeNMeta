pro man_imbuild2, img, slice_bounds, d2_start, d2_end, cont_start, cont_end, final_solar_spectrum, filename, emission_image, cont_image, SHOW=show, SHOWPLOTS=showplots
;starttime = systime(1)
cont_start = fix(cont_start[0]) & cont_end = fix(cont_end[0])
; make blank arrays that will fit the emission and continuum reflectance images
emission_image = fltarr(10,slice_bounds.length-1)
cont_image = emission_image; a copy of blank image array. 

; get the original dimensions to the input image
imgsize = img.dim
xsize = imgsize[0]
ysize = imgsize[1]

; determine the number of slices to iterate through
number_of_slices = slice_bounds.length - 1 ; for n slices, there are n+1 bounds

; keep track (visually) of where the slice integrations are starting and stopping
;plt = plot(mean(img[*,slice_bounds[0]:slice_bounds[-1]], dim = 1))
obs_slices = mean(img, dim = 1)

;print, "Building image from spectra:"

;plt = plot(obs_slices, title = "Spectral Slice Integration Boundaries")
; For each slice and subslice, remap the continuum and emssion spectra into 2D pixel images
for J = 0 , number_of_slices-1 do begin ; using start index of zero requires us to shift down from number of slices to that number minus one. 
  ;print, "There will be ", number_of_slices-1, " slices "
  ;print, "Working slice number :", J+1
  ; grab the slice: * means all columns, slice_bounds[J] is the row where the slice starts, slice_bounds[J+1]-1 is where the slice ends
  slice_start = slice_bounds[J] & slice_end = slice_bounds[J+1]-1
  primary_slice = img[*, slice_start : slice_end]
  primary_slice_original_size = primary_slice.dim
  primary_slice_width = primary_slice_original_size[0]
  primary_slice_height = primary_slice_original_size[1]
  
  ;print, "Slice start index: ", slice_start, "   slice end index: ", slice_end
  ; Image the slice (not necessary)
  ;sliceimg = image(primary_slice, title = string("slice from " + string(slice_bounds[j]) + " to " +string(slice_bounds[j+1])))
  
  ; we want to use all the data in the slice, but if the slice is 19 pixels wide, and we divide it into ten subslices, 
  ; then each subslice will only by one pixel wide (19 pixels /10 pixels equals 1 pixel.) So we scale the image in the y-direction
  ; to make each subslice by 190 pixels by 10 pixels equals 19 pixels which is useful (as opposed to 1.9 pixels
  primary_slice_large = congrid(primary_slice, primary_slice_width, primary_slice_height*10)
  
  
  ;s =  symbol(slice_start, obs_slices[slice_start],'*',  sym_color = 'red',  /data)
  ;s2 = symbol(slice_end,   obs_slices[slice_end],  '*',  sym_color = 'blue', /data)
  ;print, "Star at " , slice_start, ",",obs_slices[slice_start]
  ;print, "Star at " , slice_end, ",",obs_slices[slice_end]

  ; foreach subslice...
  for subslice=0,9 do begin
    ;print, "Working subslice number ", subslice+1
    subslice_start = subslice*primary_slice_height 
    subslice_end = ((subslice+1)*primary_slice_height - 1)
    subslice_region = primary_slice_large[*,subslice_start: subslice_end]
    
    
    ;   integrate a continuum region ( same width as d2 width)
    cont_intensity = total(subslice_region[cont_start:cont_end, *])
    
    
    ;   build continuum image -> ;      cont_image[ subslice_i,  primary slice_i]=total_reflected_light
    cont_image[subslice, j] = cont_intensity
    
    
    ;   scale solar spectrum to subslice merc spectrum ( scale2match.pro)
    mean_merc_spectrum = mean(subslice_region, dim = 2)
    d2_start = fix(d2_start) & d2_end = fix(d2_end)
    d2_peak = d2_start + (d2_end - d2_start)/2

    if keyword_set(showplots) then begin    
      if (d2_start - 100) lt 0 then pltstart = 0 else pltstart = d2_start-100
      if (d2_end + 100) gt final_solar_spectrum.length then pltend = final_solar_spectrum.length-1 else pltend = d2_end + 100
    ;stop
    endif   
      scaled_solar_spectrum = scale2match(final_solar_spectrum, mean_merc_spectrum,d2_peak, d2_start)
    if keyword_set(showplots) then begin    
      pl = plot(mean_merc_spectrum, xrange = [pltstart,pltend], yrange= [-.1,1.3],color = 'red', /overplot, layout = [2,1,1])
      pl = plot(scaled_solar_spectrum,xrange = [pltstart,pltend], yrange= [-.1,1.3],color = 'blue', /overplot,layout = [2,1,1])
      p2  = plot(mean_merc_spectrum - scaled_solar_spectrum,/current, xrange =[pltstart,pltend],yrange= [-.1,1.3],  layout = [2,1,2])
      p5  = plot([d2_start:d2_end],mean_merc_spectrum[d2_start:d2_end] - scaled_solar_spectrum[d2_start:d2_end], /overplot, /fill_background, fill_color ='yellow')
    endif 
   
    if subslice eq 5 then closewin
    ;   subtract the scaled solar spectrum from merc spectrum resulting in isolated d2 emission
    ;   integrate isolated d2 emission for this subslice (bounds need to be worked out exactly in terms of array location of start and stop of d2)
    isolated_d2 = total(subslice_region[d2_start:d2_end, *]) - total(scaled_solar_spectrum[d2_start:d2_end, *])
    
    ;   build emission image -> emsn_image[subslice_i, primary slice_i ] total isolated d2 emission
    emission_image[subslice, j] = isolated_d2

  endfor ; subslices
  
  
endfor ; primary slices
if keyword_set(show) then begin
  !null = image(emission_image, rgb_table = 13, layout = [2,1,2], title = string("Emission Image  "+file_basename(filename)))
  !null = image(cont_image,     rgb_table = 13, layout = [2,1,1], title= "Reflectance Image", /current)
  if (cont_end-cont_start) gt 1.2*(d2_end - d2_start) then begin
    ;stop
    t = text(100,100,string("* continuum range is "+strcompress(cont_end -cont_start+1)+" pixels"), /device)
    t = text(100,80,string("* D2 range is "+strcompress(d2_end-d2_start+1)+" pixels"), /device)
  endif
endif

END 
;+
; Give it the sky image that has been dark-reduced, and define a varible to hold the 
; observed wavelength scale as seen in the image. 
;-
;

pro determine_wavelength_scale, sky_bright, slice_indices, metadir, observed_wl


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 5) Determine the wavelength values for pixels of the registered image.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;stop
  ; we use the sky image to identify water absorption lines to which we already know the
  ; associated wavelengths.
  mean_sky_array = mean(sky_bright[*,slice_indices[0]:slice_indices[-1]], dim = 2)

 
  window, 1, xsize = 1200
  plot, mean_sky_array , /data, $
    title = 'Wavelength Calibration: Identify 3-5 Absorption Lines', $
    charsize = 2;, xrange= [0,200]

  xyouts, 150,0,'[ Click Down Here When Finished! ]', /device, charsize = 3.5
  xyouts, [mean_sky_array.length*.7,mean_sky_array.length*.7], [max(mean_sky_array)*.5,max(mean_sky_array)*.4], ["Click close to minima of teluric water lines, IDL will find best min.", "A line is drawn where the value is found to be best."]


  ; you must select at least three lines to get a decent linear fit of wavelength across
  ; the spectrum.Try to click on the minima of the water lines. However, sometimes only three
  ; water lines are visible. If you select all that ; you can see, then click below the
  ; y=0 line in the plot and the collected lines will be used for calculating the linfit
  ; for the wavelength.

  ; since we wont likely pick the exact correct absorption line, let IDL find the min of the
  ; mean_sky_array plot *around* where you clicked: within 6 or so pixels of your selection.
  uncertainty = 10

  lambdas = [] ; put the pixel locations of water lines in this empty array.
  cursor, pos1, notused, /down, /data
  pos1 = round(pos1)
  !null = min(mean_sky_array[(pos1-uncertainty):(pos1+uncertainty)], index)
  lambda1 = round(pos1-uncertainty+index)
  lambdas  = [lambdas, lambda1]
  print, "Pos1 was ",pos1, ' and lambda1 is calculated to be ', lambda1
  vline, lambda1
  wait, .1
  cursor, pos2, notused, /down, /data
  pos2 = round(pos2)
  !null = min(mean_sky_array[(pos2-uncertainty):(pos2+uncertainty)], index)
  lambda2 = round(pos2-uncertainty+index)
  lambdas  = [lambdas, lambda2]
  print, "Pos2 was ",pos2, ' and lambda2 is calculated to be ', lambda2

  vline, lambda2
  wait, .1
  cursor, pos3, notused, /down, /data
  pos3 = round(pos3)
  !null = min(mean_sky_array[(pos3-uncertainty):(pos3+uncertainty)], index)
  lambda3 = round(pos3-uncertainty+index)
  lambdas  = [lambdas, lambda3]
  print, "Pos3 was ",pos3, ' and lambda3 is calculated to be ', lambda3

  vline, lambda3
  wait, .1

  ; anything more than three lines is awesome, it will help get a more accurate line fit
  ; to the wavelength scale calculation.
  cursor, pos4, notused, /down, /data
  pos4 = round(pos4)

  ; if pos 4 lt 0, then dont put to lambda array, and dont ask for a fifth
  if notused gt 0 then  begin
    !null = min(mean_sky_array[(pos4-uncertainty):(pos4+uncertainty)], index)
    lambda4 = round(pos4-uncertainty+index)
    lambdas  = [lambdas, lambda4]
    print, "Pos4 was ",pos4, ' and lambda4 is calculated to be ', lambda4

    vline, lambda4
    wait, .1
    cursor, pos5, notused, /down, /data
    pos5 = round(pos5)
    if notused gt 0 then  begin
      !null = min(mean_sky_array[(pos5-uncertainty):(pos5+uncertainty)], index)
      lambda5 = round(pos5-uncertainty+index)
      lambdas  = [lambdas, lambda5]
      print, "Pos5 was ",pos5, ' and lambda5 is calculated to be ', lambda5

      vline, lambda5
      wait, 1
    endif
  endif
  ;wait, .1

  print,lambdas.length, " water lines have been identified."




  ; Sometimes it easier to do this waterline identification when you have the picture to match the spectrum against and know which line has which code.
  ; I have a png file with the identified lines and their codes, in addition to having one pinned to the wall of my office cubicle.
  ; this will ask you if you want to see a png of such a sky spectrum with waterlines.
  d = dialog_message("Do you need to see the image of waterlines and their codes? ", /question)
  if d eq 'Yes' then begin
    waterlines_image = image(read_png(file_search(metadir, "*waterlines*")))
  endif


  ; Now we associate each of the water lines we selected. From the image
  ; of identified water lines (currently a paper stapled to the wall of my cubicle at work),
  ; enter the ID codes of the water lines (*IN ORDER YOU CLIKED ON THEM*) separated by commas.
  ; "The first one I clicked on was the 5885.9771 line, so I enter '0', followed by the 5886.3363
  ; line so I put a comma and the code '1'...
  ; IDL> Codes to absorbption lines : 0,1,2,3,4
  absorption_wavelengths = float([$
    5885.9771, $  ; h2o
    5886.3363, $  ; h2o
    5887.2228, $  ; h2o
    5887.6600, $  ; h2o
    5888.7063, $  ; h2o
    5889.9661, $  ; Fraunhofer - dont click on this one.
    5891.1760, $  ; h2o
    5891.6556, $  ; h2o
    5892.3937, $  ; h2o
    5892.8794] )  ; Iron 

  ; print to the shell the wavelengths of known water lines and their codes
  print, 'index       wavelength'
  counter = 0
  foreach aw,absorption_wavelengths do begin
    print, counter, '  ', aw
    counter +=1
  endforeach

  if lambdas.length eq 3 then begin
    read, index0,index1,index2, prompt = 'Codes to absorption lines: '
    my_absorptions = [absorption_wavelengths[index0], absorption_wavelengths[index1],absorption_wavelengths[index2]  ]
  endif else if lambdas.length eq 4 then  begin
    read, index0,index1,index2,index3, prompt = 'Codes to absorption lines: '
    my_absorptions = [absorption_wavelengths[index0], absorption_wavelengths[index1],absorption_wavelengths[index2],absorption_wavelengths[index3]  ]
  endif else if lambdas.length eq 5 then  begin
    read, index0,index1,index2,index3,index4, prompt = 'Codes to absorption lines: '
    my_absorptions = [absorption_wavelengths[index0], absorption_wavelengths[index1],absorption_wavelengths[index2],absorption_wavelengths[index3],absorption_wavelengths[index4]  ]
  endif
  wdelete,1
  


  ; From the lines you have identified and the codes you gave each of them, a linear line is
  ; fit to determine the slope and intercept  of the wavelengths in the image. The intercept is the first
  ; wavelength that appears in the image, and the slope is the incremental change in wavelength from one
  ; pixel to the next. 'coeff' is a 2D array with [slope, intercept]
  coeff=linfit(lambdas,my_absorptions)
;  print, coeff
  
;  coeff_polyline = poly_fit(lambdas, my_absorptions, 2)
;  print, coeff_polyline
  
  ; FINALLY, create an array of the observed wavelength based on the 'coeff's calculated above

  observed_wl = findgen(mean_sky_array.length, increment = coeff[1], start = coeff[0])
  
  return
  end



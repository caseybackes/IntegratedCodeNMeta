;+
; PURPOSE:  To search multiple parameter space to fit the full center image to a a model. 
; PARAMETERS: Seeing - gaussian blur in increments of 0.2  arcsec. 
;             Normalization factors - scales up the ADU vaues of the model to that of the reduced observation 
;                 based on the max values of each image (observation and model).
;             Image center offset - difference in merdian loaction between the model and the observation. 
;
;Phase Angle, in degrees
;Mercury Diameter, in arcseconds
;Defect of Illumination, in arcseconds
;Mercury's Helocentric distance, in AU
; 


;- 


pro convolve_and_compare3, ephemeris_structure, continuum_reflectance_image, pixel_size,appearant_solar_position,delta_lambda, $ ; inputs
  image_pixel_multiplier, estimated_seeing, model_img , normalization_factor, imcenter_offset,  eq_slice_array, eq_slice_pos; outputs

  phase_ang = ephemeris_structure.phi & phase_ang = phase_ang[0]
  ang_diam = ephemeris_structure.angular_diameter & ang_diam = ang_diam[0]
  def_illum = ephemeris_structure.def_illum & def_illum = def_illum[0]
  r_AU = ephemeris_structure.MERC2SUN_RANGE & r_AU = r_AU[0]

  print, "Mercury Angular Diameter is ", ang_diam, " arcseconds. "

  ; note the max seeing to simulate and the pixel size of the final images.
  plate_scale = pixel_size
  max_seeing_to_simulate = 6.



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Hapke calculation of the bidirectional reflectance function
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;dummy1=phase_angle & dummy2=ang_diam & dummy3=defect & dummy4=pixel_size ;the dummy variables are necessary because the Hapke model functions change the value of the function arguments
  ;bidir_refl_image = photometric_model(dummy1,dummy2,dummy3,dummy4)

  bidir_refl_image = photometric_model(phase_ang,ang_diam,def_illum, pixel_size)


  continuum_solar_flux_at_mercury = 4.965e14*(0.324^2./(r_AU^2.)) ;photons/cm2 s Angstrom, value comes from Domingue et al 1997
  ;and is for the region near the D2 line.
  ;this is scaled by distance from Mercury as inverse square law.
  ;If the continuum were measured in the Fraunhofer absorption, it
  ;wouldn't be so simple, we would have to worry about Doppler shift as well

  ;bidir_refl_image *= continuum_solar_flux_at_mercury*4.0*!pi*1.0e-9 ;kR/Angstrom
  ;bidir_refl_image *= continuum_solar_flux_at_mercury ;kR/Angstrom
  ;this expression also comes from Domingue et al 1997



  ;; determine from the ephemeris metadata if mercury is trailing or leading the sun.
  ;; this will tell us what side of the observed continuum image the sun should be on.
  ;; if the sun is (from our observing perspective on earth) on the left of mercury, then
  ;; mercury is "leading" and the 'bidir_refl_image' needs to be flipped in the x direction.
  ;; the second half of this line below accounts for the ephemeris data being of array type
  ;; of length one, and we want it to be a string, so we extract it with 'item[0]'
  ;ld_tr = ephemeris_structure.slashr & ld_tr = ld_tr[0]
  ;if ld_tr eq '/L' then bidir_refl_image=reverse(bidir_refl_image, 1)

  ; dont flip the data to match the model, flip the model to match the data.
  ; with the input parameter 'appearent_solar_position' flip the model to match the data.
  if appearant_solar_position eq 'L' then begin
    print, "flipping L/R orientation of model to accomodate leading geometry."
    bidir_refl_image=reverse(bidir_refl_image, 1)
  endif





  ; the four parameters for some reason are changed after being used in the
  ; photometric model. So we have avoided this problem by passing the ephemeris
  ; structure into this procedure, and now recall the variables used in the
  ; photometric model. Each is of type 'array', so we 'extract' the one element that is 
  ; in each 1x1 array. ( super damn annoying... thanks, IDL.) 
  phase_ang = ephemeris_structure.phi & phase_ang = phase_ang[0]
  ang_diam = ephemeris_structure.angular_diameter & ang_diam = ang_diam[0]
  def_illum = ephemeris_structure.def_illum & def_illum = def_illum[0]
  r_AU = ephemeris_structure.MERC2SUN_RANGE & r_AU = r_AU[0]


  ; to find where the peak intensity is in terms of arseconds from the center of the disk
  ; and compare this location to the observed equitorial slice brightness as a function of
  ; distance from the center of its disk.
  peak_intensity =[]
  peak_intensity_location = []



  ; using the LIST type data structure, we
  ; the KEYS:
  seeing_array= [findgen(max_seeing_to_simulate/0.2, increment = 0.2, start = 0.0)]
  ; the VALUES:
  ; this list is added to every time we get a new model equitorial reflectance array :)
  eq_ref_list = list()
  allmodels = list()
  ; when we get the eq reflectance arrays we do this:
  ; IDL> eq_ref_list.add, new_array



  ;get equatorial cross section for smeared disks:
;  w1=window(dimensions=[1000,200], location =[0,0])
;  w2=window(dimensions=[700,500], location = [0,280])
  seeing = 0. ;FWHM of seeing disk (arcsec)
  ;while seeing le max_seeing_to_simulate do begin
  foreach seeing,seeing_array do begin
    ;first get size of image,
    imagesize=size(bidir_refl_image)
    imagesize=imagesize[1]

    seeing_disk_sigma = ( seeing/(2.0*sqrt(2.0*alog(2.))) )/pixel_size
    kernel_width=seeing_disk_sigma*3.0
    if seeing gt 0 then begin
      smoothed_image=gauss_smooth(bidir_refl_image,seeing_disk_sigma,/edge_truncate, /nan);width=kernel_width,/edge_truncate)
    endif else begin
      smoothed_image=bidir_Refl_image
    endelse
;    w1.window.setcurrent
;    xpos=(seeing+1)/(max_seeing_to_simulate+2)


    ;model_iteration = smoothed_image[ imagesize/2-0.7*ang_diam/pixel_size:imagesize/2+0.7*ang_diam/pixel_size, imagesize/2-0.7*ang_diam/pixel_size:imagesize/2+0.7*ang_diam/pixel_size ]
    allmodels.add, smoothed_image
;    im=image(smoothed_image,/current , title = seeing,$
;      xrange=[imagesize/2-0.7*ang_diam/pixel_size,imagesize/2+0.7*ang_diam/pixel_size],$
;      yrange=[imagesize/2-0.7*ang_diam/pixel_size,imagesize/2+0.7*ang_diam/pixel_size], $
;      layout=[fix(max_seeing_to_simulate/0.5)+1,1,where(seeing_array eq seeing)+1], location = [0,0], rgb_table = 13)
;    ;im.position=[xpos,0.5]


    ;  im = image(smoothed_image, layout =[ max_seeing_to_simulate+1, 1, seeing+1], /current)
    ;  dialog=dialog_message(string("The size of the smoothed image is "+string(smoothed_image.dim)+" pixels"), /information)
    ;center of Mercury's disk will be in the center of image, so equatorial cross section will be taken across
    equatorial_reflectance=smoothed_image[*,imagesize/2] ;not that imagesize should always be even
    xvals= (findgen(imagesize)+0.5)*pixel_size - (imagesize/2)*pixel_size ;x coordinates of equatorial slice in arcsec
    peak_intensity = [peak_intensity, max(equatorial_reflectance)]
    peak_intensity_location = [peak_intensity_location,xvals[where(equatorial_reflectance eq max(equatorial_reflectance))]]



    ;come up with array of colors
;    loadct,13,rgb_table=rgb_table ;can use loadcsvcolorbar in place of loadct
;    colorindex=fix(seeing/max_seeing_to_simulate*255)
;    rgb_color=transpose(rgb_table[colorindex,*])

;    w2.window.setcurrent
;    if seeing ne 0 then p=plot(xvals,equatorial_reflectance ,/overplot, title = "Equitorial Reflectance",$
;      xtitle = 'Distance from Disk Center (arcsec)',ytitle='Relative kR per $\AA$ / constant',$
;      color=rgb_color,name=strtrim(string((seeing)),2)+' " FWHM', $
;      xrange = [-10,10],$
;      thick=2)

    eq_ref_list.add, equatorial_reflectance


  endforeach;endwhile
  ;l = legend(position = [-2,0.85*max(eq_ref_list[1])], /data)




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; DETERMINE THE EQITORIAL PIXEL ROW IN THE OBSERVATION

  ; When all of the arrays for the equtorial reflectance slices of each seeing model are created
  ; and stored in the list, we can merge this eq_ref_list with the seeing_array to make a dictionary called
  seeing_keys = strcompress("s"+string(seeing_array))
  seeing_keys= seeing_keys.replace(' ','') & seeing_keys = seeing_keys.replace('0', '')& seeing_keys = seeing_keys.replace('.', 'p')
  eq_ref = dictionary(seeing_keys,eq_ref_list )

  ; visually estimate where the eq slice is in the image, by row number (not index, the numbers will help)
  cont_size = continuum_reflectance_image.dim
  continuum_xsize = cont_size[0]
  continuum_ysize = cont_size[1]
  w5 = window()
  w5.window.setcurrent
  i = image(continuum_reflectance_image, layout = [1,1,1], rgb_table = 13, title ="Which pixel row is the equator?",/current)

  ;label pixel numbers for user identification ; contour
  for i=1,continuum_ysize do t=text(70,40+38*i,string(i-1), /device, font_size = 20)
  eq_slice_pos = ''

  read, eq_slice_pos, prompt = "Which pixel row is the equator?:  row "
  
;  ; TEST TEST TEST
;  h = []
;  FOR row = 0,9 do h=[h, total(continuum_reflectance_image[*,row])]
;  eq_slice_pos = where(h eq max(h))
;  print, "Equator located in row: ", eq_slice_pos

  ;stop
  ;eq_slice_pos -=1 ; this is now in IDL array index style where the first element is the zeroth element.
  eq_slice_array = continuum_reflectance_image[*,eq_slice_pos]
  ;mean_eqsl= mean(continuum_reflectance_image[*,eq_slice_pos-1:eq_slice_pos+1], dim =2)
  ;w5.close
  ; NEW FEATURE IN TEST: RUN A X2 FIT ON THE ENTIRE OBSERVATION AGAINST A CORRECTLY WINDOWED MODEL
  msrs = []
  mdl_list = list()
  normalization_factors = []
  foreach mdl,allmodels do begin
    ; proper window of the model that fits the size of the observation (10x10 pixels) and is centered on the peak emission pixel of the observation.
    ; this assumes the peak emission is at the center of a blurred disk. This will not work if the data is distorted to a very noisy level with
    ; "peak" emission in a seemingly random place on the observed image.
    mdla = mdl[where(mdl[*,128] eq max(mdl[*,128])) mod 256 - 5: where(mdl[*,128] eq max(mdl[*,128])) mod 256 +4, $
      128-5+(5-eq_slice_pos): 128+4+(5-eq_slice_pos)]
    ;mdl_list.add, mdla
    ;stop
    ;where(mdl eq max(mdl))/256-5+(5-eq_slice_pos):where(mdl eq max(mdl))/256+4+(5-eq_slice_pos)]
    ; normalize the two images to the same intensity. Do this by multiplying the model image by a constant, determined by the
    ; ratio of the peaks in the observation and the model.
    normalization_factors = [normalization_factors,(max(continuum_reflectance_image)/max(mdla))]
    mdla*=normalization_factors[-1]
    mdl_list.add, mdla
    ;if median(mdl eq allmodels[2]) eq 1 then stop
    measurements = 0
;    for coordx=0,9 do begin
;      for coordy =0,9 do begin
;        measurements += (mdla[coordx,coordy]-continuum_reflectance_image[coordx,coordy])^2/sqrt(continuum_reflectance_image[coordx,coordy])
;      endfor
;    endfor
    measurements = total(mdla - continuum_reflectance_image)^2/(mdla.length - 2)
    msrs =[msrs,measurements]
    if measurements lt msrs[-1] then best_mdl = mdla
  endforeach

  best_rigorous_seeing = seeing_array[where(msrs eq min(msrs))]
  ;model_img = mdl_list[where(msrs eq min(msrs))] & model_img = model_img[0]
  print, ''
  print, "Seeing by full-image model fit:"
  print, "Seeing:", best_rigorous_seeing
  print, "Best Reduced X2 Fit: ", round(min(msrs)) 
  ;print,strcompress(string(best_rigorous_seeing)), ", X2_red= ", round(min(msrs))
  ;stop

  ;;;;;; The old code for the compare and convolve stuff... 
;  ; DETERMINE THE PLATE SCALE FOR THE OBSERVATION
;
;  ; determine the pixel size scale in arcseconds in the observed continuum image
;  ; The plate scale depends on the size of the image (5" or 10" image) which is
;  obs_steps = findgen(continuum_xsize)*pixel_size
;  ;offset to put the center of the image at the 0" position
;  obs_steps-= (continuum_xsize/2)*pixel_size+pixel_size
;
;  obs_ang_distance = []
;  for i=0,9 do begin
;    ;  i=0
;    ;  i+=1
;    u = string(obs_steps[i]+1) & u=u.replace('00','') & u=strcompress(u)
;    obs_ang_distance = [obs_ang_distance,u]
;    ;t = text(140+40*i,30, string(u+'"'),/device, font_size = 10)
;  endfor
;
;  ; Now, which one of the seeing conditions does the observtion most match up to?
;  ; In order to find out we need to scale this equitorial slice up to match the
;  ; high intensities of the model conditions. We can compare each scaled version
;  ; of the equitorial slice to each of the seeing conditions. But there will be
;  ; an offset in the x-direction in the plot ( inspection of the gaussian blurred
;  ; plots of the equitorial brightness will show you that the peak of the brightness
;  ; moves closer to the limb with better (lower) seeing conditions), which tells us how
;  ; much to adjust the 'obs_steps' array, which will move the equitorial plot left or right, allowing
;  ; for a 'by eye' comparison. At this point, a chai square fit would probably be best
;  ; to determine which seening condition is most likely. We may want to move to half
;  ; arcsecond intervals to determine more accurate seeing conditions.
;
;  ; the measurement of how far each pixel is from the center of the disk - each
;  ; iteration of the model has different seeing and this changes where the peak intensity
;  ; is in terms of arcseconds from the center of the disk. For each iteration, this distance
;  ; for the observation is shifted to make the peak observed intensity location (in arcseconds
;  ; from the center of the disk) align to the peak model intensity location.
;  list_of_obs_ang_dist = list()
;  for i = 0,eq_ref_list.length-1 do begin
;    list_of_obs_ang_dist.add, obs_ang_distance+peak_intensity_location[i]
;  endfor
;
;
;  w2.window.setcurrent ;  back to the plot of seeing conditions.
;
;  normalization_factors = list()
;  ; lists for making measurements
;  data_array = list() & model_array = list()
;  chisqr_results_array = []
;
;  ; plot: overlay the observed equitorial (mean'd) slice on the plot of the model's seeing
;  ; conditions. Each
;  for i=0,eq_ref_list.length-1 do begin
;    ;print, "iteration of scaling factors: ", i
;    ; match peak intensities with a scaling factor defined to be the
;    ; peak intensity for a given seeing condition in the model divided by
;    ; the peak intensity for the equitorial slice (average middle three pixel rows
;    ; centered on user input for equitorial pixel row).
;    normalization_factors.add, peak_intensity[i]/max(mean_eqsl)
;    ;print, "scaling factor for this iteration: ", peak_intensity[i]/max(mean_eqsl)
;    ;come up with array of colors
;    loadct,13,rgb_table=rgb_table ;can use loadcsvcolorbar in place of loadct
;    colorindex=fix(seeing_array[i]/max_seeing_to_simulate*255)
;    rgb_color=transpose(rgb_table[colorindex,*])
;
;    ; the data list of scaled equitorial slice pixel rows from the observation
;    data_array.add, mean_eqsl*normalization_factors[i]
;
;    if i eq 0 then continue ; skip plotting the 0" seeing.
;    ; plot the observation over the model in the plot window.
;    ;p = plot(obs_steps_shifted_to_max[i], data_array[i], linestyle = 2,color = rgb_color, /current, /overplot)
;    p = plot(list_of_obs_ang_dist[i], data_array[i], linestyle = 2,$
;      xrange = [-7,7], color = rgb_color, /current, /overplot)
;
;  endfor
;
;
;
;  ; Now compare each model with its scaled version of the data, by doing a chi sqr
;  ; measurement. The results of each X2 measurement are stored in an array and
;  ; returned on exit of procedure so that comparisons can be made. This is important
;  ; at the phase of this project where we want to compare what we get the most statistically
;  ; accurate seeing condition to what Potter has in his finished datasets.
;
;  ; isolate subset of model equitorial reflectance for each seeing condition, where the subset
;  ; is the same length as the observed equitorial pixel slice (almost always 10 pixels across).
;  ; Compare this model to the data in terms of a X2 result and
;
;
;  closewin
;  w5 = window()
;
;  w4 = window(dimensions = [1500,800]) & w4.window.setcurrent
;
;  rss_array = []
;  x2array = []
;  peak_dist_from_center_model = list()
;  iterations = eq_ref_list.length-1
;  print, "RSS:            X2: "
;  shift_model = ''
;  shft = 0
;  while shift_model ne 'n' or shift_model ne '0'  do begin
;    for i = 0,iterations do begin
;      ; define model fitting results arrays
;
;      ; define model and data arrays
;
;      data_array = mean_eqsl
;      max_intensity_pixel_location_within_data_array = where(data_array eq max (data_array))
;
;      model_array_full = eq_ref_list[i]/normalization_factors[i]
;      max_intensity_pixel_location_witin_model = where(model_array_full eq Max(model_array_full))
;
;      center_of_model_disk = 128
;      ; the difference between the peak in the model equitorial pixel slice and the center of the simulated
;      ; disk :
;      peak_dist_from_center_model.add, where(model_array_full eq Max(model_array_full)) -center_of_model_disk
;
;
;      model_array_subset = model_array_full[ max_intensity_pixel_location_witin_model+shft-max_intensity_pixel_location_within_data_array:max_intensity_pixel_location_witin_model+shft+(9-max_intensity_pixel_location_within_data_array)]
;
;
;      ;measure goodness of fit with RSS
;      rsstotal=0
;
;      for j=0,model_array_subset.length-1 do rsstotal+=(model_array_subset[j]-data_array[j])^2.0
;      rss_array= [rss_array, rsstotal]
;      x2,model_array_subset, data_array, result, /red
;      x2array = [x2array, result]
;      ;print, sigfig(rsstotal, result, i/2.0)
;
;      if  result eq "Infinity" then continue
;      ;show the measurements
;      ttl = string(seeing_array[i]) & ttl=ttl.replace('0000', '')
;      ;ttl = string(ttl + '",X2='+string(result)) & ttl = ttl.compress()
;      ttl = string(ttl + '",$\chi^2$ ='+string(fix(result))) & ttl = ttl.compress()
;
;
;      plt = plot(model_array_subset, name = 'model', layout = [eq_ref_list.length/2,2,i+1], /current, color = "green", linestyle =3 , title =ttl)
;      plt = plot(data_array,name = 'data', /overplot,/current,layout = [eq_ref_list.dim/2,2,i+1], color = "blue")
;      l = legend()
;    endfor
;
;    ; visual aid for min of X2
;    w5.erase
;    !null = plot(seeing_array, x2array, /ylog,xtitle= "Seeing(arcsec)", ytitle= "X2 fit")
;    bestseeing = seeing_array[where(x2array eq min(x2array))]
;    bestfit = x2array[where(x2array eq min(x2array))]
;    t = text(bestseeing-1,100,string("Min X2 is " + string(bestfit)), /data)
;    w4.window.setcurrent
;    print, "best seeing estimate from this model: ", bestseeing, " with X2 fit of ", bestfit
;
;    shift_model = ''
;    read, shift_model, prompt = "Shift the model(blue) L/R? (0=done, or input best seeing manually): "
;    if shift_model eq '0' then break
;    if strmatch(string(shift_model), 'R', /fold_case) then begin
;      ; when the 'while' loop repeats, this shift will change the window over which the model array is looked at.  ;sigfig
;      shft  +=1
;      ;erase the X2 result plots
;      w4.erase
;      ; start over with new RSS, x2 results, and peak dist from center of disk arrays/lists.
;      rss_array = []
;      x2array = []
;      peak_dist_from_center_model = list()
;    endif else if strmatch(string(shift_model), 'L', /fold_case) then begin
;      shft  -=1
;      w4.erase
;      rss_array = []
;      x2array = []
;      peak_dist_from_center_model = list()
;    endif else if isa(shift_model/1.0, /number) then begin
;      x2array[valueposition(seeing_array, shift_model)] = 0.0 ; artificially setting the best fit when doing "chi by eye"
;      ;stop
;
;      ;closewin
;      print, string("You have choosen "+ string(shift_model) + " as the best seeing.")
;      wait, 1
;      break
;    endif
;
;  endwhile
; ;;;;;;;;;;;;;;;;
; "IMCENTER_OFFSET" parameter determination
closewin
; HERE WE ARE CHECKING FOR THE BEST CENTER COLUMN IN THE MODEL IMAGE, IN TERMS OF THE X DIRECTION. 
; WE JUST START AT COLUMN 100 FOR THE LEFT BOUND OF THE 10X10 MODEL IMG, AND TAKE THE RIGHT BOUND 
; AS LEFT BOUND PLUS NINE. STARTING FROM THE LEFT (COLUMN 100), WE PRETTY MUCH GET A DARK SQUARE AS THE MODEL
; IMAGE. WHEN THIS IS COMPARED TO THE CONTINUUM REFLECTANCE IMAGE, WE GET A HUGE X2 RESULT. THAT RESULT IS SAVED 
; IN THE RESULTS ARRAY, AND WE INCREASE THE LEFT AND RIGHT BOUNDS OF THE MODEL IMAGE BY ONE PIXEL, AND TAKE 
; ANOTHER MEASUREMENT.  
  optimal_index = where(msrs eq min(msrs, /nan)) & optimal_index=optimal_index[0]
tstim = allmodels[optimal_index] ; the 'optimal index' is produced as a 1x1 array, extract it to integer.
testforimcenteroffset = []; store the model fitting results, to be searched for a min reduced X2 fit.
for dx1 = 100,240 do begin ; starting at column 100, going no farther than column 240
tstimg = tstim[dx1:dx1+9, 123:132]
sf = max(continuum_reflectance_image, /nan)/max(tstimg, /nan)
; in the case that a center continuum image has less than ten pixel rows, 
; we need to make sure the tstimg and continuum images are the same size. 
; one way to do this is to add an "empty" pixel row to the bottom of the 
; continuum image, which makes it the same size as the tstimg. We could also 
; remove a row from the bottom of the tstimg. I say the bottom because i 
; recall that sometimes there were only 9.5 or 8.5 usable slices 
; in some images for some observing periods. So we lost either the top or 
; bottom slice (cant recall at this moment of writting). 
; So lets, for now, just go with adding an "empty" pixel row to the bottom 
; of the continuum image.
if  total(continuum_reflectance_image.dim) ne 20 then begin 
; find out how many rows we are missing from the continuum image
dims = continuum_reflectance_image.dim
ydim = dims[1] & ydim  = ydim[0]; extract from 1x1 array to simple integer.
missing_rows = 10-ydim
; add the "empty" pixel row to the bottom
continuum_reflectance_image=[[continuum_reflectance_image],[continuum_reflectance_image[*,0]]]
endif 
x2, tstimg*sf, continuum_reflectance_image, rslt
testforimcenteroffset= [testforimcenteroffset, rslt/100.0]
endfor
best_test_index = where(testforimcenteroffset eq min(testforimcenteroffset,/nan))
best_dx1 = testforimcenteroffset[best_test_index]



; imcenter_offset fails when "/nan" is not included in the min and max funtions above. So i also put the /nan keyword in the remaining min, max functions here. 
imcenter_offset = 100+where(testforimcenteroffset eq min(testforimcenteroffset, /nan))+5-128; probably correct ;)... had to include
print, "Imcenter offset value: ",imcenter_offset


!null =image( tstim[100+best_test_index:100+best_test_index+9, 128-5-(eq_slice_pos - 5):128+4-(eq_slice_pos - 5) ]*max(continuum_reflectance_image, /nan)/max(tstim, /nan), $
  rgb_table= 13, layout = [2,1,1], title= "Model image with 'imcenter offset' term")
!null = image(continuum_reflectance_image, rgb_table = 13, layout = [2,1,2], title = "observation", /current)
;!null = image(tstim[128+imcenter_offset-5:128+imcenter_offset+4, 128-5-(eq_slice_pos - 5):128+4-(eq_slice_pos - 5) ], rgb_table= 13, layout= [1,1,1])




closewin
  ;closewin
  ; find the best fitting model and derive the best estimate for the seeing condintions in this observation
;  best_rigorous_seeing = seeing_array[where(msrs eq min(msrs))]

  bestscale = normalization_factors[optimal_index]  ;& bestscale = bestscale[0]
  ;imcenter_offset = where(allmodels[optimal_index] eq max(allmodels[optimal_index])) mod 256 -128
  ;imcenter_offset = imcenter_offset[0]; to turn it into an integer instead of a one element array.
  estimated_seeing = seeing_array[optimal_index] & estimated_seeing = strcompress(string(estimated_seeing[0]))
  print, "Results of Seeing Estimation: "
  print, "  Reduced X2 Fit*     Seeing                  *is the X2 result divided by degrees of freedom (near 100 +/- 5 is still about 100"
  rs = [[msrs], [seeing_array]]

 
  print, "------------------------------------------"
  print, "*** Best estimate of seeing : ", estimated_seeing.replace('0000',''),'"'
  print, "------------------------------------------"
  model_img = allmodels[optimal_index]
  normalization_factor = normalization_factors[optimal_index] & normalization_factor=normalization_factor[0]
  
  image_pixel_multiplier = bestscale* (continuum_solar_flux_at_mercury ) * delta_lambda *4.0*!pi*1.0e-9
  for x =optimal_index - 3 , optimal_index + 3 do print, round(rs[x]), seeing_array[x], (x eq optimal_index) ? " <-- best seeing" : ''

  return

end


;;potentially useful someday:
;base = WIDGET_BASE(/align_center)
;field = CW_FIELD(base, TITLE = "Name:", /FRAME, /STRING,/RETURN_EVENTS, /FOCUS_EVENTS)
;WIDGET_CONTROL, base, /REALIZE

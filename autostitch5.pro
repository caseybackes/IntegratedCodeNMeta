
pro autostitch5, model_img,  normalization_factor, eq_slice_pos, imcenter_offset, $
  center_ct, north_ct, south_ct, center_em, north_em, south_em, $
  best_nco, best_sco, final_ct_image, final_em_image, final_model 
  closewin
 
 eq_slice_array = center_ct[*,eq_slice_pos]
 
 
 ; TESTING FOR NORTH/CENTER OVERLAP IN UNITS OF PIXELS
 ; store the fitting results (X2 measurements) in this array
 nc_overlap_results = []
 ; make a window for the image, specifically to make the dimensions more appropriate for a tall set of images
 north_image_window= window(dimension = [100,800])
 north_image_window.setcurrent
 ; For displaying the results in one window, track the image position within the graphics window, starting at one, increasing for each overlap. 
  impos = 1
 for overlap = 1,6 DO BEGIN
   ; The subsection of the model that we want to look at will be based on some parameters passed in to this procedure and on the dimensions of the  
   ; combined center and north image. One note: the equator of the model is always on the 128 row of the model image, which is 256 square.  
   ntest_model = model_img[128+imcenter_offset-5: 128+imcenter_offset+4, 128-eq_slice_pos: 128-eq_slice_pos+center_ct.length/10+north_ct.length/10-overlap-1] ; -1 accounts for the zero index, different from counting from 1.               
   ntest_model /=normalization_factor; brings up the tiny intensity of the model to that of the observation. 
  
  
  ; The north image may be significantly dimmer than the center image in total or at the overlap region. 
  ; So we scalue up the intensity based on the median fractional change between overlapping pixels of the top
  ; of the center image and the bottom of the north image. 
  north_scale_factor = median(center_ct[*,-(1+overlap):-1]/north_ct[*,0:overlap-1])
  
  if overlap eq 1 then begin
  print, "NEED TO IMPROVE THE ROW INDEX DETERMINATION FOR THE NORTH MOSAIC(POSSIBLY SOUTH AS WELL). IT NEEDS TO BE MORE ROBUST"
  print, "Ok, I think thats done now. The mosaic is built differently now - which only means the indicies for the rows in the center and north images are found in a slightly different way. "
  print, "It uses negative integers to find the top edge of the image, instead of '8' or '9', because there have been instances where the image only has 8 or nine slices. "
  Print, "We are now also using the full center image and the full north image for comparing to the model, not just the north-center image from the equator to the top of the north image." 
  endif
  ; The north mosaic is made of three pieces: The north image excluding the overlap region, the overlap between north and center, 
  ; and the center image from the equator to the top of the center image. 
   n_mosaic =  [  [center_ct[*, 0: -(1+overlap)]], $ bottom is the center image
               [(center_ct[*,-overlap:-1] + north_scale_factor*north_ct[*,0:1-overlap])/2.0], $  ; followed by the overlap 
               [north_scale_factor*north_ct[*, overlap: -1]] ]         
            
  !null = image(n_mosaic, rgb_table= 13,  title= string(string(overlap)+ "pixel overlap"), /current,MARGIN = 0, layout= [2,6,impos])
  impos+=1
  !null = image(ntest_model, rgb_table= 13, layout= [2,6,impos], title= "model" , /current,MARGIN = 0)
  impos+=1
  ;stop
 
  ; test the fit and store the measurement of the fit. 
  x2, ntest_model, n_mosaic, result & nc_overlap_results = [nc_overlap_results, result/n_mosaic.length]
  ; print out feedback: 
  print, "X2 result: ", result/n_mosaic.length, ". Overlap: ", overlap
 endfor

 best_nco = where(nc_overlap_results eq min(nc_overlap_results))+1; where the plus one adjusts for zero-index offset. 
 print, "Best north/center overlap as determined by chai square: ", best_nco
 for x = 2,0 do t = text(280,80+x*170,string("X2 = " + strcompress(string(round(nc_overlap_results[x])))), /device)
 ;stop,''
 
;_______________________________________________________________________________________________________
closewin

 sc_overlap_results = []
 impos = 1
 w2 = window()
 w2.window.setcurrent
 
 ; We want to get the appropriate subsection of the model image for the south mosiac comparison. First, we find where the equator will fall in the observation, as a negative row index ( so number of rows from the top)
 eq_slice_pos_neg = -(center_ct.length/10-eq_slice_pos)
 ; and this will also be accurate for the south-center mosiac, since the top of the mosiac is the center image and it is unchanged in the overlapping with the south image. 
 
 for overlap = 1,6 DO BEGIN
   ; dont ask how i got these indicies. 
;   stest_model = model_img[128+imcenter_offset-5 : 128+imcenter_offset+4, 128-(center_ct.length/10+south_ct.length/10)-eq_slice_pos_neg:128-eq_slice_pos_neg-1  ]
;   stest_model = model_img[128+imcenter_offset-5 : 128+imcenter_offset+4, xxxxxx :128                                                           ]
   ; TAKING A CRITICAL LOOK AT THE UPPER AND LOWER BOUNDS OF THE MODEL IMAGE, WITH THE HEIGHT OF THE MOSAIC
   
   south_scale_factor = median(center_ct[*,0:overlap-1]/ south_ct[*,-(1+overlap):-1])


   ; lets take a crital look at the index bounds for the mosiac image (center and south with overlap region) 
   ; the mosiac is composed of (1) the south image, (2), the overlap region, and (3) the center image, and 
   ; is built from the bottom, up. 
   ; the first few pixel rows are going to be the south image. 
   south_btm = 0 ; first row in image
   south_top = -1+south_ct.length/10 - overlap; 10px high img with 1 pix overlap gives value of (-1+10-1) = 8. The last row,9, is part of the overlap region. Dont use overlap region pixel row in "south" image. 
   
   ; the overlap region takes the number of overlapping rows from both the center and the south images. The top of the south image is identified as the "-1" row (last row). This overlap region will also 
   ; on the next iteration include an additional row, so we can identify it as the "-2" row. So we have that "-overlap" value pulls the bottom of the overlap section of the south image. 
   ; Same logic for indices used for the center image, but we have to subtract a value of 1 from the overlap, since it is the bottom of the center image, and that is identified as row 0.   ...Thanks IDL  >:\  
   overlap_region = (south_scale_factor*south_ct[*,-1*overlap:-1] + center_ct[*,0:overlap-1])/ 2.0  
   
   ; the bottom of the center image will not include any rows in the overlap region. So if a 1px overlap region includes the first (row zero, the btm) row of the center image, then we start at row index 1 (the second row)
   center_btm = overlap
   ; the top of the center image included in this mosaic, is held constant at the final (top) row in the image. Since there are not always 10 pixel rows (indices 0 through 9) then we can identify the last 
   ; row of the center image with "-1"
   center_top = -1
   s_mosaic = [ [ south_scale_factor*south_ct[*,south_btm:south_top]], $
              [ overlap_region], $
              [ center_ct[*,center_btm:center_top] ] ]; 
  
  ; Take a subsection of the model that resembles the observation
  ; Easiest to find the top of where the model should be, then go "s_mosaic rows" down, and stop there. 
  ; We know that the equator is on slice "eq_slice_pos" in the center image. This corresponds to row 128 in the model, a 256x256 image. 
  ; So how many rows are included in the center image above the equator? 
  center_height =center_ct.length/10 ; gives the number of rows. if we see there are 8 rows, then center_ct.length = 80, and center_height then equals 8.  <- HAS BEEN KNOWN TO HAPPEN, THIS ENSURES ROBUST CODE!!!!!!!
  ; If the equator is located at row index 6 and the last row has index 8(just an example, yes there are ALMOST always 10), then there are two rows above the equator in the center image. 
  ; We know that the equators should align in the model and the observation. So the equator is at model image row index 128 and center image row index 6. We want the model to also have two pixel rows above the 
  ; equator. So the top of the model is at row index 130 = 128 + (center_ct.length/10-1-eq_slice_pos)  
  model_top = 128 + (center_ct.length/10 -1 - eq_slice_pos)
  
  ; Now we want to know the bottom index for the model. This depends on the size of the south-center mosaic(which changes with every overlap iteration). 
  ; So we calculate the mosaic height.
  s_mosaic_height = s_mosaic.length/10
  
  ; and define the bottom of the model to be the top minus the mosaic height, 
  model_btm = model_top - s_mosaic_height+1
  
  ; Now "cut out" a section of the model exactly where we need it, 
  stest_model =model_img[128+imcenter_offset-5 : 128+imcenter_offset+4, model_btm: model_top]
  ; and scale it up to the order of mag values of the observation (just makes the X2 results less misleading)
  stest_model /= normalization_factor

 
  ; If we managed to f**k up the indices calculations, then the images may not be the same dimensions (which they need to be to make a comparison with the X2 procedure below. Actually, it works with them being the same size but 
  ; we are professionals here. They NEED to be the same size, to have an accurate comparison.)
  if total(s_mosaic.dim eq stest_model.dim) ne 2 then stop; if stopped here, the two images are not the same size 
  
  ; show the results
  !null = image(s_mosaic, rgb_table= 13, layout=[2,6,impos], title= string(string(overlap)+ "pixel overlap"), /current, MARGIN = 0)
  impos+=1
  !null = image(stest_model, rgb_table= 13, layout=[2,6,impos], /current,MARGIN = 0)
  impos+=1
  
  ; test the fit and store the measurement of the fit.
  x2, stest_model, s_mosaic, result & sc_overlap_results = [sc_overlap_results, result/s_mosaic.length]
  print, "reduced X2 result: ", result/s_mosaic.length, ". Overlap: ", overlap
 ; stop,''
 endfor
 
 best_sco = where(sc_overlap_results eq min(sc_overlap_results))+1
 print, "Best south/center overlap: ", best_sco, " pixels. "
;
; PUT TOGETHER THE MOSAIC IMAGE
;   determine the intensity scaling factors for the north and south (prevents mosiac from being discontinuous at overlap area)
 north_delta_i = median(center_ct[*,(-1*best_nco):-1]/north_ct[*,0:(best_nco-1)])
 south_delta_i = median(center_ct[*,0:(best_sco-1)]/north_ct[*,-best_sco:-1])
 
print, "North image intensifying factor: ", north_delta_i
print, "South image intensifying factor: ", south_delta_i

; APPLY THE OVERLAP TO THE IMAGES THROUGH STITCHING PROCESS: 
 smallstitch3, center_ct, eq_slice_pos, final_ct_image, $
   northimg = north_ct*north_delta_i, southimg = south_ct*south_delta_i, $
   nco = best_nco, sco = best_sco  ;, /noscale
 smallstitch3, center_em, eq_slice_pos, final_em_image, $
   northimg = north_em*north_delta_i, southimg = south_em*south_delta_i, $
   nco = best_nco, sco = best_sco  ;, /noscale

; final image size in y direction
height = final_ct_image.length/10

;find where the eq slice is in the observation
eqpos = (south_ct.length/10) + (eq_slice_pos) - best_sco - 1

; Make model image, same size as stitched images, with the equator on the same pixel row. (per Tim's request)
modl = model_img[128+imcenter_offset-5: 128+imcenter_offset +4, $
                 128-eqpos:128-eqpos+height -1      ]/normalization_factor


closewin
im = image(modl, layout = [3,1,1], rgb_table= 13, title= "Model image")
im = image(final_ct_image, layout= [3,1,2], rgb_table = 13, title = "Observation CT", /current)
im = image(final_em_image, layout= [3,1,3], rgb_table = 13, title = "Observation D2", /current, max_value= max(final_em_image[*,8:16]))
final_model = modl
 
 
return
end
 
;; THE OLD CODE, WHICH HAS BEEN UPDATED/REPLACED WITH ALL CODE ABOVE 
; 
;  print, "for the north!!!"
;  ; now we have the seeing estimate for the center image and a model with the same seeing.
;  ; we can put the north and center continuum observation images together, and compare the HWHM between
;  ; this and the model.
;
;  ; define an isolated model array,equally dimensioned with (what will be) the stitched data array.
;  test_model = model_img*normalization_factor;[128-5+imcenter_offset:128+4+imcenter_offset,128-14:128+15]
;
;  ; and we need the center pixel cols (meaned cols 4 and 5, to smooth the signal)
;  mid_column_mean_equator_to_northpole_model =  mean(test_model[4:5,15:-1],dim = 1)/normalization_factor[0]
;
;  ; store the results of the hwhm into an array.
;  x2results = []
;  model_fit_results_n = []
;
;  ;(2) Iteratively overlap the north and center, eacht time comparing the HWHM of model and observation.
;
;  for overlap = 0,3 do begin
;    ; stitch the image with incrementally increasing pixel overlap, steps of 1 pixel
;    smallstitch3,  center_ct, eq_slice_pos, stitched_northcenter, northimg = north_ct, nco = overlap
;    closewin
;    ; now take from this stitched image the "middle column" (meaned from cols 4,5 to smooth the signal), from the
;    ; equatorial pixel row up to the top of the composite image.
;    mid_column_mean_equator_to_northpole_observation = mean(stitched_northcenter[4:5,eq_slice_pos:-1],dim = 1)
;    j = plot(mid_column_mean_equator_to_northpole_observation, color= 'blue', name= "Data")
;
;    ; DEVELOPMENT ONLY: Check the x2 results and see what comes back as the best fit (or RSS)
;    x2, mid_column_mean_equator_to_northpole_model,mid_column_mean_equator_to_northpole_observation, x2result, /rtrim
;    print, "Model fit for ",strcompress(string(overlap))," pixels overlap: X2 = ", fix(x2result)
;    x2results =[x2results, x2result]
;    j = plot(mid_column_mean_equator_to_northpole_model, color= 'green', name="Model", /overplot, title= (string(string(overlap) + ' Ovrlp, X2 = ' + string(fix(x2result)))))
;
;  endfor
;  ;stop
;  closewin
;  best_nco = where(x2results eq min(x2results))
;  print, "Best north-center overlap: ", best_nco
;  ; ------------------------------------------------------------------------------
;  ;stop
;  ; FOR THE SOUTH/CENTER OVERLAP:
;  print, "for the south!!!"
;  ; define the center pixel cols (meaned cols 4 and 5, to smooth the signal) from the equator to the bottom of the south image
;  mid_column_mean_equator_to_southpole_model = mean(test_model[4:5,0:15],dim = 1)/normalization_factor[0]
;
;  ;(2) Iteratively overlap the north and center, eacht time comparing the HWHM of model and observation.
;  ;------------------------------------------------
;
;  southimgdims = south_ct.dim & southimgheight = southimgdims[1]
;
;  x2results2 = []
;  model_fit_results_s =[]
;  for overlap = 0,3 do begin
;
;    ; stitch the image with incrementally increasing pixel overlap, steps of 1 pixel
;    smallstitch3,  center_ct, eq_slice_pos, stitched_southcenter, southimg = south_ct, sco = overlap
;    closewin
;    ;redefine where the equitorial pixel row is in the observation
;    stitched_image_dims = stitched_southcenter.dim & stim_hgt = stitched_image_dims[1]; the height of the stitched image
;    eq_slice_in_stim = southimgheight+eq_slice_pos-overlap
;
;    ; now take from this stitched image the "middle column" (meaned from cols 4,5 to smooth the signal), from the
;    ; equatorial pixel row up to the top of the composite image.
;    mid_column_mean_equator_to_southpole_observation = mean(stitched_southcenter[4:5,0:eq_slice_in_stim],dim = 1)
;
;    x2, mid_column_mean_equator_to_southpole_model,mid_column_mean_equator_to_southpole_observation, x2result, /ltrim
;    print, "Model fit for ",strcompress(string(overlap))," pixels overlap: X2 = ", fix(x2result)
;    x2results2 = [x2results2, x2result]
;
;  endfor
;
;  best_sco = where(x2results2 eq min(x2results2))
;  print, "Best south-center overlap: ", best_sco
;  closewin
;  return
;
;
;end
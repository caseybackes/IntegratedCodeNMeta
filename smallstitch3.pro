;+
; NCO = nco:
;     north-center overlap, in pixels. 
; SCO = sco:
;     south-center overlap, in pixels.
; centerimg:
;     the array contining the reduced center image.
; stitchedimg:
;     the named variable containing the two (or all three) images after stitching.     
; NORTHIMG = northimg : 
;     the array contining the reduced north image. 
; SOUTHIMG = southimg:
;     the array contining the reduced south image.     
;-


pro smallstitch3, centerimg, eq_pos, resultingimg, NORTHIMG = northimg, SOUTHIMG= southimg, NCO = nco, SCO = sco, NOSCALE = noscale, north_delta_i, south_delta_i
  ; empty image pixel rows need to be removed from the reduced images before being fed into this procedure
    ;stop
    if keyword_set(northimg) and not keyword_set(southimg) then begin
      print, "stitiching the center to north..."
      north_delta_i = median(centerimg[*,(-1*NCO):-1]/northimg[*,0:(NCO-1)])
      
      if not keyword_set(noscale) then northimg *= north_delta_i
      if not keyword_set(noscale) then print, "scaling north&south to center"
      ;most of the north image:
      piece_a = northimg[*,NCO:-1]
      ;the north-center overlap
      if nco ne 0 then   piece_b = ((centerimg[*,(-1*NCO):-1]+northimg[*,0:(NCO-1)])/2.0) 
      ;most of the center:
      piece_c = centerimg[*,0:-(NCO+1)]
      
      ;concatonate images, bottom to top
      resultingimg  = (nco ne 0) ?  [[piece_c],[piece_b],[piece_a]] : [[piece_c],[piece_a]]
      ;!null = image(resultingimg, rgb_table = 13, layout = [3,1,1])
      
    endif
    
    if keyword_set(southimg) and not keyword_set(northimg) then begin
      print, "stitching the center and south..."
       south_delta_i = median(centerimg[*,0:(sco-1)]/southimg[*,-sco:-1])
       if not keyword_set(noscale) then southimg*=south_delta_i
       if not keyword_set(noscale) then print, "scaling north&south to center"

       ;most of the center image:
       piece_a = centerimg[*,SCO:-1]
       ;the south-center overlap
       piece_b = (centerimg[*,0:(sco-1)]+southimg[*,-sco:-1])/2.0
       ;most of the south
       piece_c = southimg[*,0:-(1+sco)]
       
       ;concatonate images, bottom to top
       resultingimg =  [[piece_c],[piece_b],[piece_a]]
       ;!null = image(resultingimg, rgb_table = 13, layout = [3,1,1])
       
    endif
    
    
    if keyword_set(northimg) and keyword_set(southimg) then begin 
      print, "stitiching the whole thing..."
      ; determine the change in intensity to the north image to make it as bright as the center image based on the overlapping region
      
      piece_a = northimg[*,NCO:-1]
      piece_b = (centerimg[*,(-1*NCO):-1]+northimg[*,0:(NCO-1)])/2.0
      piece_c = centerimg[*,SCO:-(NCO+1)]
      piece_d = (centerimg[*,0:(sco-1)]+southimg[*,-sco:-1])/2.0
      piece_e = southimg[*,0:-(1+sco)]
      before  = [[piece_e],[piece_d],[piece_c],[piece_b],[piece_a]]
      ;!null= image(before, rgb_table = 13, layout = [2,1,1], title= "no scaling for N/S")
      
      
      if not keyword_set(north_delta_i) and not keyword_set(south_delta_i) then begin
        north_delta_i = median(centerimg[*,(-1*NCO):-1]/northimg[*,0:(NCO-1)])
        south_delta_i = median(centerimg[*,0:(sco-1)]/southimg[*,-sco:-1])
        ;print, "North Scale: ", north_delta_i
        ;print, "South Scale: ", south_delta_i
      endif
      
      ; apply the change in intensity (scaling factor)
      if not keyword_set(noscale) then northimg*=north_delta_i
      if not keyword_set(noscale) then southimg*=south_delta_i
      if  keyword_set(noscale) then print, "not scaling north&south to center"

      ;most of the north image:
      piece_a = northimg[*,NCO:-1]
      ;the north-center overlap
      piece_b = (centerimg[*,(-1*NCO):-1]+northimg[*,0:(NCO-1)])/2.0
      ;most of the center:
      piece_c = centerimg[*,SCO:-(NCO+1)]
      ; the south-center overlap
      piece_d = (centerimg[*,0:(sco-1)]+southimg[*,-sco:-1])/2.0
      ;most of the south img
      piece_e = southimg[*,0:-(1+sco)]
      ;stop
     
      ;stack them to together
      resultingimg = [[piece_e],[piece_d],[piece_c],[piece_b],[piece_a]]
      ;!null = image(resultingimg, rgb_table = 13, layout = [3,1,1], /current, title= "scaled n/s")
    endif 
end


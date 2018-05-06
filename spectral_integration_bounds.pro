;+
;   Basically, give it the "final observed spectrum" and "slice indices", while also passing 
;   variable names to store the d2/continuum boundaries in.  
;- singlemercproc
;
pro spectral_integration_bounds, final_observed_spectrum, slice_indices, d2_start, d2_end, cont_start, cont_end, M=m

; find d2/cont start stop indicies
window, 1
plot, final_observed_spectrum, $
  $yrange = [-.5,1.5], $
  title = 'Zoom to D2 region (LEFT & RIGHT LIMITS AROUND D2) - or y < 0 for total escape'
cursor, zoom1, notused, /data, /down
zoom1 = round(zoom1)
;if zoom1 lt 0 then retall
vline, zoom1
cursor, zoom2, notused, /data, /down
zoom2 = round(zoom2)
vline, zoom2

;establish the d2 boundaries
plot, final_observed_spectrum, xrange = [zoom1,zoom2], title = 'Select d2 peak and then a MIN boundary'

cursor, d2_1, notused, /data, /down
d2_1 = round(d2_1)

if !mouse.button eq 1 then begin
  !null = max(final_observed_spectrum[d2_1-5:d2_1+5], maxindex)
  d2_peak = maxindex +d2_1-5
endif else if !mouse.button eq 4 then d2_peak = d2_1
vline, d2_peak

cursor, d2_2, notused, /data, /down
d2_2 = round(d2_2)
if !mouse.button eq 1 then  begin 
  !null = min(final_observed_spectrum[d2_2 - 5: d2_2+5], minindex) 
  d2_bound = minindex+d2_2-5
endif else if !mouse.button eq 4 then d2_bound = d2_2
vline, d2_bound
wait, .1
filter_halfwidth = abs(d2_peak - d2_bound)
d2_start = d2_peak - filter_halfwidth
d2_end = d2_peak + filter_halfwidth
;wdelete, 1
;establish the continuum boundaries
;y = 10
;gauge = "?"
;cont_center = 250 ; arbitrary start point
;window,1
;while y gt 0 do begin
;  plot, final_observed_spectrum , yrange = [-.5,1.5], title  = "select the quietest continuum range"
;  cont_start = cont_center - filter_halfwidth
;  cont_end   = cont_center + filter_halfwidth
;  r = final_observed_spectrum[cont_start:cont_end]
;  mean_cont_range = mean(final_observed_spectrum[cont_start:cont_end])
;  m = fltarr(r.length, 1)+mean_cont_range
;  x2, m,final_observed_spectrum[cont_start:cont_end], gauge
;  vline, cont_center
;  vline, cont_start
;  vline, cont_end
;  xyouts, 15,3,string("Gauge: " + string(gauge) + '  centered at ' + string(cont_center)), /device, charsize = 2.0
;  prev_cont_center = cont_center
;  cursor, cont_center,y,/data, /down
;
;  cont_center = round(cont_center)
;endwhile
if keyword_set(m) then filter_halfwidth*=m
gauge_array = []
cstarts =[]
cends = []
for cstart = 0,final_observed_spectrum.length-1-(2*filter_halfwidth) do begin
  cstarts = [cstarts, cstart]
  cend = cstart+2*filter_halfwidth & cends = [cends,cend]
  test_region = final_observed_spectrum[cstart:cend]
  test_line = make_array(test_region.length, value =  mean(test_region))
  residual_sum_of_squares  = total((test_line - test_region)^2) ; measure the relative flatness in this region 
  gauge_array = [gauge_array ,residual_sum_of_squares] ;  
  ;if cstart lt 10 then stop
endfor
cont_start = cstarts[where(gauge_array eq min(gauge_array))]
cont_end= cends[where(gauge_array eq min(gauge_array))]

print, "Start: ",cont_start
print, "End: ", cont_end

;wdelete,1

end
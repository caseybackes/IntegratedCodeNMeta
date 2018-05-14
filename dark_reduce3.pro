;+
;  After finding out that sometimes a dark frame is not available, 
;  and that the original scientists did not always use a dark frame, 
;  it should be possible to move forward with the data reduction 
;  without using a dark frame. 
;-
function dark_reduce3, merc_center
  ;sometimes the filename has a space at the end of the string. READFITS and HEADFITS
  ;cant access the file unless the space at the end of the string is removed -> basically ".fits " not equal to ".fits"
  !null = readfits(merc_center, hdr, /silent)
  size_of_image_x = sxpar(hdr, 'naxis1')
  size_of_image_y = sxpar(hdr, 'naxis2')

 
  ; find all files in the base directory that are fits files
  all_dark_files = file_search(file_dirname(merc_center),'*dark*.fit*', /fold_case)

if all_dark_files.dim eq 0 then begin
  !null = dialog_message("Houston, we have a problem. There appear to be no dark files for this observing day. So we need to pick an adjacent day's dark files.", /information)
  adj_days_dark = dialog_pickfile(path = file_dirname(merc_center), filter = '*dark*', title = "Need a sample dark file")
  if adj_days_dark ne '' then begin
    all_dark_files = file_search(file_dirname(adj_days_dark),'*dark*.fit*', /fold_case, count = count)
  endif
  if adj_days_dark eq '' then begin
    !null = dialog_message("Not using dark frame for data reduction.", /information)
    dark_array = !null
    return, dark_array
    end
  endif

  exceptions = 0
  dark_array = make_array(size_of_image_x,size_of_image_y,/integer,value = 0 )
  foreach dark,all_dark_files do begin
    if sxpar(headfits(dark, /silent), 'naxis1') ne size_of_image_x or sxpar(headfits(dark, /silent), 'naxis2') ne size_of_image_y or strmatch(dark,'*test*') then begin
      exceptions+=1
      continue
    endif
    dark_data = readfits(dark, /silent)
    dark_array +=dark_data
  endforeach

  ; take the average for the dark frames
  dark_array = dark_array / (all_dark_files.LENGTH - exceptions)
  
  if dark_array[0] eq -1 then begin
    print,'stop encountered at readfits for dark_array'
    stop
  endif


  return, dark_array
end




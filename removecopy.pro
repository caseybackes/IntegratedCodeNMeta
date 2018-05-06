;+
; This procedure permanently removes any file in the provided directory that is a "New copy of..."
; some other file.
;  
; Use with caution. 
;-

pro removecopy, folder

  files = file_search(folder, "*.fit*", /fold_case)
  count= 0.0
  foreach file,files do begin
    h = headfits(strtrim(file))
    hist = sxpar(h,'history')
    foreach elemnt, hist do begin
      if strmatch(elemnt,'*copy*') then begin 
        print, "A copy: ",file_basename(file)
        print, '       ', hist
        count+=1
        file_delete, file
      endif
     endforeach
  endforeach
  if count eq 0 then print, "No copies here."
  ; remove the white space at the end of the filename

return
end
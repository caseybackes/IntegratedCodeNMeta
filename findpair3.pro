;description of the pro 'findpair'?
; no, its in the comment below. 
pro findpair3, known_file, unknown_file, min_delta_t,SKY_FILE =sky, NORTH_FILE= north, SOUTH_FILE= south, EAST = east, WEST = west, FLAT=flat
  ; searches the directory of a known observation file, and searches for the closest (in time) file
  ; that contains a string. Stores the full path to the matching file in the unknown_file variable

  ; determine the relevant directory
  known_file = strtrim(known_file)
  directory = file_dirname(known_file)
  

  unknown_files = file_search(directory, '*.fit*')
  
  
  ; determine what file is being serached for based on the keyword activation
  ; SKY: 
  ;---------
  if keyword_set(sky) then begin
    unknown_files = unknown_files[  where(strmatch(unknown_files, '*sky*', /fold_case)) ]
      
    ;if total((where(strmatch(unknown_files, '*test*', /fold_case)))) ne 0 then remove, where(strmatch(unknown_files, '*test*', /fold_case)), unknown_files
    ;if unknown_files.dim eq 0 then unknown_files = file_search(directory, '*flat*', /fold_case)
    if unknown_files.length eq 0 then d= dialog_message("There are no sky images to use with this observation!", /information)
    
  endif else if keyword_set(north) then begin
  ; NORTH
    ; ------------
    ; need the north file to the known file
    
    filematches = [ $
      where([strmatch(file_basename(unknown_files), '*north*'         )]), $
      where([strmatch(file_basename(unknown_files), '*NP*', /fold_case)]), $ 
      where([strmatch(file_basename(unknown_files), '*m*n*', /fold_case)])]
    ;remove the'-1' from the list (for null result from searchs above)
    if total(filematches eq  -1) gt 0 then remove, where(filematches eq -1), filematches
    ; choose those unique file indices from the files in the directory, 
    unknown_files = unknown_files[filematches[uniq(filematches, sort(filematches))]]
    ; clean out the junk 
    if total(strmatch(file_basename(unknown_files), '*nw*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*nw*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*ne*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*ne*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*nn*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*nn*', /fold_case)), unknown_files
    ;if total(strmatch(file_basename(unknown_files), '*nofn*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*nn*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*cent*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*cent*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*bias*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*bias*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*sky*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*sky*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*south*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*south*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mercs*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mercs*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*venus*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*venus*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mars*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mars*', /fold_case)), unknown_files

  endif else if keyword_set(south) then begin
    ;SOUTH
    ;stop
    ;---------
    ; need the south file to the known file
    FILEMATCHES = [ $, 
    where([strmatch(file_basename(unknown_files), '*south*'            )]), $
      where([strmatch(file_basename(unknown_files), '*SP*',  /fold_case)]), $
      where([strmatch(file_basename(unknown_files), '*M*S*', /fold_case)])]
    ;remove the '-1' results for the search results string, 
    if total(filematches eq  -1) gt 0 then remove, where(filematches eq -1), filematches
    ; choose those unique file indices from the files in the directory, 
    unknown_files = unknown_files[filematches[uniq(filematches, sort(filematches))]]
    ;clean out the junk
    if total(strmatch(file_basename(unknown_files), '*se*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*se*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*sw*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*sw*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*ss*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*ss*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*NW*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*NW*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*cent*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*cent*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*bias*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*bias*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*sky*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*sky*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*north*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*north*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*venus*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*venus*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mars*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mars*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*west*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*west*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mercn*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mercn*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*merce*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*merce*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mercww*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mercww*', /fold_case)), unknown_files
    if total(strmatch(file_basename(unknown_files), '*mercw*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*mercw*', /fold_case)), unknown_files

  endif else if keyword_set(east) then begin
    ; need the south file to the known file
    unknown_files = file_search(directory, 'm*e*.fit*', /fold_case)
    if total(strmatch(file_basename(unknown_files), '*cent*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*cent*', /fold_case)), unknown_files


  endif else if keyword_set(west) then begin
    ; need the south file to the known file
    unknown_files = file_search(directory, 'm*w*.fit*', /fold_case)
    if total(strmatch(file_basename(unknown_files), '*cent*', /fold_case)) gt 0 then remove, where(strmatch(file_basename(unknown_files), '*cent*', /fold_case)), unknown_files




  endif else if keyword_set(flat) then begin
    ; need the south file to the known file
    unknown_files = file_search(directory, '*flat*.fit*', /fold_case)
    ;print, "flats before: ",unknown_files
    ;remove, where(strmatch(unknown_files, '*flatfork*', /fold_case)), unknown_files
    ;print, "flats after remove procedure: ",unknown_files
  endif

  ;removecopy, directory
  

  ;stop
  ; find the unknown file in the same directory that has the required string and is closest proximity to known file

  known_filename_testarray = []
  unknown_filename_testarray = []
  time_diff_testarray = []


  known_filename_finalarray = []
  unknown_filename_finalarray = []
  time_diff_finalarray = []

  time_Stamps=[]
  time_diff = 1e8    ; used only for the initial iteration of the loop below.
  
  
  ;stop
  foreach unk_file, unknown_files do begin
    ;remove whitespace at end of filepath string that kills the headfits procedure,
    
    ; filter 'same file' matching, 'test', 'tst' files
    if unk_file eq known_file or strmatch(unk_file,'[tst][test]')  then continue
    

    previous_time_diff = time_diff
    ; get the time of day in seconds of the unknown file
    unknown_header = headfits(strtrim(unk_file), /silent, ERRMSG = errmsg) ;headfits
    known_file_size = size(readfits(known_file, /silent)) & known_file_size = known_file_size[1:2]
    unk_file_size = size(readfits(unk_file, /silent)) & unk_file_size = unk_file_size[1:2]
    if total(known_file_size eq unk_file_size) ne 2 then continue

    ; get the dates and times of the image into integer formats
    ; for post 2000 imgs, the fits date will be there and correctly formatted
    ;
    ;
    ;   if (sxpar(header, 'date-new') ne 0 then unknown_ts = timestampstovalues (date-new)
    ;   else:
    ;   if date-obs is in format '????-??-??T??:??:??' then unknown_ts = timestampstovalues (date-obs)
    ;   else:
    ;   if date-obs is in format '??/??/??' then this is a pre2000 format,
    ;     date-obs = strsplit(date-obs, '/')     ; split the string into three strings, split at the '/' character
    ;     year = 19+date-obs[2] (the third slice)
    ;     month = date-obs[1] (the second slice)
    ;     day = date-obs[0] ( the first slice)
    ;
    ;     UTtimestring = strsplit ( sxpar(header 'UT')  , ':')   ; looks like   HH:MM:SS
    ;     hour = UTtimestring[0]   (first slice)
    ;     minute = UTtimestring[1]  (second slice)
    ;     second = UTtimestring[2]   (last slice)

    ; FORMAT 1:
    if sxpar(unknown_header, 'date-new') ne 0 then begin  ; usually just in apr1999
      unknown_timestamp = string(sxpar(unknown_header, 'date-new')+'-00:00')
      timestamptovalues, unknown_timestamp, day = unknownday, hour = unknownhour, minute = unknownminute, second = unknownsecond
    endif else begin

      ;FORMAT 2:
      if strmatch(sxpar(unknown_header, 'date-obs'), '????-??-??T??:??:??*') then begin
        unknown_timestamp = string(sxpar(unknown_header, 'date-obs')+'-00:00')
        timestamptovalues, unknown_timestamp, day = unknownday, hour = unknownhour, minute = unknownminute, second = unknownsecond
      endif else if strmatch(sxpar(unknown_header, 'date-obs'), '??/??/??*') then begin
        dateobs= strsplit(sxpar(unknown_header, 'date-obs'), '/', /extract)
        unknownday = dateobs[0]
        UTtimestring = strsplit(sxpar(unknown_header, 'UT'), ':', /extract)
        unknownhour = UTtimestring[0]
        unknownminute = UTtimestring[1]
        unknownsecond =fix(UTtimestring[2])


      endif
    endelse

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;THE KNOWN FILE:

    ; stop
    known_header = headfits(known_file, /silent,  ERRMSG = !null)
    ;stop
    !null = readfits(known_file, known_header, /silent)
    ;print, known_header
    ;if known_header eq '-1' then !null = readfits(known_file, known_header)
    ;if isa(known_header) eq 0  then stop
    ; FORMAT 1:
    if (sxpar(known_header, 'date-new')) ne 0 then begin  ; usually just in apr1999
      known_timestamp = string(sxpar(known_header, 'date-new')+'-00:00')
      ;stop
      timestamptovalues, known_timestamp, day = knownday, hour = knownhour, minute = knownminute, second = knownsecond
    endif else begin

      ;FORMAT 2:
      if strmatch(sxpar(known_header, 'date-obs'), '????-??-??T??:??:??*') then begin
        known_timestamp = string(sxpar(known_header, 'date-obs')+'-00:00')
        timestamptovalues, known_timestamp, day = knownday, hour = knownhour, minute = knownminute, second = knownsecond
      endif else if strmatch(sxpar(known_header, 'date-obs'), '??/??/??*') then begin
        dateobs= strsplit(sxpar(known_header, 'date-obs'), '/', /extract)
        knownday = dateobs[0]
        UTtimestring = strsplit(sxpar(known_header, 'UT'), ':', /extract)
        knownhour = UTtimestring[0]
        knownminute = UTtimestring[1]
        knownsecond =fix(UTtimestring[2])


      endif
    endelse
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; correct "00 hr" to "24 hr"
    if (knownhour eq 00) || (knownhour eq 0) || (knownhour eq '00') then knownhour = 0
    if (unknownhour eq 00) || (unknownhour eq 0) || (unknownhour eq '00') then unknownhour = 0

    ; determine the absolute time difference betwen the two images
    ; IDL quirk: the '3600' seconds in an hour needs to be written as below
    ;     to do the math correctly. Im guessing 3600 is too many bytes to do
    ;     the eq: 24 x 3600, because it kept telling me there were 20,000 seconds in a day, short by 40,000.
    ;     This is fixed by using '3.6e3' for '3600'
    known_time = knownday*8.6400e4 +knownhour*3.6e3  + knownminute*60.0   + knownsecond
    unknown_time =  unknownday*8.6400e4+ unknownhour*3.6e3    +  unknownminute*60.0   +  unknownsecond

    ; the time difference between the images is taken as the absolute difference.
    time_diff = abs(known_time - unknown_time)

    ; print out the files and the time difference between them.
    known_short = file_expand_path(known_file)+' ' & unknown_short = file_expand_path(unk_file)+' '


    ; append the files and time diff to their test vectors
    known_filename_testarray = [ known_filename_testarray,known_short]
    unknown_filename_testarray =  [unknown_filename_testarray,unknown_short ]
    time_diff_testarray = [time_diff_testarray,time_diff ]
    ;stop

  endforeach

  ; find index of min value in the time array, t_index...

  !null = min(time_diff_testarray, t_index)

  ;if keyword_set(flat) eq 0 then print, "min time diff is ",time_diff_testarray[t_index], "seconds with file: ",file_basename(unknown_filename_testarray[t_index])
  unknown_file = unknown_filename_testarray[t_index]
  min_delta_t = time_diff_testarray[t_index]
  print, file_basename(unknown_file), ";   dT = ",strcompress(string(min_delta_t))

  return
END
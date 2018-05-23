pro merc_file_rename, dir
  

  all_files = file_search(dir, "*.fit*")
  foreach f, all_files do begin
    obj = strtrim(sxpar(headfits(f), 'OBJECT'))
    ;stop
    filename = file_basename(f)
    oldname= strsplit(f,'.', /extract)
    newname = string(oldname[0]) + string(obj) + string('.') + string(oldname[-1])
    
    ;Modifications to accetpable nameing string:
    newname = repstr(newname,'/','div')
    
    
    file_move, f, newname
    print, "object id: ",obj
    print, "file name: ",filename
    print, "******"
    
  endforeach

end


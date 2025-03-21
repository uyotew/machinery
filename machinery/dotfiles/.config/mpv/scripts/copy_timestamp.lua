function copy_timestamp()
  time = mp.get_property_osd("time-pos/full")
  os.execute("wl-copy " .. time)
end

mp.add_key_binding("t","copy_timestamp_to_clipboard",copy_timestamp)

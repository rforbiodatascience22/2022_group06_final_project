pcr = read_csv("data/01_dat_load") %>% 
  distinct(PCRsucces, .keepall())


recoded_dataset <- read_csv("recoded_dataset_prev_removed.csv")

# here i need to understand which participants to remove and remove irrelevant cols 
# start by checking ethics 

test <- recoded_dataset %>%
  group_by(across(1)) %>%
  summarise(count = n())

# find who didnt agree to understanding the study 
checked_responses <- recoded_dataset %>% 
  filter(is.na(understands_study_purpose)) %>% 
  select(id)

# find row numbers 
which(recoded_dataset$id %in% c("pri300", "new300", "puffinmccloud", 
                                "lew161", "ran220"))
#  remove these participants 

final_participants <- recoded_dataset %>% 
  slice(-c(39, 202, 262, 282, 309))

# check next columns 

test <- final_participants %>%
  group_by(across(2:10)) %>%
  summarise(count = n())

# find who disagreed to the study 
checked_responses <- final_participants %>% 
  filter( wishes_to_take_part_in_study == "i disagree") %>% 
  select(id)

# find their column
which(final_participants$id %in% c("nooty", "g"))

# remove them
final_participants <- final_participants %>% 
  slice(-c(148, 149))

# find out who has never vaped and remove this needs to be done 

checked_responses <- final_participants %>%
  filter(
      (tolower(str_squish(as.character(current_vaping_device_type))) == "other (please specify)" | is.na(current_vaping_device_type)) &
      (tolower(str_squish(as.character(current_vaping_device_type_other))) == "other (please specify)" | is.na(current_vaping_device_type_other))
  ) %>%
  select(id,
         current_vaping_device_type, current_vaping_device_type_other)

# find their column  "
which(final_participants$id %in% c("fra060",
"smi080",
"top010",
"gho120",
"par161",
"mcc1302",
"som010",
"snoozychoc",
"cun230",
"kan141",
"sa2304",
"rb19",
"dav060",
"str201",
"jon060",
"pun220",
"ros110",
"gil061",
"don310",
"roo220",
"dai28/",
"iii457",
"bly140",
"cha281",
"ho1409",
"hou150",
"mca240",
"bat195",
"how240",
"kuf161",
"cha120",
"ter260",
"mob130",
"ric191",
"dry051",
"rol100",
"s4203406",
"mor300",
"win070",
"tur180",
"hip125"
))

# remove these participants 
final_participants <- final_participants %>% 
  slice(-c( 5,  22,  53,  54,  74, 103, 107, 134, 156, 172, 178, 182, 190, 193, 197, 198, 208,
           210, 214, 219, 231, 238, 256, 265, 267, 268, 282, 300, 304, 
           306, 325, 327, 330, 342, 346, 374, 381, 382, 384, 390, 402, 416))

# check attention check 
test <- final_participants %>%
  group_by(attention_check) %>%
  summarise(count = n())

# check who didnt put slightly agree 

checked_responses <- final_participants %>%
  filter(attention_check %in% c("agree", "slightly disagree", "strongly agree")) %>%
  select(id, attention_check)

which(final_participants$id %in% c( "phi170",
                                    "man030",
                                    "tcf011",
                                    "rte",
                                    "jea210",
                                    "pal210",
                                    "flexible-vaper",
                                    "hep140",
                                    "cre070",
                                    "lid260",
                                    "mos020",
                                    "con300",
                                    "cin250",
                                    "jon161",
                                    "smy072503",
                                    "raj301",
                                    "jwt010199",
                                    "bob010",
                                    "bar090"))

final_participants <- final_participants %>% 
  slice(-c(18, 29, 44, 50, 72, 99, 101, 129, 185, 196, 214, 257, 278, 279, 310, 360, 383, 385, 402))

# check everyone is appropriate age 

test <- final_participants %>%
  group_by(age) %>%
  summarise(count = n())

which(final_participants$age == 98)

final_participants <- final_participants %>% 
  slice(-c(275))

write_csv(final_participants, "final_participants.csv")





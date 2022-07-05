## Perform garbage code redistribution of snakebite data

library(data.table)
library(magrittr)
library(readr)
locs <- get_location_metadata(22, gbd_round = 6)

df <- fread('[DIRECTORY]/envenomation_deaths.csv')
df$icd <- gsub("[.]","",df$icd)
df <- merge(df, locs[, c('location_id', 'level', 'parent_id', 'ihme_loc_id', 'region_name', 'super_region_name')], by = 'location_id', all.x = TRUE)
df[parent_id == 6, parent_id := 44533] # Hack CHina's to line up correctly for redistribution

# Map
df[icd %like% 'E9050' | icd %like% 'X20', venom := 'snake']
df[icd %like% 'E9051' | icd %like% 'X21', venom := 'spider']
df[icd %like% 'E9052' | icd %like% 'X22', venom := 'scorpion']
df[icd %like% 'E9053' | icd %like% 'X23', venom := 'bees']
df[is.na(venom), venom := 'other']

# Get the codes that we redistribute
df[icd == 'E905' | icd %like% 'E9059' | icd %like% 'X29', venom := 'redistribute']
df[, dupe_check := .N, by = c('icd', 'location_id', 'year_id', 'age_group_id', 'nid', 'sex_id', 'deaths')]

redistribute_dems <- unique(df[venom == 'redistribute' & deaths != 0, c('location_id', 'age_group_id', 'sex_id', 'year_id', 'nid', 'code_system', 'source_name')])

# Now redistribute. Get denominator of total deaths by age/sex/location

df[, original := sum(deaths), by = c('location_id', 'nid', 'code_system', 'source_name', 'sample_size', 'year_id', 'age_group_id', 'sex_id', 'venom')]
df <- unique(df[, c('location_id', 'region_name', 'parent_id', 'nid', 'code_system', 'source_name', 'sample_size', 'year_id', 'age_group_id', 'sex_id', 'venom', 'original')]) # Aggregates over ICD codes

df[, venom_total := sum(original), by = c('location_id', 'age_group_id', 'sex_id', 'venom')]
df[, venom_total_loc_age := sum(original), by = c('location_id', 'age_group_id', 'venom')]
df[, venom_total_loc := sum(original), by = c('location_id', 'venom')]
df[, venom_total_region := sum(original), by = c('region_name', 'venom')]
df[, venom_total_parent := sum(original), by = c('parent_id', 'venom')]

demographic_ref <- unique(df[, c('location_id', 'age_group_id', 'sex_id')]) # To make sure I get them all through the different stages
props_all <- unique(df[, c('location_id', 'age_group_id', 'sex_id', 'venom', 'venom_total')])
props_no_sex <- unique(df[, c('location_id', 'age_group_id', 'venom', 'venom_total_loc_age')])
props_no_sex_no_age <- unique(df[, c('location_id', 'venom', 'venom_total_loc')])
props_region <- unique(df[, c('region_name', 'venom', 'venom_total_region')])
props_parent <- unique(df[, c('parent_id', 'venom', 'venom_total_parent')])


props <- dcast(props_all, location_id + age_group_id + sex_id ~ venom, value.var = 'venom_total')
props[is.na(props)] <- 0
props[, denominator := bees + other + scorpion + snake + spider]
props[, bees_rate := bees/denominator]
props[, other_rate := other/denominator]
props[, scorpion_rate := scorpion/denominator]
props[, snake_rate := snake/denominator]
props[, spider_rate := spider/denominator]
props[is.na(props)] <- 0

props_no_sex <- dcast(props_no_sex, location_id + age_group_id ~ venom, value.var = 'venom_total_loc_age')
props_no_sex[is.na(props_no_sex)] <- 0
props_no_sex[, denominator := bees + other + scorpion + snake + spider]
props_no_sex[, bees_rate := bees/denominator]
props_no_sex[, other_rate := other/denominator]
props_no_sex[, scorpion_rate := scorpion/denominator]
props_no_sex[, snake_rate := snake/denominator]
props_no_sex[, spider_rate := spider/denominator]
props_no_sex[is.na(props_no_sex)] <- 0

props_loc_only <- dcast(props_no_sex_no_age, location_id ~ venom, value.var = 'venom_total_loc')
props_loc_only[is.na(props_loc_only)] <- 0
props_loc_only[, denominator := bees + other + scorpion + snake + spider]
props_loc_only[, bees_rate := bees/denominator]
props_loc_only[, other_rate := other/denominator]
props_loc_only[, scorpion_rate := scorpion/denominator]
props_loc_only[, snake_rate := snake/denominator]
props_loc_only[, spider_rate := spider/denominator]
props_loc_only[is.na(props_loc_only)] <- 0

props_region_only <- dcast(props_region, region_name ~ venom, value.var = 'venom_total_region')
props_region_only[is.na(props_region_only)] <- 0
props_region_only[, denominator := bees + other + scorpion + snake + spider]
props_region_only[, bees_rate := bees/denominator]
props_region_only[, other_rate := other/denominator]
props_region_only[, scorpion_rate := scorpion/denominator]
props_region_only[, snake_rate := snake/denominator]
props_region_only[, spider_rate := spider/denominator]
props_region_only[is.na(props_region_only)] <- 0

props_parent_only <- dcast(props_parent, parent_id ~ venom, value.var = 'venom_total_parent')
props_parent_only[is.na(props_parent_only)] <- 0
props_parent_only[, denominator := bees + other + scorpion + snake + spider]
props_parent_only[, bees_rate := bees/denominator]
props_parent_only[, other_rate := other/denominator]
props_parent_only[, scorpion_rate := scorpion/denominator]
props_parent_only[, snake_rate := snake/denominator]
props_parent_only[, spider_rate := spider/denominator]
props_parent_only[is.na(props_parent_only)] <- 0


# Now go through, merge on proportions to redistribute by and do it
redistribute_dems[, redistribute_row := 1]
first <- merge(df, redistribute_dems, by = c('location_id', 'age_group_id', 'sex_id', 'year_id', 'nid', 'code_system', 'source_name'))
non_redist <- merge(df, redistribute_dems, by = c('location_id', 'age_group_id', 'sex_id', 'year_id', 'nid', 'code_system', 'source_name'), all.x = TRUE)
non_redist <- non_redist[is.na(redistribute_row)] # Goal is for final dataframe to look like this
  
  
  
first <- merge(first[, c('location_id', 'nid', 'code_system', 'source_name', 'sample_size', 'venom',
                      'year_id', 'age_group_id', 'sex_id', 'original')],
               props[, c('location_id', 'age_group_id', 'sex_id', 'snake_rate', 'scorpion_rate', 'spider_rate', 'bees_rate', 'other_rate', 'denominator')],
               by = c('location_id', 'age_group_id', 'sex_id'))

first_wide <- dcast(first, location_id + age_group_id + sex_id + nid + code_system + source_name + sample_size +
                      year_id + snake_rate + scorpion_rate + spider_rate + bees_rate + other_rate + denominator ~ venom, value.var = 'original')

first_wide[is.na(first_wide)] <- 0


# These can be re-distributed in the first round
redist <- first_wide[redistribute != 0 & denominator != 0 & redistribute < denominator]
redist[, check := (bees_rate + spider_rate + scorpion_rate + snake_rate + other_rate)]
check <- sum(redist$bees) + sum(redist$other) + sum(redist$scorpion) + sum(redist$snake) + sum(redist$spider) + sum(redist$redistribute)

# and redistribute based on rates
redist[, bees_dist := bees_rate*redistribute]
redist[, other_dist := other_rate*redistribute]
redist[, scorpion_dist := scorpion_rate*redistribute]
redist[, snake_dist := snake_rate*redistribute]
redist[, spider_dist := spider_rate*redistribute]

redist[, bees_real := bees + bees_dist]
redist[, other_real := other + other_dist]
redist[, scorpion_real := scorpion + scorpion_dist]
redist[, snake_real := snake + snake_dist]
redist[, spider_real := spider + spider_dist]
check1 <- sum(redist$bees_real) + sum(redist$other_real) + sum(redist$scorpion_real) + sum(redist$snake_real) + 
  sum(redist$spider_real)
stopifnot(check1 == check)



# These ones only have deaths under bad ICD codes, and need redistribution from a more aggregated spot
bad_ones <- first_wide[denominator == 0 | redistribute >= denominator]
zeros <- bad_ones[redistribute == 0 & denominator == 0]
bad_ones <- bad_ones[!(redistribute == 0 & denominator == 0)]

#Merge on rates without sexes
bad1 <- merge(bad_ones[, c('location_id', 'age_group_id', 'sex_id', 'year_id',
                           'nid', 'code_system', 'source_name', 'sample_size',
                           'redistribute', 'bees', 'other', 'scorpion', 'snake', 'spider')],
              props_no_sex[, c('location_id', 'age_group_id', 'bees_rate', 'other_rate', 'scorpion_rate', 'snake_rate', 'spider_rate', 'denominator')])
bad <- bad1[denominator > redistribute]
worse <- bad1[denominator <= redistribute] # To have location only merged on

check <- sum(bad$bees) + sum(bad$other) + sum(bad$scorpion) + sum(bad$snake) + sum(bad$spider) + sum(bad$redistribute)


bad[, bees_dist := bees_rate*redistribute]
bad[, other_dist := other_rate*redistribute]
bad[, scorpion_dist := scorpion_rate*redistribute]
bad[, snake_dist := snake_rate*redistribute]
bad[, spider_dist := spider_rate*redistribute]

bad[, bees_real := bees + bees_dist]
bad[, other_real := other + other_dist]
bad[, scorpion_real := scorpion + scorpion_dist]
bad[, snake_real := snake + snake_dist]
bad[, spider_real := spider + spider_dist]

check1 <- sum(bad$bees_real) + sum(bad$other_real) + sum(bad$scorpion_real) + sum(bad$snake_real) + 
  sum(bad$spider_real)
stopifnot(check == check1)

# Now location only rates
worse1 <- merge(worse[, c('location_id', 'age_group_id', 'sex_id','year_id', 'nid', 'code_system', 'source_name', 'sample_size',
                          'redistribute', 'bees', 'other', 'scorpion', 'snake', 'spider')],
              props_loc_only[, c('location_id', 'bees_rate', 'other_rate', 'scorpion_rate', 'snake_rate', 'spider_rate', 'denominator')])


worse2 <- worse1[redistribute <= denominator]
check <- sum(worse2$bees) + sum(worse2$other) + sum(worse2$scorpion) + sum(worse2$snake) + sum(worse2$spider) + sum(worse2$redistribute)

worse2[, bees_dist := bees_rate*redistribute]
worse2[, other_dist := other_rate*redistribute]
worse2[, scorpion_dist := scorpion_rate*redistribute]
worse2[, snake_dist := snake_rate*redistribute]
worse2[, spider_dist := spider_rate*redistribute]

worse2[, bees_real := bees + bees_dist]
worse2[, other_real := other + other_dist]
worse2[, scorpion_real := scorpion + scorpion_dist]
worse2[, snake_real := snake + snake_dist]
worse2[, spider_real := spider + spider_dist]

check1 <- sum(worse2$bees_real) + sum(worse2$other_real) + sum(worse2$scorpion_real) + sum(worse2$snake_real) + 
  sum(worse2$spider_real)
stopifnot(check == check1)


# These need regional or parent patterns. All have a denominator of zero and only have redistribute ICD codes
zero <- worse1[redistribute > denominator]
zero <- merge(zero, locs[, c('location_id', 'region_name', 'parent_id', 'level')], by = 'location_id')
zero[parent_id == 6, parent_id := 44533] # Hack CHina's to line up correctly for redistribution

sub <- zero[level > 3]
region <- zero[level == 3]

# Do region first
region <- merge(region[, c('location_id', 'age_group_id', 'sex_id','year_id', 'nid', 'code_system', 'source_name', 'sample_size',
                           'region_name', 'redistribute', 'bees', 'other', 'scorpion', 'snake', 'spider')],
                props_region_only[, c('region_name', 'bees_rate', 'other_rate', 'scorpion_rate', 'snake_rate', 'spider_rate', 'denominator')],
                by = c('region_name'))

check <- sum(region$bees) + sum(region$other) + sum(region$scorpion) + sum(region$snake) + sum(region$spider) + sum(region$redistribute)


region[, bees_dist := bees_rate*redistribute]
region[, other_dist := other_rate*redistribute]
region[, scorpion_dist := scorpion_rate*redistribute]
region[, snake_dist := snake_rate*redistribute]
region[, spider_dist := spider_rate*redistribute]

region[, bees_real := bees + bees_dist]
region[, other_real := other + other_dist]
region[, scorpion_real := scorpion + scorpion_dist]
region[, snake_real := snake + snake_dist]
region[, spider_real := spider + spider_dist]

check1 <- sum(region$bees_real) + sum(region$other_real) + sum(region$scorpion_real) + sum(region$snake_real) + 
  sum(region$spider_real)
stopifnot(check == check1)


# Now subnationals by their parent pattern
sub <- merge(sub[, c('location_id', 'age_group_id', 'sex_id','year_id', 'nid', 'code_system', 'source_name', 'sample_size',
                     'parent_id', 'redistribute', 'bees', 'other', 'scorpion', 'snake', 'spider')],
                props_parent_only[, c('parent_id', 'bees_rate', 'other_rate', 'scorpion_rate', 'snake_rate', 'spider_rate', 'denominator')],
                by = c('parent_id'))

check <- sum(sub$bees) + sum(sub$other) + sum(sub$scorpion) + sum(sub$snake) + sum(sub$spider) + sum(sub$redistribute)

sub[, bees_dist := bees_rate*redistribute]
sub[, other_dist := other_rate*redistribute]
sub[, scorpion_dist := scorpion_rate*redistribute]
sub[, snake_dist := snake_rate*redistribute]
sub[, spider_dist := spider_rate*redistribute]

sub[, bees_real := bees + bees_dist]
sub[, other_real := other + other_dist]
sub[, scorpion_real := scorpion + scorpion_dist]
sub[, snake_real := snake + snake_dist]
sub[, spider_real := spider + spider_dist]

check1 <- sum(sub$bees_real) + sum(sub$other_real) + sum(sub$scorpion_real) + sum(sub$snake_real) + 
  sum(sub$spider_real)
stopifnot(check == check1)

# Now put back together
all_df <- rbind(redist, bad, worse2, sub, region, fill = TRUE)
all_df[, bees := bees_real][, other := other_real][, scorpion := scorpion_real][, snake := snake_real][, spider := spider_real]

all_df <- all_df[, c('location_id', 'age_group_id', 'sex_id', 'nid', 'code_system', 'source_name', 'sample_size', 'year_id',
                     'bees', 'other', 'scorpion', 'snake', 'spider')]
all_df_long <- melt(all_df, id.vars = c('location_id', 'age_group_id', 'sex_id', 'nid', 'code_system', 'source_name', 'sample_size', 'year_id'),
                    value.vars = c('snake', 'spider', 'scorpion', 'bees', 'other'), value.name = 'deaths', 
                    variable.name = 'venom')

# Put together, start withones that didn't need to be redistributed
final <- copy(non_redist)
final <- final[, c('location_id', 'age_group_id', 'sex_id', 'year_id', 'nid','code_system', 'source_name', 'sample_size', 
                   'venom', 'original')]
setnames(final, 'original', 'deaths')

final_write <- rbind(all_df_long, final, fill = T)
final_write <- final_write[venom != 'redistribute']

zeros <- final_write[deaths == 0]

# Line up zero's to what it was before
zero_dems_real <- df[original == 0, c('location_id', 'age_group_id', 'sex_id', 'year_id', 'venom')] %>% unique() %>% .[, old := 1]
zeros <- merge(zeros, zero_dems_real, by = c('location_id', 'age_group_id', 'year_id', 'sex_id', 'venom'), all.x = TRUE)
zeros <- zeros[old == 1]
zeros[, old := NULL]

final_write <- final_write[deaths != 0]
final_write <- rbind(final_write, zeros)

write_csv(final_write, '[DIRECTORY]/garbage_code_redistributed_snake.csv')



